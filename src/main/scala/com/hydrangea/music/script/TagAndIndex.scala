package com.hydrangea.music.script

import java.time.{Duration, Instant}

import com.hydrangea.android.adb.{ADB, ADBCommandLine, Device}
import com.hydrangea.android.file.{AndroidFile, AndroidPath, AndroidRegularFile, VirtualFile, VirtualPath}
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.library.index.{DeviceIndexRecord, DeviceIndexRecordService, IndexService, LastIndexedRecord}
import com.hydrangea.music.script.ScriptHelpers.findDevice
import com.hydrangea.music.tagger.TikaAndroidTagger
import org.apache.commons.lang3.time.DurationFormatUtils
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.Logger

import scala.annotation.tailrec
import scala.collection.mutable

object TagAndIndex extends App {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(TagAndIndex.getClass)

  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
    val fileCount = opt[Int]("count", 'c', required = true)
  }

  val cliArgs = new Args(args)
  cliArgs.verify()

  val device: Device =
    cliArgs.device.map(findDevice).getOrElse(ADB.firstDevice)

  val fileCount: Int =
    cliArgs.fileCount.getOrElse(throw new IllegalArgumentException("A maximum file count must be specified"))

  device.withCommandLine() { commandLine =>
    val index: DeviceIndexRecord =
      DeviceIndexRecordService
        .getRecordForDevice(device)
        .getOrElse(throw new IllegalStateException(s"Somehow index was deleted for ${device.serial}"))
    val schedule: Schedule = Scheduler.schedule(fileCount, commandLine, index)

    logger.info(
      s"Processing scheduled files: ${schedule.queued.flatMap(entry => Seq(s"Record: ${entry._1}") ++ entry._2.map(_.path.raw)).mkString("\n")}")

    val progressReport: ScheduleProgressReport = ScheduleProgressReport.start(schedule)
    val now: Instant = Instant.now
    val finalIndex: DeviceIndexRecord =
      schedule.queued.reverse.foldRight(index)({
        case ((record, files), currentIndex) =>
          logger.info(s"Tagging ${files.size} files for record $record")
          val trackRecords: Seq[TrackRecord] =
            files.map(file => {
              val trackRecord: TrackRecord = TikaAndroidTagger.tag(commandLine, file)
              progressReport.completeFile(record)
              val remaining: String =
                progressReport.estimatedRemaining().map(rem => s" --- $rem remaining").getOrElse("")
              logger.info(
                s"Total: ${progressReport.overallStatus()} --- ${record.directoryPath.fileName}: ${progressReport
                  .recordStatus(record)} --- ${progressReport.elapsed()} elapsed" + remaining)
              trackRecord
            })

          logger.debug(s"Writing track records:\n${trackRecords.mkString("\n")}")
          IndexService.putAll(device, trackRecords, forceOverwrite = true)

          val updatedRecord: LastIndexedRecord = record.updateLastIndexed(now)
          val updatedIndex: DeviceIndexRecord = currentIndex.updateRecord(updatedRecord)
          DeviceIndexRecordService.writeRecord(device, updatedIndex)
          updatedIndex
      })

    logger.info(s"Final index is: $finalIndex")
  }
}

private object Scheduler {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(Scheduler.getClass)

  /**
    * Creates a schedule for the next batch of records to index.  This grabs the next available records from the given
    * [[DeviceIndexRecord]] and determines the number of the files from that record to index.  The scheduler grabs file until:
    * <p>The specified number of files have been found (within a margin of error)
    * <p>All remaining records have been scheduled
    * <p>A maximium number of records have been skipped
    *
    * @param fileCount         the desired number of files to process in this batch
    * @param commandLine       an ADBCommandLine to determine files for records
    * @param deviceIndexRecord the index to process
    * @return a [[Schedule]] of the next records to process
    */
  def schedule(fileCount: Int, commandLine: ADBCommandLine, deviceIndexRecord: DeviceIndexRecord): Schedule = {
    val marginOfError = Math.max(5, fileCount / 10)

    @tailrec
    def take(remainingFileCount: Int, schedule: Schedule, remainingRecords: List[LastIndexedRecord]): Schedule = {
      val noFilesLeft: Boolean = remainingFileCount - marginOfError <= 0
      val nothingLeftToIndex: Boolean = remainingRecords.isEmpty
      if (noFilesLeft || nothingLeftToIndex) {
        schedule.remaining(remainingRecords)
      } else {
        val next: LastIndexedRecord = remainingRecords.head

        val mp3Files: Seq[AndroidFile] =
          commandLine
            .listRecursive(next.directoryPath)
            .filter(VirtualFile.mp3Filter)

        val mp3FilesNeedingIndexing: List[AndroidRegularFile] =
          mp3Files
            .filter(file => next.needsIndexing(file))
            .flatMap(_.to[AndroidRegularFile])
            .toList

        val (updatedSchedule, fileCountTaken) =
          if (mp3FilesNeedingIndexing.size < remainingFileCount + marginOfError) {
            logger.info(
              s"Found ${mp3FilesNeedingIndexing.size} mp3 files to index out of ${mp3Files.size} in ${next.directoryPath}")
            (schedule.take(next, mp3FilesNeedingIndexing), mp3FilesNeedingIndexing.size)
          } else {
            logger.info(
              s"Skipping ${next.directoryPath}, ${mp3FilesNeedingIndexing.size} mp3 files is greater than our remaining file count ($remainingFileCount).")
            (schedule.skip(next), 0)
          }

        take(remainingFileCount - fileCountTaken, updatedSchedule, remainingRecords.tail)
      }
    }

    take(fileCount, Schedule(Nil, Nil, Nil), deviceIndexRecord.needsUpdating)
  }
}

private case class Schedule(queued: List[(LastIndexedRecord, List[AndroidRegularFile])],
                            skipped: List[LastIndexedRecord],
                            remaining: List[LastIndexedRecord]) {
  def take(record: LastIndexedRecord, files: List[AndroidRegularFile]): Schedule =
    copy(queued = queued :+ (record, files))

  def skip(record: LastIndexedRecord): Schedule = copy(skipped = skipped :+ record)

  def remaining(records: List[LastIndexedRecord]): Schedule = copy(remaining = remaining ++ records)
}

private class ScheduleProgressReport(recordFileCounts: Map[AndroidPath, Int],
                                     complete: mutable.Map[AndroidPath, Int] = mutable.Map.empty) {
  val totalFiles = recordFileCounts.values.sum
  val start = System.currentTimeMillis()

  def completeFile(forRecord: LastIndexedRecord): Unit =
    complete.put(forRecord.directoryPath, complete.getOrElse(forRecord.directoryPath, 0) + 1)

  def recordStatus(forRecord: LastIndexedRecord): String =
    format(complete.getOrElse(forRecord.directoryPath, 0), recordFileCounts(forRecord.directoryPath))

  def overallComplete: Int = complete.values.sum

  def overallStatus(): String =
    format(overallComplete, totalFiles)

  private def elapsedTime(): Long = System.currentTimeMillis() - start

  def elapsed(): String = formatTime(elapsedTime())

  private def estimatedRemainingTime(): Option[Long] = {
    if (overallComplete >= 5) {
      val perFile: Double = elapsedTime().toDouble / overallComplete
      // Adding an arbitrary 20 mills to make this a slightly more worst case approximation
      val millsRemaining: Long = (perFile * (totalFiles - overallComplete)).toLong + 20
      Some(millsRemaining)
    } else {
      None
    }
  }

  def estimatedRemaining(): Option[String] = estimatedRemainingTime().map(formatTime)

  def format(count: Int, total: Int): String = {
    val len = total.toString.length
    s"%${len}d".format(count) + s"/$total"
  }

  def formatTime(mills: Long): String =
    DurationFormatUtils.formatDuration(mills, "H:mm:ss", true)
}

private object ScheduleProgressReport {
  def start(schedule: Schedule): ScheduleProgressReport = {
    val totals: Map[AndroidPath, Int] =
      schedule.queued
        .map({
          case (record, files) => record.directoryPath -> files.size
        })
        .toMap

    new ScheduleProgressReport(totals)
  }
}
