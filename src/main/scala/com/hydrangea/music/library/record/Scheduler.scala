package com.hydrangea.music.library.record

import com.hydrangea.android.file.{VirtualPath, VirtualRegularFile}
import org.apache.commons.lang3.time.DurationFormatUtils
import org.slf4j.Logger

import scala.annotation.tailrec
import scala.collection.mutable

abstract class Scheduler[P <: VirtualPath, F <: VirtualRegularFile] {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(getClass)

  /**
    * Returns all MP3 files found under the given {{path}}.
    *
    * @param path the path potentially containing MP3 files
    * @return the found files
    */
  def scan(path: P): Seq[F]

  /**
    * Creates a schedule for the next batch of records to index.  This grabs the next available records from the given
    * [[IndexRecord]] and determines the number of the files from that record to index.  The scheduler grabs file until:
    * <p>The specified number of files have been found (within a margin of error)
    * <p>All remaining records have been scheduled
    * <p>A maximum number of records have been skipped
    *
    * @param fileCount   the desired number of files to process in this batch
    * @param indexRecord the index to process
    * @return a [[Schedule]] of the next records to process
    */
  def schedule(fileCount: Int, indexRecord: IndexRecord[P]): Schedule[P, F] = {
    val marginOfError = Math.max(5, fileCount / 10)
    take(marginOfError)(fileCount, Schedule.empty[P, F], indexRecord.needsUpdating)
  }

  @tailrec
  private def take(marginOfError: Int)(remainingFileCount: Int,
                                       schedule: Schedule[P, F],
                                       remainingRecords: List[LastIndexedRecord[P]]): Schedule[P, F] = {
    val noFilesLeft: Boolean = remainingFileCount - marginOfError <= 0
    val nothingLeftToIndex: Boolean = remainingRecords.isEmpty
    if (noFilesLeft || nothingLeftToIndex) {
      schedule.remaining(remainingRecords)
    } else {
      val next: LastIndexedRecord[P] = remainingRecords.head

      val mp3Files: Seq[F] = scan(next.directoryPath)

      val mp3FilesNeedingIndexing: List[F] =
        mp3Files
          .filter(file => next.needsIndexing(file))
          .toList

      val (updatedSchedule, fileCountTaken) =
        if (mp3FilesNeedingIndexing.size < remainingFileCount + marginOfError) {
          logger.info(
            s"Found ${mp3FilesNeedingIndexing.size} mp3 files to index out of ${mp3Files.size} in ${next.directoryPath}")
          (schedule.queue(next, mp3FilesNeedingIndexing), mp3FilesNeedingIndexing.size)
        } else {
          logger.info(
            s"Skipping ${next.directoryPath}, ${mp3FilesNeedingIndexing.size} mp3 files is greater than our remaining file count ($remainingFileCount).")
          (schedule.skip(next), 0)
        }

      take(marginOfError)(remainingFileCount - fileCountTaken, updatedSchedule, remainingRecords.tail)
    }
  }
}

case class ScheduleEntry[P <: VirtualPath, F <: VirtualRegularFile](record: LastIndexedRecord[P], filePaths: List[F])

case class Schedule[P <: VirtualPath, F <: VirtualRegularFile](queued: List[ScheduleEntry[P, F]],
                                                               skipped: List[LastIndexedRecord[P]],
                                                               remaining: List[LastIndexedRecord[P]]) {
  def isEmpty: Boolean = queued.isEmpty && skipped.isEmpty && remaining.isEmpty

  def queue(record: LastIndexedRecord[P], files: List[F]): Schedule[P, F] =
    copy(queued = queued :+ ScheduleEntry(record, files))

  def skip(record: LastIndexedRecord[P]): Schedule[P, F] = copy(skipped = skipped :+ record)

  def remaining(records: List[LastIndexedRecord[P]]): Schedule[P, F] = copy(remaining = remaining ++ records)
}

object Schedule {
  def empty[P <: VirtualPath, F <: VirtualRegularFile] = Schedule[P, F](Nil, Nil, Nil)
}

private[library] class ScheduleProgressReport[P <: VirtualPath](recordFileCounts: Map[P, Int],
                                                                complete: mutable.Map[P, Int] =
                                                                  mutable.Map.empty[P, Int]) {
  val totalFiles = recordFileCounts.values.sum
  val start = System.currentTimeMillis()

  def completeFile(forRecord: LastIndexedRecord[P]): Unit =
    complete.put(forRecord.directoryPath, complete.getOrElse(forRecord.directoryPath, 0) + 1)

  def recordStatus(forRecord: LastIndexedRecord[P]): String =
    format(complete.getOrElse(forRecord.directoryPath, 0), recordFileCounts(forRecord.directoryPath))

  def overallComplete: Int = complete.values.sum

  def overallStatus(): String =
    format(overallComplete, totalFiles)

  private def elapsedTime(): Long = System.currentTimeMillis() - start

  def elapsed(): String = formatTime(elapsedTime())

  private def estimatedRemainingTime(): Option[Long] = {
    if (overallComplete >= 5) {
      val perFile: Double = elapsedTime().toDouble / overallComplete
      // Adding an arbitrary 100 mills to make this a slightly more worst case approximation
      val millsRemaining: Long = (perFile * (totalFiles - overallComplete)).toLong + 100
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

private[library] object ScheduleProgressReport {
  def start[P <: VirtualPath, F <: VirtualRegularFile](schedule: Schedule[P, F]): ScheduleProgressReport[P] = {
    val totals: Map[P, Int] =
      schedule.queued
        .map({
          case ScheduleEntry(record, files) => record.directoryPath -> files.size
        })
        .toMap

    new ScheduleProgressReport[P](totals)
  }
}
