package com.hydrangea.music.library.record

import java.time.Instant

import com.hydrangea.android.file.{VirtualPath, VirtualRegularFile}
import com.hydrangea.music.library.{IndexName, IndexService, TrackRecord}
import org.slf4j.Logger

/**
  * A job that tags files based on a given schedule and writes records out to elasticsearch.  The job then updates the
  * index record accordingly.
  *
  * @tparam P the type of file path
  * @tparam F the type of file
  */
abstract class SynchronizationJob[P <: VirtualPath, F <: VirtualRegularFile] {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  /**
    * Reads and returns the MP3 tag information for the given file.
    *
    * @param filePath the path to the file to tag
    * @return the MP3 tag information for the given file
    */
  protected def tag(filePath: F): TrackRecord

  /**
    * Updates and persists the index record for the completed files.
    *
    * @param record        the index record for the updated index
    * @param updatedRecord the record of the file directory that was updated
    * @return the updated [[IndexRecord]]
    */
  protected def updateAndWriteIndex(record: IndexRecord[P], updatedRecord: LastIndexedRecord[P]): IndexRecord[P]

  /**
    * Runs the scheduled job on the records in the given [[Schedule]], updating the index with the given name.  The
    * given [[IndexRecord]] will be updated and persisted accordingly.
    *
    * @param schedule    the schedule to run
    * @param indexRecord the [[IndexRecord]] containing the current state of the index
    * @param indexName   the name of the elasticsearch index in question
    */
  def run(schedule: Schedule[P, F], indexRecord: IndexRecord[P], indexName: IndexName): Unit = {
    logger.info(s"Processing scheduled files: ${schedule.queued
      .flatMap(entry => Seq(s"Record: ${entry.record}") ++ entry.filePaths.map(_.path.raw))
      .mkString("\n")}")

    val progressReport: ScheduleProgressReport[P] = ScheduleProgressReport.start(schedule)

    val now: Instant = Instant.now
    val finalIndex: IndexRecord[P] =
      schedule.queued.reverse.foldRight(indexRecord)({
        case (ScheduleEntry(record, filePaths), currentIndex) =>
          logger.info(s"Tagging ${filePaths.size} files for record $record")
          val trackRecords: Seq[TrackRecord] =
            filePaths.map(file => {
              val trackRecord: TrackRecord = tag(file)
              progressReport.completeFile(record)
              val remaining: String =
                progressReport.estimatedRemaining().map(rem => s" --- $rem remaining").getOrElse("")
              logger.info(
                s"Total: ${progressReport.overallStatus()} --- ${record.directoryPath.fileName}: ${progressReport
                  .recordStatus(record)} --- ${progressReport.elapsed()} elapsed" + remaining)
              trackRecord
            })

          logger.debug(s"Writing track records to $indexName:\n${trackRecords.mkString("\n")}")
          IndexService.putAll(indexName, trackRecords, forceOverwrite = true)

          val updatedRecord: LastIndexedRecord[P] = record.updateLastIndexed(now)
          updateAndWriteIndex(currentIndex, updatedRecord)
      })

    logger.info(s"Final index is: $finalIndex")
  }
}
