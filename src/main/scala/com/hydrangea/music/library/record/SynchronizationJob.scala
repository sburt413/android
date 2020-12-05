package com.hydrangea.music.library.record

import java.time.Instant

import com.hydrangea.file.RegularFileData
import com.hydrangea.music.library.{IndexName, IndexService, TrackRecord}
import com.hydrangea.music.track.{Track, TrackService}
import com.hydrangea.process.CLIProcessFactory
import org.slf4j.Logger

/**
  * A job that tags files based on a given schedule and writes records out to elasticsearch.  The job then updates the
  * index record accordingly.
  */
class SynchronizationJob(trackService: TrackService) {
  import SynchronizationJob._;

  /**
    * Updates and persists the index record for the completed files.
    *
    * @param record        the index record for the updated index
    * @param updatedRecord the record of the file directory that was updated
    * @return the updated [[IndexRecord]]
    */
  private def updateAndWriteIndex(indexName: IndexName,
                                  record: IndexRecord,
                                  updatedRecord: LastIndexedRecord): IndexRecord = {
    val updatedIndex: IndexRecord = record.updateRecord(updatedRecord)
    IndexRecordService.writeRecord(indexName, updatedIndex)
    updatedIndex
  }

  /**
    * Runs the scheduled job on the records in the given [[Schedule]], updating the index with the given name.  The
    * given [[IndexRecord]] will be updated and persisted accordingly.
    *
    * @param schedule    the schedule to run
    * @param indexRecord the [[IndexRecord]] containing the current state of the index
    * @param indexName   the name of the elasticsearch index in question
    *
    * @tparam F the type of file
    */
  def run[F <: RegularFileData](schedule: Schedule[F], indexRecord: IndexRecord, indexName: IndexName): Unit = {
    logger.info(s"Processing scheduled files: ${schedule.queued
      .flatMap(entry => Seq(s"Record: ${entry.record}") ++ entry.fileData.map(_.location.path.raw))
      .mkString("\n")}")

    val progressReport: ScheduleProgressReport = ScheduleProgressReport.start(schedule)

    val now: Instant = Instant.now
    val finalIndex: IndexRecord =
      schedule.queued.reverse.foldRight(indexRecord)({
        case (ScheduleEntry(record, fileData), currentIndex) =>
          logger.info(s"Tagging ${fileData.size} files for record $record")
          val trackRecords: Seq[TrackRecord] =
            fileData.map(file => {
              val track: Track = trackService.readTrack(file)
              val trackRecord = TrackRecord(track, Instant.now())
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

          val updatedRecord: LastIndexedRecord = record.updateLastIndexed(now)
          updateAndWriteIndex(indexName, currentIndex, updatedRecord)
      })

    logger.info(s"Final index is: $finalIndex")
  }
}

object SynchronizationJob {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  def apply(trackService: TrackService): SynchronizationJob =
    new SynchronizationJob(trackService)

  def apply(cliProcessFactory: CLIProcessFactory): SynchronizationJob =
    apply(TrackService(cliProcessFactory))
}
