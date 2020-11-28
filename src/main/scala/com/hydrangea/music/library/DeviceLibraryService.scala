package com.hydrangea.music.library

import java.time.Instant

import com.hydrangea.Configuration
import com.hydrangea.android.adb.Device
import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, AndroidDirectoryData, AndroidFileData, AndroidRegularFileData, FilePath}
import com.hydrangea.music.library.device._
import com.hydrangea.music.library.record._
import org.slf4j.Logger

/**
  * A service for managing an index of music for devices.  This service will allow for partial indexing and
  * synchronization of music files on a device.
  */
object DeviceLibraryService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(DeviceLibraryService.getClass)

  type DeviceSchedule = Schedule[AndroidRegularFileData]

  def indexName(device: Device): IndexName = IndexName(device.serial.toLowerCase)

  def createDeviceIndex(device: Device): IndexRecord = {
    val indexRecord: IndexRecord = createIndexRecord(device)
    IndexRecordService.writeRecord(indexName(device), indexRecord)
    logger.debug(s"Creating elasticsearch index for ${device.serial}")
    IndexService.createIndex(indexName(device))
    logger.info(s"Created elasticsearch index for ${device.serial}: ${indexName(device)}")

    indexRecord
  }

  private def createIndexRecord(device: Device): IndexRecord =
    device.withCommandLine() { commandLine =>
      val musicDirectoryPath: AbsolutePath = Configuration.deviceMusicDirectory.toUnixPath
      val musicDirectory: AndroidFileData =
        commandLine
          .stat(musicDirectoryPath)
          .getOrElse(throw new IllegalArgumentException(s"Music directory not found: $musicDirectoryPath"))

      val artistFolders: List[RecordCandidate] =
        commandLine
          .list(musicDirectoryPath)
          .flatMap(_.to[AndroidDirectoryData])
          .map(dir => {
            val fileCount: Int = commandLine.findRegularFiles(dir.location.path).count(FilePath.mp3Filter)
            val lastModify: Instant = commandLine.mostRecentUpdate(dir.location.path).getOrElse(dir.modifyTime)
            RecordCandidate(dir.location.path, fileCount, lastModify)
          })
          .toList

      logger.info(s"Writing record index for directory $musicDirectory with ${artistFolders.length} entries.")
      val indexRecord: IndexRecord = IndexRecord.create(musicDirectory.location.path, artistFolders)
      logger.info(s"New index record is: $indexRecord")
      indexRecord
    }

  def scanDevice(device: Device): IndexRecord = {
    val record: IndexRecord =
      IndexRecordService
        .getRecord(indexName(device))
        .getOrElse(throw new IllegalArgumentException(s"No index record exists for device (${device.serial})"))

    device.withCommandLine() { commandLine =>
//      val musicDirectoryPath: AndroidPath = Configuration.deviceMusicDirectory.toAndroidPath
      val musicDirectoryPath = Configuration.deviceMusicDirectory.toUnixPath
      val musicDirectory: AndroidDirectoryData =
        commandLine
          .stat(musicDirectoryPath)
          .getOrElse(throw new IllegalArgumentException(s"Music directory not found: $musicDirectoryPath"))
          .to[AndroidDirectoryData]
          .getOrElse(throw new IllegalArgumentException(s"Music file is not a directory: $musicDirectoryPath"))

      val artistFolders: List[RecordCandidate] =
        commandLine
          .list(musicDirectory.location.path)
          .flatMap(_.to[AndroidDirectoryData])
          .map(dir => {
            val fileCount: Int = commandLine.findRegularFiles(dir.location.path).count(FilePath.mp3Filter)
            val lastModify: Instant = commandLine.mostRecentUpdate(dir.location.path).getOrElse(dir.modifyTime)
            RecordCandidate(dir.location.path, fileCount, lastModify)
          })
          .toList

      val (updatedRecord, deletedRecords) = record.reindex(artistFolders)
      IndexService.remove(indexName(device), deletedRecords.map(_.directoryPath))
      IndexRecordService.writeRecord(indexName(device), updatedRecord)

      updatedRecord
    }
  }

  def getRecordsToSynchronize(device: Device): Option[List[LastIndexedRecord]] =
    IndexRecordService.getRecord(indexName(device)).map(record => record.needsUpdating)

  def scheduleSynchronization(device: Device, desiredFileCount: Int): Option[DeviceSchedule] =
    device.withCommandLine() { commandLine =>
      IndexRecordService
        .getRecord(indexName(device))
        .map(record => {
          DeviceScheduler(device).schedule(desiredFileCount, record)
        })
    }

  def synchronizeElasticsearchIndex(device: Device, schedule: DeviceSchedule): Unit = {
    val indexRecord: IndexRecord =
      IndexRecordService
        .getRecord(indexName(device))
        .getOrElse(throw new IllegalStateException(s"No index record for repository: ${device.serial}"))

    SynchronizationJob.run(schedule, indexRecord, indexName(device))
  }

  //  def runSynchronization(device: Device, schedule: DeviceSchedule): Unit =
//    device.withCommandLine() { commandLine =>
//      logger.info(s"Processing scheduled files: ${schedule.queued
//        .flatMap(entry => Seq(s"Record: ${entry.record}") ++ entry.filePaths.map(_.path.raw))
//        .mkString("\n")}")
//
//      val index: IndexRecord =
//        DeviceIndexRecordService
//          .getRecord(indexName(device))
//          .getOrElse(throw new IllegalStateException(s"Somehow index was deleted for ${device.serial}"))
//
//      val progressReport: ScheduleProgressReport[AndroidPath] = ScheduleProgressReport.start(schedule)
//      val now: Instant = Instant.now
//      val finalIndex: IndexRecord =
//        schedule.queued.reverse.foldRight(index)({
//          case (ScheduleEntry(record, files), currentIndex) =>
//            logger.info(s"Tagging ${files.size} files for record $record")
//            val trackRecords: Seq[TrackRecord] =
//              files.map(file => {
//                val trackRecord: TrackRecord = TikaTagger.tag(commandLine, file)
//                progressReport.completeFile(record)
//                val remaining: String =
//                  progressReport.estimatedRemaining().map(rem => s" --- $rem remaining").getOrElse("")
//                logger.info(
//                  s"Total: ${progressReport.overallStatus()} --- ${record.directoryPath.fileName}: ${progressReport
//                    .recordStatus(record)} --- ${progressReport.elapsed()} elapsed" + remaining)
//                trackRecord
//              })
//
//            logger.debug(s"Writing track records:\n${trackRecords.mkString("\n")}")
//            IndexService.putAll(indexName(device), trackRecords, forceOverwrite = true)
//
//            val updatedRecord: LastIndexedRecord[AndroidPath] = record.updateLastIndexed(now)
//            val updatedIndex: IndexRecord = currentIndex.updateRecord(updatedRecord)
//            DeviceIndexRecordService.writeRecord(indexName(device), updatedIndex)
//            updatedIndex
//        })
//
//      logger.info(s"Final index is: $finalIndex")
//    }

  def dropIndex(device: Device): Unit = {
    logger.info(s"Deleting index for ${device.serial}.")
    IndexService.dropIndex(indexName(device))
    IndexRecordService.deleteRecord(indexName(device))
  }
}
