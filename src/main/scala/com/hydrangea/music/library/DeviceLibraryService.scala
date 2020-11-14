package com.hydrangea.music.library

import java.time.Instant

import com.hydrangea.Configuration
import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.VirtualPath._
import com.hydrangea.android.file.{AndroidDirectory, AndroidPath, AndroidRegularFile, VirtualPath}
import com.hydrangea.music.library.device._
import com.hydrangea.music.library.record.IndexRecord.DeviceIndexRecord
import com.hydrangea.music.library.record._
import org.slf4j.Logger

/**
  * A service for managing an index of music for devices.  This service will allow for partial indexing and
  * synchronization of music files on a device.
  */
object DeviceLibraryService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(DeviceLibraryService.getClass)

  type DeviceSchedule = Schedule[AndroidPath, AndroidRegularFile]

  def indexName(device: Device): IndexName = IndexName(device.serial.toLowerCase)

  def createDeviceIndex(device: Device): IndexRecord[AndroidPath] = {
    val indexRecord: DeviceIndexRecord = createIndexRecord(device)
    DeviceIndexRecordService.writeRecord(indexName(device), indexRecord)
    logger.debug(s"Creating elasticsearch index for ${device.serial}")
    IndexService.createIndex(indexName(device))
    logger.info(s"Created elasticsearch index for ${device.serial}: ${indexName(device)}")

    indexRecord
  }

  private def createIndexRecord(device: Device): DeviceIndexRecord =
    device.withCommandLine() { commandLine =>
      val musicDirectoryPath: AndroidPath = Configuration.deviceMusicDirectory.toAndroidPath
      val musicDirectory: AndroidDirectory =
        commandLine
          .stat(musicDirectoryPath)
          .getOrElse(throw new IllegalArgumentException(s"Music directory not found: $musicDirectoryPath"))
          .to[AndroidDirectory]
          .getOrElse(throw new IllegalArgumentException(s"Music file is not a directory: $musicDirectoryPath"))

      val artistFolders: List[RecordCandidate[AndroidPath]] =
        commandLine
          .list(musicDirectoryPath)
          .flatMap(_.to[AndroidDirectory])
          .map(dir => {
            val fileCount: Int = commandLine.findRegularFiles(dir.path).count(VirtualPath.mp3Filter)
            val lastModify: Instant = commandLine.mostRecentUpdate(dir.path).getOrElse(dir.modifyTime)
            RecordCandidate(dir.path, fileCount, lastModify)
          })
          .toList

      logger.info(s"Writing record index for directory $musicDirectory with ${artistFolders.length} entries.")
      val indexRecord: DeviceIndexRecord = IndexRecord.create(musicDirectory.path, artistFolders)
      logger.info(s"New index record is: $indexRecord")
      indexRecord
    }

  def scanDevice(device: Device): DeviceIndexRecord = {
    val record: DeviceIndexRecord =
      DeviceIndexRecordService
        .getRecord(indexName(device))
        .getOrElse(throw new IllegalArgumentException(s"No index record exists for device (${device.serial})"))

    device.withCommandLine() { commandLine =>
      val musicDirectoryPath: AndroidPath = Configuration.deviceMusicDirectory.toAndroidPath
      val musicDirectory: AndroidDirectory =
        commandLine
          .stat(musicDirectoryPath)
          .getOrElse(throw new IllegalArgumentException(s"Music directory not found: $musicDirectoryPath"))
          .to[AndroidDirectory]
          .getOrElse(throw new IllegalArgumentException(s"Music file is not a directory: $musicDirectoryPath"))

      val artistFolders: List[RecordCandidate[AndroidPath]] =
        commandLine
          .list(musicDirectory.path)
          .flatMap(_.to[AndroidDirectory])
          .map(dir => {
            val fileCount: Int = commandLine.findRegularFiles(dir.path).count(VirtualPath.mp3Filter)
            val lastModify: Instant = commandLine.mostRecentUpdate(dir.path).getOrElse(dir.modifyTime)
            RecordCandidate(dir.path, fileCount, lastModify)
          })
          .toList

      val (updatedRecord, deletedRecords) = record.reindex(artistFolders)
      IndexService.remove(indexName(device), deletedRecords.map(_.directoryPath))
      DeviceIndexRecordService.writeRecord(indexName(device), updatedRecord)

      updatedRecord
    }
  }

  def getRecordsToSynchronize(device: Device): Option[List[LastIndexedRecord[AndroidPath]]] =
    DeviceIndexRecordService.getRecord(indexName(device)).map(record => record.needsUpdating)

  def scheduleSynchronization(device: Device, desiredFileCount: Int): Option[DeviceSchedule] =
    device.withCommandLine() { commandLine =>
      DeviceIndexRecordService
        .getRecord(indexName(device))
        .map(record => {
          DeviceScheduler(device).schedule(desiredFileCount, record)
        })
    }

  def synchronizeElasticsearchIndex(device: Device, schedule: DeviceSchedule): Unit = {
    val indexRecord: IndexRecord[AndroidPath] =
      DeviceIndexRecordService
        .getRecord(indexName(device))
        .getOrElse(throw new IllegalStateException(s"No index record for repository: ${device.serial}"))

    DeviceSynchronizationJob.run(device, schedule, indexRecord, indexName(device))
  }

  //  def runSynchronization(device: Device, schedule: DeviceSchedule): Unit =
//    device.withCommandLine() { commandLine =>
//      logger.info(s"Processing scheduled files: ${schedule.queued
//        .flatMap(entry => Seq(s"Record: ${entry.record}") ++ entry.filePaths.map(_.path.raw))
//        .mkString("\n")}")
//
//      val index: DeviceIndexRecord =
//        DeviceIndexRecordService
//          .getRecord(indexName(device))
//          .getOrElse(throw new IllegalStateException(s"Somehow index was deleted for ${device.serial}"))
//
//      val progressReport: ScheduleProgressReport[AndroidPath] = ScheduleProgressReport.start(schedule)
//      val now: Instant = Instant.now
//      val finalIndex: DeviceIndexRecord =
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
//            val updatedIndex: DeviceIndexRecord = currentIndex.updateRecord(updatedRecord)
//            DeviceIndexRecordService.writeRecord(indexName(device), updatedIndex)
//            updatedIndex
//        })
//
//      logger.info(s"Final index is: $finalIndex")
//    }

  def dropIndex(device: Device): Unit = {
    logger.info(s"Deleting index for ${device.serial}.")
    IndexService.dropIndex(indexName(device))
    DeviceIndexRecordService.deleteRecord(indexName(device))
  }
}
