package com.hydrangea.music.script

import java.time.Instant

import com.hydrangea.android.adb.{ADB, Device}
import com.hydrangea.android.file.{AndroidFile, AndroidRegularFile, VirtualFile}
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.library.index.{DeviceIndexRecord, DeviceIndexRecordService, IndexService, LastIndexedRecord}
import com.hydrangea.music.script.ScriptHelpers.findDevice
import com.hydrangea.music.tagger.TikaAndroidTagger
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.Logger

object UpdateDeviceIndex extends App {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(UpdateDeviceIndex.getClass)

  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
    val folderCount = opt[Int]("count", 'c', required = true)
  }

  val cliArgs = new Args(args)
  cliArgs.verify()

  val device: Device =
    cliArgs.device.map(findDevice).getOrElse(ADB.firstDevice)

  val folderCount: Int =
    cliArgs.folderCount.getOrElse(throw new IllegalArgumentException("A maximum folder count must be specified"))

  val recordsToIndex: List[LastIndexedRecord] =
    DeviceIndexRecordService
      .getIndexRecordForDevice(device)
      .getOrElse(throw new IllegalArgumentException(s"No index record exists for device (${device.serial})"))
      .needsUpdating
      .take(folderCount)

  device.withCommandLine() { commandLine =>
    logger.info(
      s"Preparing to index:\n${recordsToIndex.map(record => record.directoryPath + " / " + record.lastIndexed).mkString("\n")}")
    val updatedRecords: List[LastIndexedRecord] =
      recordsToIndex
        .map(record => {
          val index: DeviceIndexRecord =
            DeviceIndexRecordService
              .getIndexRecordForDevice(device)
              .getOrElse(throw new IllegalStateException(s"Somehow index was deleted for ${device.serial}"))

          val now: Instant = Instant.now
          logger.info(s"Updating index for ${record.directoryPath}")

          val mp3Files: Seq[AndroidFile] = commandLine
            .listRecursive(record.directoryPath)
            .filter(VirtualFile.mp3Filter)

          val mp3FilesNeedingIndexing: Seq[AndroidRegularFile] =
            mp3Files
              .filter(file => record.needsIndexing(file))
              .flatMap(_.to[AndroidRegularFile])

          logger.debug(
            s"Found ${mp3FilesNeedingIndexing.size} mp3 files to index out of $mp3Files in ${record.directoryPath}")

          val trackRecords: Seq[TrackRecord] = mp3FilesNeedingIndexing.map(TikaAndroidTagger.tag(commandLine, _))
          logger.debug(s"Writing track records:\n${trackRecords.mkString("\n")}")
          trackRecords.foreach(IndexService.put(device, _, forceOverwrite = true))

          val updated: LastIndexedRecord = record.updateLastIndexed(now)
          DeviceIndexRecordService.writeIndex(device, index.updateRecord(updated))
          updated
        })

    logger.info(s"Finished updating index with ${updatedRecords.size} updated records.")
    logger.debug(updatedRecords.mkString("\n"))
  }
}
