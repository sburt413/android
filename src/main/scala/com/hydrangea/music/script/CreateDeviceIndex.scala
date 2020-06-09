package com.hydrangea.music.script

import java.time.Instant

import com.hydrangea.Configuration
import com.hydrangea.android.adb.{ADB, Device}
import com.hydrangea.android.file.{AndroidDirectory, AndroidPath, VirtualPath}
import com.hydrangea.music.library.index.{DeviceIndexRecord, DeviceIndexRecordService, IndexService, RecordCandidate}
import com.hydrangea.music.script.ScriptHelpers._
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.Logger
import com.hydrangea.android.file.VirtualPath._

object CreateDeviceIndex extends App {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(CreateDeviceIndex.getClass)

  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  def createIndexRecord(device: Device): DeviceIndexRecord =
    device.withCommandLine() { commandLine =>
      val musicDirectoryPath: AndroidPath = Configuration.musicDirectory.toAndroidPath
      val musicDirectory: AndroidDirectory =
        commandLine
          .stat(musicDirectoryPath)
          .getOrElse(throw new IllegalArgumentException(s"Music directory not found: $musicDirectoryPath"))
          .to[AndroidDirectory]
          .getOrElse(throw new IllegalArgumentException(s"Music file is not a directory: $musicDirectoryPath"))

      val artistFolders: List[RecordCandidate] =
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
      val indexRecord: DeviceIndexRecord = DeviceIndexRecord.create(musicDirectory, artistFolders)
      logger.info(s"New index record is: $indexRecord")
      indexRecord
    }

  val cliArgs = new Args(args)
  cliArgs.verify()
  val device: Device = cliArgs.device.map(findDevice).getOrElse(ADB.firstDevice)

  val indexRecord: DeviceIndexRecord = createIndexRecord(device)
  DeviceIndexRecordService.writeRecord(device, indexRecord)
  logger.info(s"Creating elasticsearch index for ${device.serial}")
  IndexService.createIndex(device)
}
