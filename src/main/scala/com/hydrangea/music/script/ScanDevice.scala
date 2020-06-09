package com.hydrangea.music.script

import java.time.Instant

import com.hydrangea.Configuration
import com.hydrangea.android.adb.{ADB, Device}
import com.hydrangea.android.file.VirtualPath._
import com.hydrangea.android.file.{AndroidDirectory, AndroidPath, VirtualFile, VirtualPath}
import com.hydrangea.music.library.index.{DeviceIndexRecord, DeviceIndexRecordService, IndexService, RecordCandidate}
import com.hydrangea.music.script.ScriptHelpers._
import org.rogach.scallop.{ScallopConf, ScallopOption}

/**
  * Updates the most recent update times
  */
object ScanDevice extends App {
  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  val cliArgs = new Args(args)
  cliArgs.verify()
  val device: Device =
    cliArgs.device.map(findDevice).getOrElse(ADB.firstDevice)

  val index: DeviceIndexRecord =
    DeviceIndexRecordService
      .getRecordForDevice(device)
      .getOrElse(throw new IllegalArgumentException(s"No index record exists for device (${device.serial})"))

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

    val (updatedIndex, deletedRecords) = index.reindex(artistFolders)
    IndexService.remove(device, deletedRecords.map(_.directoryPath))
    DeviceIndexRecordService.writeRecord(device, updatedIndex)
  }
}
