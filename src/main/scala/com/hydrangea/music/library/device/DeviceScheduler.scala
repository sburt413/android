package com.hydrangea.music.library.device

import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.file.{AbsolutePath, AndroidRegularFileData, FilePath}
import com.hydrangea.music.library.record.Scheduler
import com.hydrangea.process.CLIProcessFactory

class DeviceScheduler(adbService: ADBService, device: Device) extends Scheduler[AndroidRegularFileData] {
  override def scan(path: AbsolutePath): Seq[AndroidRegularFileData] =
    adbService.withCommandLine(device) { commandLine =>
      commandLine
        .listRecursive(path)
        .filter(fileData => FilePath.mp3Filter(fileData.location.path))
        .flatMap(_.to[AndroidRegularFileData])
    }
}

object DeviceScheduler {
  def apply(adbService: ADBService, device: Device): DeviceScheduler =
    new DeviceScheduler(adbService, device)

  def apply(cliProcessFactory: CLIProcessFactory, device: Device): DeviceScheduler =
    apply(ADBService(cliProcessFactory), device)
}
