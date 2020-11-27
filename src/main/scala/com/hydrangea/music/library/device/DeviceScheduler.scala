package com.hydrangea.music.library.device

import com.hydrangea.android.adb.Device
import com.hydrangea.file.{AbsolutePath, AndroidRegularFileData, FilePath}
import com.hydrangea.music.library.record.Scheduler

class DeviceScheduler(device: Device) extends Scheduler[AndroidRegularFileData] {
  override def scan(path: AbsolutePath): Seq[AndroidRegularFileData] =
    device.withCommandLine() { commandLine =>
      commandLine
        .listRecursive(path)
        .filter(fileData => FilePath.mp3Filter(fileData.location.path))
        .flatMap(_.to[AndroidRegularFileData])
    }
}

object DeviceScheduler {
  def apply(device: Device): DeviceScheduler = new DeviceScheduler(device)
}
