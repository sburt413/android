package com.hydrangea.music.library.device

import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.{AndroidPath, AndroidRegularFile, VirtualFile}
import com.hydrangea.music.library.record.Scheduler

class DeviceScheduler(device: Device) extends Scheduler[AndroidPath, AndroidRegularFile] {
  override def scan(path: AndroidPath): Seq[AndroidRegularFile] =
      device.withCommandLine() { commandLine =>
        commandLine
          .listRecursive(path)
          .filter(VirtualFile.mp3Filter)
          .flatMap(_.to[AndroidRegularFile])
      }
}

object DeviceScheduler {
  def apply(device: Device): DeviceScheduler = new DeviceScheduler(device)
}