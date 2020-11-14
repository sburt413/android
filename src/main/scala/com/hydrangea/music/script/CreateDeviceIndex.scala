package com.hydrangea.music.script

import com.hydrangea.android.adb.{ADB, Device}
import com.hydrangea.music.library.DeviceLibraryService
import com.hydrangea.music.script.ScriptHelpers._
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.Logger

object CreateDeviceIndex extends App {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(CreateDeviceIndex.getClass)

  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  val cliArgs = new Args(args)
  cliArgs.verify()
  val device: Device = cliArgs.device.map(findDevice).getOrElse(ADB.firstDevice)

  DeviceLibraryService.createDeviceIndex(device)
}
