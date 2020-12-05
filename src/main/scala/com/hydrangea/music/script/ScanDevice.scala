package com.hydrangea.music.script

import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.music.library.DeviceLibraryService
import com.hydrangea.music.script.ScriptHelpers._
import com.hydrangea.process.DefaultCLIProcessFactory
import org.rogach.scallop.{ScallopConf, ScallopOption}

/**
  * Updates the most recent update times
  */
object ScanDevice extends App {
  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  val adbService: ADBService = ADBService(DefaultCLIProcessFactory.instance)

  val cliArgs = new Args(args)
  cliArgs.verify()
  val device: Device =
    cliArgs.device.map(findDevice).getOrElse(adbService.firstDevice)

  DeviceLibraryService(DefaultCLIProcessFactory.instance).scanDevice(device)
}
