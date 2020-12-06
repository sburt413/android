package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.music.library.DeviceLibraryService
import com.hydrangea.music.script.ScriptHelpers._
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.rogach.scallop.{ScallopConf, ScallopOption}

/**
  * Updates the most recent update times
  */
object ScanDevice extends App {
  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule)

  val adbService: ADBService = injector.instance[ADBService]

  val cliArgs = new Args(args.toSeq)
  cliArgs.verify()
  val device: Device =
    cliArgs.device.map(findDevice).getOrElse(adbService.firstDevice)

  val deviceLibraryService: DeviceLibraryService = injector.instance[DeviceLibraryService]
  deviceLibraryService.scanDevice(device)
}
