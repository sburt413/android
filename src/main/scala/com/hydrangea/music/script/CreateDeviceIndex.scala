package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.music.library.DeviceLibraryService
import com.hydrangea.music.script.ScriptHelpers._
import com.hydrangea.process.{DefaultCLIProcessFactory, DefaultCLIProcessFactoryModule}
import net.codingwell.scalaguice.InjectorExtensions._
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.slf4j.Logger

object CreateDeviceIndex extends App {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(CreateDeviceIndex.getClass)

  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule)
  val adbService = new ADBService(DefaultCLIProcessFactory.instance)

  val cliArgs = new Args(args.toSeq)
  cliArgs.verify()
  val device: Device = cliArgs.device.map(findDevice).getOrElse(adbService.firstDevice)

  val deviceLibraryService: DeviceLibraryService = injector.instance[DeviceLibraryService]
  deviceLibraryService.createDeviceIndex(device)
}
