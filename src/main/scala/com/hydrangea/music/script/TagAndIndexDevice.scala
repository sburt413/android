package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.music.library.DeviceLibraryService
import com.hydrangea.music.library.DeviceLibraryService.DeviceSchedule
import com.hydrangea.music.script.ScriptHelpers.findDevice
import com.hydrangea.process.{DefaultCLIProcessFactory, DefaultCLIProcessFactoryModule}
import net.codingwell.scalaguice.InjectorExtensions._
import org.rogach.scallop.{ScallopConf, ScallopOption}

object TagAndIndexDevice extends App {
  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
    val fileCount = opt[Int]("count", 'c', required = true)
  }

  val cliArgs = new Args(args.toSeq)
  cliArgs.verify()

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule)

  val device: Device =
    cliArgs.device.map(findDevice).getOrElse(new ADBService(DefaultCLIProcessFactory.instance).firstDevice)

  val fileCount: Int =
    cliArgs.fileCount.getOrElse(throw new IllegalArgumentException("A maximum file count must be specified"))

  val deviceLibraryService: DeviceLibraryService = injector.instance[DeviceLibraryService]
  val schedule: DeviceSchedule =
    deviceLibraryService
      .scheduleSynchronization(device, fileCount)
      .getOrElse(throw new IllegalStateException("No index for device."))

  deviceLibraryService.synchronizeElasticsearchIndex(device, schedule)
}
