package com.hydrangea.music.script

import java.io.{BufferedReader, InputStreamReader}

import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.music.library.DeviceLibraryService
import com.hydrangea.music.script.ScriptHelpers.findDevice
import com.hydrangea.process.DefaultCLIProcessFactory
import org.apache.commons.lang3.RandomStringUtils
import org.rogach.scallop.{ScallopConf, ScallopOption}

object DropDeviceIndex extends App {
  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
  }

  val cliArgs = new Args(args)
  cliArgs.verify()

  val adbService: ADBService = ADBService(DefaultCLIProcessFactory.instance)
  val device: Device = cliArgs.device.map(findDevice).getOrElse(adbService.firstDevice)

  private val nonce: String = RandomStringUtils.randomAlphabetic(4)
  System.out.print(s"Enter the given token to delete the index for device ${device.serial}: $nonce \n> ")

  private val stdin = new BufferedReader(new InputStreamReader(System.in))
  private val input: String = stdin.readLine()
  if (input.equals(nonce)) {
    DeviceLibraryService(DefaultCLIProcessFactory.instance).dropIndex(device)
  } else {
    System.err.println(s"Input did not match token: $input != $nonce")
  }
}
