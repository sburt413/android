package com.hydrangea.music.script

import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.process.DefaultCLIProcessFactory

object ScriptHelpers {
  def findDevice(serial: String): Device =
    ADBService(DefaultCLIProcessFactory.instance).devices
      .find(_.serial.equals(serial))
      .getOrElse(throw new IllegalArgumentException(s"No device for serial: $serial"))
}
