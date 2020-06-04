package com.hydrangea.music.script

import com.hydrangea.android.adb.{ADB, Device}

object ScriptHelpers {
  def findDevice(serial: String): Device =
    ADB.devices
      .find(_.serial.equals(serial))
      .getOrElse(throw new IllegalArgumentException(s"No device for serial: $serial"))
}
