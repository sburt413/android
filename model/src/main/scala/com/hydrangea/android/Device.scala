package com.hydrangea.android

import argonaut.Argonaut._
import argonaut._

/**
  * An Android device capable of of communicating with the program via ADB.
  *
  * @param serial the serial number of the device
  */
case class Device(serial: String)

object Device {
  implicit val codec: CodecJson[Device] = casecodec1(Device.apply, Device.unapply)("serial")
}
