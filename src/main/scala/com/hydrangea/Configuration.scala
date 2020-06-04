package com.hydrangea

import com.typesafe.config.{Config, ConfigFactory}

object Configuration {
  private val config = ConfigFactory.load("application.conf")

  private val deviceConfig: Config = config.getConfig("com.hydrangea.device")
  val musicDirectory: String = deviceConfig.getString("music-directory")
}
