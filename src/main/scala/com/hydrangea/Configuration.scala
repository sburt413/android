package com.hydrangea

import java.nio.file.Path

import com.typesafe.config.{Config, ConfigFactory}

object Configuration {
  private val config = ConfigFactory.load("application.conf")

  private val deviceConfig: Config = config.getConfig("com.hydrangea.device")
  val deviceMusicDirectory: String = deviceConfig.getString("music-directory")

  private val repositoryConfig: Config = config.getConfig("com.hydrangea.repository")
  val repositoryDirectory: Path = Path.of(repositoryConfig.getString("repository-directory"))

  private val recordConfig: Config = config.getConfig("com.hydrangea.music.library.record")
  val indexRecordDirectory: Path = Path.of(recordConfig.getString("index-record-directory"))
}
