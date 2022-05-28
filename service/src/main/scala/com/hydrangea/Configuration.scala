package com.hydrangea

import com.hydrangea.android.Device
import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, AndroidLocation, LocalFileLocation}
import com.typesafe.config.{Config, ConfigFactory}

import java.nio.file.Path

trait Configuration {

  /**
    * Returns where programs should store its metadata about music.
    *
    * @return where programs should store its metadata about music
    */
  def repositoryDataDirectory: Path

  def localRepositoryLocation: LocalFileLocation

  def androidRepositoryLocation: AndroidLocation
}

object TypeSafeConfiguration extends Configuration {
  private val config = ConfigFactory.load("application.conf")

  private val dataConfig: Config = config.getConfig("com.hydrangea.data")
  val repositoryDataDirectory: Path = Path.of(dataConfig.getString("repository-directory"))

  private val repositoryConfig: Config = config.getConfig("com.hydrangea.repository")
  val repositoryDirectory: Path = Path.of(repositoryConfig.getString("local.directory"))
  val localRepositoryLocation: LocalFileLocation =
    LocalFileLocation(repositoryDirectory).getOrElse(
      throw new IllegalArgumentException(s"$repositoryDirectory is not a valid path"))
  val deviceSerial: String = repositoryConfig.getString("device.serial")
  val deviceRepositoryPath: AbsolutePath = repositoryConfig.getString("device.path").toUnixPath
  def androidRepositoryLocation: AndroidLocation = AndroidLocation(Device(deviceSerial), deviceRepositoryPath)
}

case class ConfigurationValue(override val repositoryDataDirectory: Path,
                              override val localRepositoryLocation: LocalFileLocation,
                              override val androidRepositoryLocation: AndroidLocation)
    extends Configuration
