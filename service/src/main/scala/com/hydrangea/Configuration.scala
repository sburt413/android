package com.hydrangea

import java.nio.file.Path

import com.google.inject.AbstractModule
import com.typesafe.config.{Config, ConfigFactory}
import net.codingwell.scalaguice.ScalaModule

trait Configuration {

  /**
    * Returns where programs should store its metadata about music.
    *
    * @return where programs should store its metadata about music
    */
  def repositoryDataDirectory: Path
}

object TypeSafeConfiguration extends Configuration {
  private val config = ConfigFactory.load("application.conf")

  private val dataConfig: Config = config.getConfig("com.hydrangea.data")
  val repositoryDataDirectory: Path = Path.of(dataConfig.getString("repository-directory"))

  // TODO: These should be source and destination and be a Location, not a path
  private val deviceConfig: Config = config.getConfig("com.hydrangea.device")
  val deviceMusicDirectory: String = deviceConfig.getString("music-directory")

  private val repositoryConfig: Config = config.getConfig("com.hydrangea.repository")
  val repositoryDirectory: Path = Path.of(repositoryConfig.getString("repository-directory"))
}

case class ConfigurationValue(override val repositoryDataDirectory: Path) extends Configuration

case class ConfigurationModule(configuration: Configuration) extends AbstractModule with ScalaModule {
  override def configure(): Unit = {
    bind[Configuration].toInstance(configuration)
  }
}

object TypeSafeConfigurationModule extends ConfigurationModule(TypeSafeConfiguration)
