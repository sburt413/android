package com.hydrangea.repository

import argonaut.{DecodeJson, EncodeJson}
import com.google.inject.AbstractModule
import com.hydrangea.file.{AndroidLocation, FileLocation, LocalFileLocation}
import net.codingwell.scalaguice.ScalaModule

import java.nio.file.{Files, Path}
import java.util.Objects
import javax.inject.Inject
import scala.jdk.StreamConverters._

class MusicRepositoryService @Inject()(configuration: MusicRepositoryConfiguration, repositoryDao: MusicRepositoryDAO) {
  def loadRepository[L <: FileLocation: EncodeJson: DecodeJson](fileLocation: L): Option[MusicRepository[L]] =
    repositoryDao.load[L](repositoryFile(fileLocation))

  def writeRepository[L <: FileLocation: EncodeJson: DecodeJson](repository: MusicRepository[L]): Unit =
    repositoryDao.persist(repository, repositoryFile(repository))

  def loadLocalRepositories: Seq[MusicRepository[LocalFileLocation]] =
    Files
      .walk(localLocationFolder)
      .toScala(LazyList)
      .flatMap(file => repositoryDao.load(file))

  def loadAndroidRepositories: Seq[MusicRepository[AndroidLocation]] =
    Files
      .walk(androidLocationFolder)
      .toScala(LazyList)
      .flatMap(file => repositoryDao.load(file))

  def loadAllRepositories: Seq[MusicRepository[_ <: FileLocation]] =
    loadLocalRepositories ++ loadAndroidRepositories

  private def localLocationFolder: Path =
    configuration.repositoryDataDirectory.resolve("local")

  private def androidLocationFolder: Path =
    configuration.repositoryDataDirectory.resolve("android")

  private def repositoryFile[L <: FileLocation](musicRepository: MusicRepository[L]): Path =
    repositoryFile(musicRepository.root)

  private def repositoryFile(repositoryLocation: FileLocation): Path = {
    val hash: Int = hashLocation(repositoryLocation)
    val (folder, indexName) =
      repositoryLocation match {
        case LocalFileLocation(path)       => (localLocationFolder, s"local-${path.last}-$hash")
        case AndroidLocation(device, path) => (androidLocationFolder, s"android-${device.serial}-${path.last}-$hash")
      }

    folder.resolve(s"$indexName.json")
  }

  private def hashLocation[L <: FileLocation](location: L): Int =
    location match {
      case LocalFileLocation(path)       => Objects.hash("local", path)
      case AndroidLocation(device, path) => Objects.hash(device.serial, path)
    }
}

case class MusicRepositoryConfiguration(repositoryDataDirectory: Path)

case class MusicRepositoryServiceConfigurationModule(configuration: MusicRepositoryConfiguration)
    extends AbstractModule
    with ScalaModule {

  override def configure(): Unit = {
    bind[MusicRepositoryConfiguration].toInstance(configuration)
  }
}
