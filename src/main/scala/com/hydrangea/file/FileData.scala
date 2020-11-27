package com.hydrangea.file

import java.nio.file.{Files, Path}
import java.time.Instant

import scala.reflect.ClassTag

sealed trait FileData {
  def location: FileLocation
  def modifyTime: Instant
}

object FileData {
  import FilePath._

  def extensionFilter(extension: String): FileData => Boolean =
    file => FilePath.extensionFilter(extension)(file.location.path)

  val mp3Filter: FileData => Boolean = extensionFilter(".mp3")

  implicit class PathOps(javaPath: Path) {
    def toLocalRegularFileData: Option[LocalRegularFileData] = {
      val (path, modifiedTime) = getData(javaPath)
      if (Files.isRegularFile(javaPath)) {
        Some(LocalRegularFileData(LocalFileLocation(path), modifiedTime))
      } else {
        None
      }
    }

    def toLocalDirectoryData: Option[LocalDirectoryData] = {
      val (path, modifiedTime) = getData(javaPath)
      if (Files.isDirectory(javaPath)) {
        Some(LocalDirectoryData(LocalFileLocation(path), modifiedTime))
      } else {
        None
      }
    }

    def toLocalFileData: Option[LocalFileData] =
      if (Files.isDirectory(javaPath)) {
        javaPath.toLocalDirectoryData
      } else {
        javaPath.toLocalRegularFileData
      }

    LocalRegularFileData
  }

  private def getData(javaPath: Path): (AbsolutePath, Instant) = {
    val path: AbsolutePath = javaPath.asAbsolutePath
    val modifyTime: Instant = Files.getLastModifiedTime(javaPath).toInstant

    (path, modifyTime)
  }
}

sealed trait LocalFileData extends FileData {
  def location: LocalFileLocation

  def toJavaPath: Path = location.toJavaPath

  def to[A <: LocalFileData: ClassTag]: Option[A] =
    this match {
      case f: A => Some(f)
      case _    => None
    }
}

sealed trait AndroidFileData extends FileData {
  def location: AndroidLocation

  def to[A <: AndroidFileData: ClassTag]: Option[A] =
    this match {
      case f: A => Some(f)
      case _    => None
    }
}

sealed trait RegularFileData extends FileData

sealed trait DirectoryFileData extends FileData

case class LocalRegularFileData(location: LocalFileLocation, modifyTime: Instant)
    extends LocalFileData
    with RegularFileData
case class LocalDirectoryData(location: LocalFileLocation, modifyTime: Instant)
    extends LocalFileData
    with DirectoryFileData
case class AndroidRegularFileData(location: AndroidLocation, modifyTime: Instant)
    extends AndroidFileData
    with RegularFileData
case class AndroidDirectoryData(location: AndroidLocation, modifyTime: Instant)
    extends AndroidFileData
    with DirectoryFileData
