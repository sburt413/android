package com.hydrangea.android.file

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.time.Instant

import scala.language.implicitConversions
import scala.reflect.ClassTag

sealed trait VirtualFile {
  def path: VirtualPath

  def modifyTime: Instant

  /**
    * Downcasts this [[VirtualFile]] if it is the given type.
    *
    * @param classTag
    * @tparam A the type of [[VirtualFile]] this is expected to be
    * @return [[Some]] [[VirtualFile]] of type A; otherwise [[None]]
    */
  def to[A <: VirtualFile](implicit classTag: ClassTag[A]): Option[A] =
    this match {
      case f: A => Some(f)
      case _    => None
    }

  def castOrThrow[A <: VirtualFile](implicit classTag: ClassTag[A]): A =
    to[A].getOrElse(throw new IllegalStateException(s"$this is not expected type of ${classTag.runtimeClass.getName}"))

  override def toString: String = s"VirtualFile[${this.getClass.getSimpleName}](${path.raw}, $modifyTime)"
}

object VirtualFile extends VirtualFileCodecs {
  def extensionFilter(extension: String): VirtualFile => Boolean =
    file => VirtualPath.extensionFilter(extension)(file.path)

  val mp3Filter: VirtualFile => Boolean = extensionFilter(".mp3")
}

// Remote as in 'on the android device'
sealed trait AndroidFile extends VirtualFile with AndroidFileCodecs {
  def path: AndroidPath
}

object AndroidFile {
  def apply(path: AndroidPath, lastModified: Instant): AndroidFile = {
    if (path.isDirectoryPath) {
      AndroidDirectory(path, lastModified)
    } else {
      AndroidRegularFile(path, lastModified)
    }
  }
}

case class AndroidDirectory(path: AndroidPath, modifyTime: Instant) extends AndroidFile

object AndroidDirectory extends AndroidDirectoryCodecs

case class AndroidRegularFile(path: AndroidPath, modifyTime: Instant) extends AndroidFile

object AndroidRegularFile extends AndroidRegularFileCodecs

// Local as in 'from the developer's system'
sealed abstract class WindowsFile(javaPath: Path) extends VirtualFile {
  val path: WindowsPath = WindowsPath(javaPath.toString)
  val modifyTime: Instant = Files.getLastModifiedTime(javaPath).toInstant

  def toJavaFile: File = new File(path.raw)
  def toPath: Path = Paths.get(path.raw)
}

object WindowsFile extends WindowsFileCodecs {
  def of(path: Path): WindowsFile =
    if (Files.isDirectory(path)) {
      WindowsDirectory(path)
    } else {
      WindowsRegularFile(path)
    }

  def of(f: File): WindowsFile = of(f.toPath)

  implicit class PathOps(javaPath: Path) {
    def toLocalDirectory: Option[WindowsDirectory] =
      WindowsFile.of(javaPath) match {
        case localDirectory: WindowsDirectory => Some(localDirectory)
        case _                                => None
      }

    def toLocalRegularFile: Option[WindowsRegularFile] =
      WindowsFile.of(javaPath) match {
        case localRegularFile: WindowsRegularFile => Some(localRegularFile)
        case _                                    => None
      }
  }

  implicit class FileOps(javaFile: File) {
    def toLocalDirectory: Option[WindowsDirectory] =
      WindowsFile.of(javaFile) match {
        case localDirectory: WindowsDirectory => Some(localDirectory)
        case _                                => None
      }

    def toLocalRegularFile: Option[WindowsRegularFile] =
      WindowsFile.of(javaFile) match {
        case localRegularFile: WindowsRegularFile => Some(localRegularFile)
        case _                                    => None
      }
  }

  implicit def wrapFile(file: File): VirtualFile = WindowsFile.of(file)
  implicit def wrapFile(path: Path): VirtualFile = WindowsFile.of(path)
}

case class WindowsDirectory(javaPath: Path) extends WindowsFile(javaPath)

object WindowsDirectory extends WindowsDirectoryCodecs

case class WindowsRegularFile(javaPath: Path) extends WindowsFile(javaPath)

object WindowsRegularFile extends WindowsRegularFileCodecs
