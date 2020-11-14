package com.hydrangea.android.file

import java.io.File
import java.nio.file.{Path, Paths}

import argonaut.Argonaut._
import org.apache.commons.exec.util.StringUtils

import scala.language.implicitConversions

sealed trait VirtualPath {
  import VirtualPath._

  type thisType >: this.type <: VirtualPath

  def pathSeparator: Char

  def parent(implicit builder: PathBuilder[thisType]): thisType =
    builder.build(raw.substring(0, raw.lastIndexOf(pathSeparator)))

  def raw: String

  //  adb -d shell ls -pla --full-time '/storage/0123-4567/Music/Metallica/S&M'
//  def quoted: String = '"' + raw + '"'

  // This goes through maniacal parsing in ProcessImpl#createCommandLine
//  def encoded: String = "\"" + raw.replace(" ", "\\ ").replace("!", "\\!") + "\""
//  def quoted: String = raw.replace(" ", "\\ ")
//  def escaped: String = '"' + raw.replace(" ", "\\ ") + '"'
  def quoted: String = "\"" + raw + "\""
//  def quoted: String = '"' + raw.replace(" ", "\\ ") + '"'
  def commandLine: String =
    if (raw.contains("'")) {
      StringUtils.quoteArgument(raw).escape(' ').escape('&').escape('\'').escape('`').escape('(').escape(')')
    } else {
      "\'" + raw + "\'"
    }
  def escaped: String = "\"" + raw.escape(' ').escape('&').escape('\'').escape('(').escape(')') + "\""

  def fileName: String = {
    if (raw.endsWith(pathSeparator.toString)) {
      val withoutTrailingSlash: JsonField = raw.substring(0, raw.length - 1)
      val split: Int = withoutTrailingSlash.lastIndexOf(pathSeparator)
      withoutTrailingSlash.substring(split + 1)
    } else {
      val split: Int = raw.lastIndexOf(pathSeparator)
      if (split > 0) {
        raw.substring(split + 1)
      } else {
        raw
      }
    }
  }

  def :+(childPath: String)(implicit builder: PathBuilder[thisType]): thisType = {
    val base: String =
      if (raw.endsWith("/")) {
        raw
      } else {
        raw + "/"
      }

    builder.build(base + childPath)
  }

  def ++[A <: VirtualPath](childPath: A)(implicit builder: PathBuilder[thisType],
                                         toThis: PathConverter[A, thisType],
                                         fromThis: PathConverter[thisType, A]): A = {
    if (VirtualPath.isCurrentDirectory(this)) {
      childPath
    } else if (VirtualPath.isParentDirectory(childPath)) {
      fromThis.convert(this)
    } else {
      val thisChildPath: thisType = toThis.convert(childPath)
      val base: String =
        if (raw.endsWith("/")) {
          raw
        } else {
          raw + "/"
        }

      val path: thisType = builder.build(base + thisChildPath.raw)
      fromThis.convert(path)
    }
  }

  def rebase[A <: VirtualPath](sourceBase: thisType, newBase: A)(implicit builder: PathBuilder[thisType],
                                                                 toThis: PathConverter[A, thisType],
                                                                 fromThis: PathConverter[thisType, A]): A = {
    if (!this.raw.startsWith(sourceBase.raw)) {
      throw new IllegalArgumentException(s"This ($raw) is not based off of source base ($sourceBase)")
    } else {
      val rebased: thisType = builder.build(toThis.convert(newBase).raw ++ this.raw.replace(sourceBase.raw, ""))
      fromThis.convert(rebased)
    }
  }

  def isDirectoryPath: Boolean = raw.endsWith(pathSeparator.toString)
}

case class AndroidPath(raw: String) extends VirtualPath {
  override type thisType = AndroidPath

  override val pathSeparator: Char = AndroidPath.pathSeparator

  def toWindows: WindowsPath =
    WindowsPath(raw.replace("/", "\\"))
}

object AndroidPath {
  val pathSeparator: Char = '/'
}

case class WindowsPath(raw: String) extends VirtualPath {
  override type thisType = WindowsPath

  override val pathSeparator: Char = WindowsPath.pathSeparator

  def toAndroid: AndroidPath =
    AndroidPath(raw.replace(pathSeparator, AndroidPath.pathSeparator))

  def toJavaFile: File = new File(raw)
  def toJavaPath: Path = Paths.get(raw)
}

object WindowsPath {
  def apply(javaPath: Path): WindowsPath = WindowsPath(javaPath.toAbsolutePath.toString)

  val pathSeparator: Char = '\\'
}

trait PathBuilder[A <: VirtualPath] {
  def build(raw: String): A
}

trait PathConverter[A <: VirtualPath, B <: VirtualPath] {
  def convert(sourcePath: A): B
}

object VirtualPath extends VirtualPathCodecs {
  def extensionFilter(extension: String): VirtualPath => Boolean =
    path => path.raw.endsWith(extension)

  val mp3Extension = ".mp3"
  val mp3Filter = extensionFilter(mp3Extension)

  def isCurrentDirectory(path: VirtualPath): Boolean =
    path match {
      case AndroidPath(raw) => raw.equals(".") || raw.equals("./")
      case WindowsPath(raw) => raw.equals(".") || raw.equals(".\\")
    }

  def isParentDirectory(path: VirtualPath): Boolean =
    path match {
      case AndroidPath(raw) => raw.equals("..") || raw.equals("../")
      case WindowsPath(raw) => raw.equals("..") || raw.equals("..\\")
    }

  implicit def convert(path: Path): WindowsPath = WindowsPath(path.toAbsolutePath.toString)

  implicit class StringPathOps(str: String) {
    def escape(ch: Char): String = str.replace("" + ch, s"\\$ch")
    def toAndroidPath: AndroidPath = AndroidPath(str)
    def toWindowsPath: WindowsPath = WindowsPath(str)
  }

  implicit val androidBuilder: PathBuilder[AndroidPath] = AndroidPath.apply

  implicit val androidToWindows: PathConverter[AndroidPath, WindowsPath] =
    androidPath => androidPath.toWindows

  implicit val androidToAndroid: PathConverter[AndroidPath, AndroidPath] =
    identity

  implicit val windowsBuilder: PathBuilder[WindowsPath] = WindowsPath.apply

  implicit val windowsToAndroid: PathConverter[WindowsPath, AndroidPath] =
    windowsPath => windowsPath.toAndroid

  implicit val windowsToWindows: PathConverter[WindowsPath, WindowsPath] =
    identity
}
