package com.hydrangea.android.file

import java.io.File
import java.nio.file.{Path, Paths}

import argonaut._
import Argonaut._
import enumeratum.EnumEntry
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

  // adb -s 98897a364a4d574549 exec-out base64 '/storage/0123-4567/Test/Addicted/01-02- Universe In a Ball!.mp3'
  // adb -s 98897a364a4d574549 exec-out base64 "/storage/0123-4567/Test/Addicted/01-02- Universe In a Ball"'!'".mp3" | head

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

  override def pathSeparator: Char = '/'

  def toWindows: WindowsPath =
    WindowsPath(raw.replace("/", "\\"))
}

object AndroidPath {}

case class WindowsPath(raw: String) extends VirtualPath {
  override type thisType = WindowsPath

  override def pathSeparator: Char = '\\'

  def toAndroid: AndroidPath =
    AndroidPath(raw.replace("\\", "/"))

  def toJavaFile: File = new File(raw)
  def toJavaPath: Path = Paths.get(raw)
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

  val mp3Filter = extensionFilter(".mp3")

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
