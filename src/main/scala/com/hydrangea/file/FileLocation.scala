package com.hydrangea.file

import java.nio.file.Path

import com.hydrangea.android.adb.Device

/**
  * A data structure containing enough information to retrieve a file.
  *
  * @param path the path to find the file
  * @tparam P the type of path structure
  */
sealed abstract class FileLocation[P <: AbsolutePath](path: P)

sealed trait WindowsFileLocation[P <: WindowsPath2] extends FileLocation[P] {

  /**
    * Returns this file as a native JVM object.
    *
    * @return this file as a native JVM object
    */
  def javaPath: Path
}

case class LocalWindowsLocation(path: LocalWindowsPath) extends FileLocation[LocalWindowsPath](path)

case class WindowsNetworkLocation(path: WindowsNetworkPath) extends FileLocation[WindowsNetworkPath](path)

case class AndroidLocation(device: Device, path: UnixPath) extends FileLocation[UnixPath](path)
