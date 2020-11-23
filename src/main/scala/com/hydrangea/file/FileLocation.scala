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

sealed abstract class WindowsFileLocation[P <: WindowsPath2](path: P) extends FileLocation[P](path) {

  /**
    * Returns this file as a native JVM object.
    *
    * @return this file as a native JVM object
    */
  def javaPath: Path = Path.of(path.raw)
}

case class LocalWindowsLocation(path: LocalWindowsPath) extends WindowsFileLocation[LocalWindowsPath](path)

case class WindowsNetworkLocation(path: WindowsNetworkPath) extends WindowsFileLocation[WindowsNetworkPath](path)

case class AndroidLocation(device: Device, path: UnixPath) extends FileLocation[UnixPath](path)
