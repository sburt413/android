package com.hydrangea.file

import java.nio.file.Path

import com.hydrangea.android.adb.Device

/**
  * A data structure containing enough information to retrieve a file.
  */
sealed trait FileLocation {
  type thisType >: this.type <: FileLocation

  def path: AbsolutePath

  def ++(relativePath: RelativePath): thisType
}

case class LocalFileLocation(path: AbsolutePath) extends FileLocation {
  override type thisType = LocalFileLocation

  override def ++(relativePath: RelativePath): LocalFileLocation =
    LocalFileLocation(path ++ relativePath)

  def toJavaPath: Path = Path.of(path.raw)
}

case class AndroidLocation(device: Device, path: AbsolutePath) extends FileLocation {
  override type thisType = AndroidLocation

  override def ++(relativePath: RelativePath): AndroidLocation =
    AndroidLocation(device, path ++ relativePath)
}
