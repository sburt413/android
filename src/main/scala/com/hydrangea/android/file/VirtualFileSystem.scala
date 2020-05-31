package com.hydrangea.android.file

import java.io.{File, InputStream}
import java.nio.file.{Files, Path}

import scalaz.\/

sealed trait VirtualFileSystem {
  def read(path: VirtualPath): Option[VirtualFile]

  def write(path: VirtualPath, data: InputStream): String \/ VirtualFile
}

object LocalFileSystem extends VirtualFileSystem {
  override def read(path: VirtualPath): Option[WindowsFile] =
    Option(new File(path.raw))
      .filter(_.canRead)
      .map(WindowsFile.of);

  override def write(path: VirtualPath, data: InputStream): String \/ VirtualFile = ???
}
