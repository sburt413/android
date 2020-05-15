package com.aluminumrain.android.file

import java.io.{File, InputStream}

import scalaz.\/

sealed trait VirtualFileSystem {
  def read(path: VirtualPath): Option[VirtualFile]

  def write(path: VirtualPath, data: InputStream): String \/ VirtualFile
}

object LocalFileSystem extends VirtualFileSystem {
  override def read(path: VirtualPath): Option[VirtualFile] = {
    Option(new File(path.raw)).filter(_.canRead).map(LocalFile.apply);
  }

  override def write(path: VirtualPath, data: InputStream): String \/ VirtualFile = ???
}