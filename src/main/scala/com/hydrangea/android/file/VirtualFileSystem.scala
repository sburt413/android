package com.hydrangea.android.file

import java.io.{File, InputStream}

import scalaz.\/

sealed trait VirtualFileSystem {
  def read(path: VirtualPath): Option[VirtualFile]

  def write(path: VirtualPath, data: InputStream): String \/ VirtualFile
}

object LocalFileSystem extends VirtualFileSystem {
  override def read(path: VirtualPath): Option[LocalFile] = {
    Option(new File(path.raw))
      .filter(_.canRead)
      .map(convert);
  }

  override def write(path: VirtualPath, data: InputStream): String \/ VirtualFile = ???

  private def convert(file: File): LocalFile =
    if (file.isDirectory) {
      LocalDirectory.apply(file)
    } else {
      LocalFileEntry.apply(file)
    }

  implicit def wrapFile(file: File): VirtualFile = convert(file)
}
