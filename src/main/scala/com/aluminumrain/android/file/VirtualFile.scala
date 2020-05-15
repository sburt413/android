package com.aluminumrain.android.file

import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes
import java.time.Instant

sealed trait VirtualFile {
  def path: VirtualPath

  def modifyTime: Instant

  override def toString: String = s"VirtualFile[${this.getClass.getSimpleName}](${path.raw}, $modifyTime)"
}

case class Directory(path: VirtualPath, modifyTime: Instant) extends VirtualFile

case class AndroidFile(path: VirtualPath, modifyTime: Instant) extends VirtualFile

case class LocalFile(file: File, attributes: BasicFileAttributes) extends VirtualFile {
  override def path: VirtualPath = VirtualPath(file.getAbsolutePath)

  override def modifyTime: Instant = attributes.lastModifiedTime().toInstant
}

object LocalFile {
  def apply(f: File): LocalFile = {
    val attr: BasicFileAttributes = Files.readAttributes(f.toPath, classOf[BasicFileAttributes])
    this (f, attr)
  }
}

case class ADBFile(path: VirtualPath, createDate: Instant, updateDate: Instant)

case class VirtualPath(raw: String) {
  //  adb -d shell ls -pla --full-time '/storage/0123-4567/Music/Metallica/S\&M'
  def quoted: String = '"' + raw + '"'

  def ++(childPath: VirtualPath): VirtualPath = {
    if (VirtualPath.isCurrentDirectory(this)) {
      childPath
    } else if (VirtualPath.isParentDirectory(childPath)) {
      this
    } else {
      if (raw.endsWith("/")) {
        VirtualPath(raw + childPath.raw)
      } else {
        VirtualPath(raw + "/" + childPath.raw)
      }
    }
  }

  def ++(childPath: String): VirtualPath = {
    this ++ VirtualPath(childPath)
  }

  def isDirectoryPath: Boolean = raw.endsWith("/")
}

object VirtualPath {
  def isCurrentDirectory(path: VirtualPath): Boolean = path.raw.equals(".") || path.raw.equals("./")

  def isParentDirectory(path: VirtualPath): Boolean = path.raw.equals("..") || path.raw.equals("../")

  implicit class ToPathOps(str: String) {
    def toVirtualPath: VirtualPath = {
      VirtualPath(str)
    }
  }
}