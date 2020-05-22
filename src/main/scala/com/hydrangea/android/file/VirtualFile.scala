package com.hydrangea.android.file

import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes
import java.time.Instant

sealed trait VirtualFile {
  def path: VirtualPath

  def modifyTime: Instant

  override def toString: String = s"VirtualFile[${this.getClass.getSimpleName}](${path.raw}, $modifyTime)"
}

// Remote as in 'on the android device'
sealed trait RemoteFile extends VirtualFile

case class AndroidDirectory(path: AndroidPath, modifyTime: Instant) extends RemoteFile

case class AndroidFile(path: AndroidPath, modifyTime: Instant) extends RemoteFile

// Local as in 'from the developer's system'
sealed trait LocalFile extends VirtualFile {
  def toJavaFile: File = new File(path.raw)
}

case class LocalFileEntry(file: File, attributes: BasicFileAttributes) extends LocalFile {
  override def path: WindowsPath = WindowsPath(file.getAbsolutePath)

  override def modifyTime: Instant = attributes.lastModifiedTime().toInstant
}

object LocalFileEntry {
  def apply(f: File): LocalFileEntry = {
    val attr: BasicFileAttributes = Files.readAttributes(f.toPath, classOf[BasicFileAttributes])
    this(f, attr)
  }
}

case class LocalDirectory(file: File, attributes: BasicFileAttributes) extends LocalFile {
  override def path: WindowsPath = WindowsPath(file.getAbsolutePath)

  override def modifyTime: Instant = attributes.lastModifiedTime().toInstant
}

object LocalDirectory {
  def apply(f: File): LocalDirectory = {
    val attr: BasicFileAttributes = Files.readAttributes(f.toPath, classOf[BasicFileAttributes])
    this(f, attr)
  }
}
