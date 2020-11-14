package com.hydrangea.music.library.repository

import java.nio.file.Files

import com.hydrangea.android.file.{VirtualPath, WindowsPath, WindowsRegularFile}
import com.hydrangea.music.library.record.Scheduler

import scala.jdk.StreamConverters._

class RepositoryScheduler(repository: Repository) extends Scheduler[WindowsPath, WindowsRegularFile] {
  override def scan(path: WindowsPath): Seq[WindowsRegularFile] =
    Files
      .walk(path.toJavaPath)
      .toScala(LazyList)
      .filter(path => path.toString.endsWith(VirtualPath.mp3Extension))
      .map(WindowsRegularFile.apply)
}

object RepositoryScheduler {
  def apply(repository: Repository): RepositoryScheduler = new RepositoryScheduler(repository)
}
