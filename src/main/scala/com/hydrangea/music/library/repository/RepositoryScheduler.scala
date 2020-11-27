package com.hydrangea.music.library.repository

import java.nio.file.Files

import com.hydrangea.file.{AbsolutePath, FilePath, LocalRegularFileData}
import com.hydrangea.music.library.record.Scheduler

import scala.jdk.StreamConverters._

class RepositoryScheduler(repository: Repository) extends Scheduler[LocalRegularFileData] {

  import com.hydrangea.file.FileData._

  override def scan(path: AbsolutePath): Seq[LocalRegularFileData] =
    Files
      .walk(path.toJavaPath)
      .toScala(LazyList)
      .filter(path => path.toString.endsWith(FilePath.mp3Extension))
      .flatMap(_.toLocalRegularFileData)
}

object RepositoryScheduler {
  def apply(repository: Repository): RepositoryScheduler = new RepositoryScheduler(repository)
}
