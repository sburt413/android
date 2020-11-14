package com.hydrangea.music.script

import com.hydrangea.Configuration
import com.hydrangea.android.file.{WindowsDirectory, WindowsFile}
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.RepositoryLibraryService.RepositorySchedule
import com.hydrangea.music.library.repository.Repository
import org.rogach.scallop.{ScallopConf, ScallopOption}

object TagAndIndexRepository extends App {
  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
    val fileCount = opt[Int]("count", 'c', required = true)
  }

  val cliArgs = new Args(args)
  cliArgs.verify()

  val directory =
    WindowsFile
      .of(Configuration.repositoryDirectory)
      .to[WindowsDirectory]
      .getOrElse(throw new IllegalStateException(
        s"Repository directory (${Configuration.repositoryDirectory}) is not a Windows directory."))
  val repository: Repository = Repository(directory)

  val fileCount: Int =
    cliArgs.fileCount.getOrElse(throw new IllegalArgumentException("A maximum file count must be specified"))

  val schedule: RepositorySchedule =
    RepositoryLibraryService
      .scheduleSynchronization(repository, fileCount)
      .getOrElse(throw new IllegalStateException("No index for repository."))

  RepositoryLibraryService.synchronizeElasticsearchIndex(repository, schedule)
}
