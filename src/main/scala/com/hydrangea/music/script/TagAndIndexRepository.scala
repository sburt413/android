package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.Configuration
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.RepositoryLibraryService.RepositorySchedule
import com.hydrangea.music.library.repository.Repository
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.rogach.scallop.{ScallopConf, ScallopOption}

object TagAndIndexRepository extends App {
  import com.hydrangea.file.FileData._

  class Args(args: Seq[String]) extends ScallopConf(args) {
    val device: ScallopOption[String] = opt[String]("device", 'd')
    val fileCount = opt[Int]("count", 'c', required = true)
  }

  val cliArgs = new Args(args.toSeq)
  cliArgs.verify()

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule)

  val directory =
    Configuration.repositoryDirectory.toLocalDirectoryData
      .getOrElse(
        throw new IllegalStateException(
          s"Repository directory (${Configuration.repositoryDirectory}) is not a Windows directory."))
  val repository: Repository = Repository(directory)

  val fileCount: Int =
    cliArgs.fileCount.getOrElse(throw new IllegalArgumentException("A maximum file count must be specified"))

  val repositoryLibraryService: RepositoryLibraryService = injector.instance[RepositoryLibraryService]
  val schedule: RepositorySchedule =
    repositoryLibraryService
      .scheduleSynchronization(repository, fileCount)
      .getOrElse(throw new IllegalStateException("No index for repository."))

  repositoryLibraryService.synchronizeElasticsearchIndex(repository, schedule)
}
