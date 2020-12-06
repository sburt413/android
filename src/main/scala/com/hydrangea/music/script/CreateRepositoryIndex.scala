package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.Configuration
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.repository.Repository
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._

object CreateRepositoryIndex extends App {
  import com.hydrangea.file.FileData._
  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule)

  val directory =
    Configuration.repositoryDirectory.toLocalDirectoryData.getOrElse(throw new IllegalStateException("Not a directory"))
  val repository: Repository = Repository(directory)

  val repositoryLibraryService: RepositoryLibraryService = injector.instance[RepositoryLibraryService]
  repositoryLibraryService.createRepositoryIndex(repository)
}
