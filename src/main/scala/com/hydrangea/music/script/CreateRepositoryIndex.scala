package com.hydrangea.music.script

import com.hydrangea.Configuration
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.repository.Repository
import com.hydrangea.process.DefaultCLIProcessFactory

object CreateRepositoryIndex extends App {
  import com.hydrangea.file.FileData._
  val directory =
    Configuration.repositoryDirectory.toLocalDirectoryData.getOrElse(throw new IllegalStateException("Not a directory"))
  val repository: Repository = Repository(directory)

  RepositoryLibraryService(DefaultCLIProcessFactory.instance).createRepositoryIndex(repository)
}
