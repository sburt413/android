package com.hydrangea.music.script

import com.hydrangea.Configuration
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.repository.Repository
import com.hydrangea.process.DefaultCLIProcessFactory

/**
  * Updates the most recent update times
  */
object ScanRepository extends App {
  import com.hydrangea.file.FileData._

  val directory =
    Configuration.repositoryDirectory.toLocalDirectoryData
      .getOrElse(
        throw new IllegalStateException(
          s"Repository directory (${Configuration.repositoryDirectory}) is not a Windows directory."))
  val repository: Repository = Repository(directory)

  RepositoryLibraryService(DefaultCLIProcessFactory.instance).scanRepository(repository)
}
