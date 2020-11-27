package com.hydrangea.music.script

import com.hydrangea.Configuration
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.repository.Repository

object CreateRepositoryIndex extends App {
  import com.hydrangea.file.FileData._
  val directory = Configuration.repositoryDirectory.toLocalDirectoryData
//      .getOrElse(throw new IllegalStateException(
//        s"Repository directory (${Configuration.repositoryDirectory}) is not a Windows directory."))
  val repository: Repository = Repository(directory)

  RepositoryLibraryService.createRepositoryIndex(repository)
}
