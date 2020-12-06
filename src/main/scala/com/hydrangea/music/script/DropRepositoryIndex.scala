package com.hydrangea.music.script

import java.io.{BufferedReader, InputStreamReader}

import com.google.inject.Guice
import com.hydrangea.Configuration
import com.hydrangea.music.library.RepositoryLibraryService
import com.hydrangea.music.library.repository.Repository
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.apache.commons.lang3.RandomStringUtils

object DropRepositoryIndex extends App {
  import com.hydrangea.file.FileData._

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule)

  val directory =
    Configuration.repositoryDirectory.toLocalDirectoryData.getOrElse(
      throw new IllegalStateException(
        s"Repository directory (${Configuration.repositoryDirectory}) is not a Windows directory."))
  val repository: Repository = Repository(directory)

  private val nonce: String = RandomStringUtils.randomAlphabetic(4)
  System.out.print(s"Enter the given token to delete the index for repository ${repository}: $nonce \n> ")

  private val stdin = new BufferedReader(new InputStreamReader(System.in))
  private val input: String = stdin.readLine()
  if (input.equals(nonce)) {
    val repositoryLibraryService: RepositoryLibraryService = injector.instance[RepositoryLibraryService]
    repositoryLibraryService.dropIndex(repository)
  } else {
    System.err.println(s"Input did not match token: $input != $nonce")
  }
}
