package com.hydrangea.music.library.repository

import java.nio.file.Files
import java.time.Instant

import com.hydrangea.file.FileData._
import com.hydrangea.file.{AbsolutePath, FileData}
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.track.TrackService
import com.hydrangea.process.CLIProcessFactory

import scala.jdk.StreamConverters._

class RepositoryService(trackService: TrackService) {
  def readRepository(path: AbsolutePath): List[TrackRecord] = {
    val tags: List[TrackRecord] =
      Files
        .walk(path.toJavaPath)
        .toScala(LazyList)
        .flatMap(_.toLocalRegularFileData)
        .filter(FileData.mp3Filter)
        .map(trackService.getLocalTrack)
        .map(TrackRecord(_, Instant.now()))
        .toList

    tags.foreach(tag => println(s"Tag is: $tag"))

    tags
  }
}

object RepositoryService {
  def apply(trackService: TrackService): RepositoryService =
    new RepositoryService(trackService)

  def apply(cliProcessFactory: CLIProcessFactory): RepositoryService =
    apply(TrackService(cliProcessFactory))
}
