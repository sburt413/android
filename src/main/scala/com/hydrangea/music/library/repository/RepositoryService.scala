package com.hydrangea.music.library.repository

import java.nio.file.Files

import com.hydrangea.android.file.{VirtualPath, WindowsPath}
import com.hydrangea.music.TrackRecord
import com.hydrangea.music.tagger.TikaTagger

import scala.jdk.StreamConverters._

object RepositoryService {
  def readRepository(path: WindowsPath): List[TrackRecord] = {
    val tags: List[TrackRecord] =
      Files
        .walk(path.toJavaPath)
        .toScala(LazyList)
        .map(javaPath => WindowsPath(javaPath.toAbsolutePath.toString))
        .filter(VirtualPath.mp3Filter)
        .map(TikaTagger.tag)
        .toList

    tags.foreach(tag => println(s"Tag is: $tag"))

    tags
  }
}
