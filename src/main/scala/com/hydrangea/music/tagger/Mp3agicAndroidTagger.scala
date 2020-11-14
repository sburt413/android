package com.hydrangea.music.tagger

import java.nio.file.{FileVisitor, Files, Path}

import com.hydrangea.android.adb.ADBCommandLine
import com.hydrangea.android.file.WindowsFile._
import com.hydrangea.android.file._
import com.hydrangea.music.TrackRecord
import com.mpatric.mp3agic.{ID3v2, Mp3File}
import org.apache.commons.io.FileUtils
import org.slf4j.Logger

object Mp3agicAndroidTagger {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(Mp3agicAndroidTagger.getClass)

  def tagDirectory(commandLine: ADBCommandLine, remoteDirectory: AndroidDirectory): Seq[(AndroidFile, TrackRecord)] = {
    val localTempDirectory: Path = Files.createTempDirectory("ADB")
    logger.debug(s"Created temporary directory to transfer and tag files: ${localTempDirectory.toAbsolutePath}")
    val target: WindowsDirectory = localTempDirectory.toLocalDirectory.getOrElse(
      throw new IllegalStateException("Temp directory is not a directory."))

    val result: Seq[(AndroidFile, TrackRecord)] =
      commandLine
        .pull(remoteDirectory, target)
        .toSeq
        .filter({ case (_, copied) => VirtualPath.mp3Filter(copied) })
        .map({
          case (src, dest) =>
            val sourceSum: String = commandLine.sha1sum(src)
            val srcFile: AndroidFile =
              commandLine
                .stat(src)
                .getOrElse(throw new IllegalStateException(s"Source file for $src no longer exists."))

            val destFile: WindowsFile =
              LocalFileSystem.read(dest).getOrElse(throw new IllegalStateException(s"Did not copy file: $dest"))
            val mp3File = new Mp3File(destFile.toJavaFile)
            val tag: ID3v2 = mp3File.getId3v2Tag
            println(src + s" ($sourceSum) => " + src + s" (${tag.getArtist} / ${tag.getTitle})")

            srcFile -> TrackRecord(sourceSum, srcFile, tag)
        })

    FileUtils.deleteDirectory(localTempDirectory.toFile)
    result
  }
}
