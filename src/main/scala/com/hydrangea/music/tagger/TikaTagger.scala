package com.hydrangea.music.tagger

import java.io.InputStream
import java.nio.file.{Files, Path}

import com.hydrangea.android.adb.ADBCommandLine
import com.hydrangea.android.file.{AndroidFile, VirtualFile, WindowsFile, WindowsPath}
import com.hydrangea.file.{AndroidLocation, FileLocation, FileSystemService, WindowsFileLocation}
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.track.Tag
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.ParseContext
import org.apache.tika.parser.mp3.Mp3Parser
import org.slf4j.Logger
import org.xml.sax.helpers.DefaultHandler

object TikaTagger {
  import com.hydrangea.file.FilePath._

  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(TikaTagger.getClass)

  private class ParserThread(inputStream: InputStream, metadata: Metadata) extends Thread {
    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser

    override def run(): Unit = {
      parser.parse(inputStream, handler, metadata, context)
    }
  }

  def tag(location: FileLocation[_]): Tag =
    location match {
      case windowsLocation: WindowsFileLocation[_] => tag(windowsLocation.javaPath)
      case androidLocation: AndroidLocation        => tagAndroid(androidLocation)
    }

  private def tagAndroid(androidLocation: AndroidLocation): Tag = {
    logger.trace(s"Extracting tag for location ($androidLocation).")
    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser
    val metadata = new Metadata

    val start: Long = System.currentTimeMillis()
    FileSystemService.readFromDevice(androidLocation) { inputStream =>
      parser.parse(inputStream, handler, metadata, context)
    }

    logger.debug(s"Read file in ${System.currentTimeMillis() - start}")
    val parsedTag: Tag = fromMetadata(metadata)
    logger.debug(s"Tag for location ($parsedTag) is: $parsedTag")
    parsedTag
  }

  // TODO
  def tag(commandLine: ADBCommandLine, file: AndroidFile): TrackRecord = {
    logger.trace(s"Extracting record for filepath ($file.path).")
    val hash: String = commandLine.sha1sum(file.path)
    val location: AndroidLocation = AndroidLocation(commandLine.device, file.path.raw.toUnixPath)
    TrackRecord(hash, file, tag(location))
  }

  def tag(path: WindowsPath): TrackRecord = {
    logger.info(s"Parsing: ${path.raw}")

    val sha1 = new DigestUtils(SHA_1)
    val hash: String = sha1.digestAsHex(path.toJavaFile)
    val parsedTag: Tag = tag(path.toJavaPath)
    val record: TrackRecord = TrackRecord(hash, WindowsFile.of(path.toJavaPath), parsedTag)
    logger.info(s"Record is: $record")
    record
  }

  private def tag(javaPath: Path): Tag = {
    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser

    val metadata = new Metadata
    parser.parse(Files.newInputStream(javaPath), handler, metadata, context)
    fromMetadata(metadata)
  }

  // TODO: Record can be composed at different level
  private def fromMetadata(hash: String, file: VirtualFile, metadata: Metadata): TrackRecord =
    TrackRecord(hash, file, fromMetadata(metadata))

  private def fromMetadata(metadata: Metadata): Tag = {
    val title: String =
      Option(metadata.get("title"))
        .getOrElse(throw new IllegalArgumentException(s"Cannot parse title from file"))
    val album: String =
      Option(metadata.get("xmpDM:album"))
        .getOrElse(throw new IllegalArgumentException(s"Cannot parse album from file"))
    val artist: String =
      Option(metadata.get("xmpDM:artist"))
        .getOrElse(throw new IllegalArgumentException(s"Cannot parse artist from file"))
    val year: Option[Int] =
      Option(metadata.get("xmpDM:releaseDate"))
        .filter(_.nonEmpty)
        .map(_.toInt)

    val (trackNumber, trackCount) =
      Option(metadata.get("xmpDM:trackNumber")).map(TrackRecord.slashSplit).getOrElse((None, None))
    val (discNumber, discCount) =
      Option(metadata.get("xmpDM:discNumber")).map(TrackRecord.slashSplit).getOrElse((None, None))

    Tag(title, album, artist, year, trackNumber, trackCount, discNumber, discCount)
  }
}
