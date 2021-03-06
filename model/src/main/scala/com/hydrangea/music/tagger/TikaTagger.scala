package com.hydrangea.music.tagger

import java.io.InputStream

import com.google.inject.Inject
import com.hydrangea.file.{FileLocation, FileSystemService}
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.track.Tag
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.ParseContext
import org.apache.tika.parser.mp3.Mp3Parser
import org.slf4j.Logger
import org.xml.sax.helpers.DefaultHandler

class TikaTagger @Inject()(fileSystemService: FileSystemService) {
  import TikaTagger._

  private class ParserThread(inputStream: InputStream, metadata: Metadata) extends Thread {
    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser

    override def run(): Unit = {
      parser.parse(inputStream, handler, metadata, context)
    }
  }

  def tag(location: FileLocation): Tag = {
    logger.trace(s"Extracting tag for location ($location).")
    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser
    val metadata = new Metadata

    val start: Long = System.currentTimeMillis()
    fileSystemService.read(location) { inputStream =>
      parser.parse(inputStream, handler, metadata, context)
    }

    logger.debug(s"Read file in ${System.currentTimeMillis() - start}")
    val parsedTag: Tag = fromMetadata(metadata)
    logger.debug(s"Tag for location ($parsedTag) is: $parsedTag")
    parsedTag
  }

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

object TikaTagger {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(TikaTagger.getClass)
}
