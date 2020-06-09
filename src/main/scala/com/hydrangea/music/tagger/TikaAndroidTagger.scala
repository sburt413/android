package com.hydrangea.music.tagger

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.nio.file.Files

import com.hydrangea.android.adb.{ADBCommandLine, ADBProcessListener, ADBProcessListenerBuilder}
import com.hydrangea.android.file.{AndroidRegularFile, VirtualFile, WindowsFile, WindowsPath}
import com.hydrangea.music.library.{Tag, TrackRecord}
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.ParseContext
import org.apache.tika.parser.mp3.Mp3Parser
import org.slf4j.Logger
import org.xml.sax.helpers.DefaultHandler

object TikaAndroidTagger {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(TikaAndroidTagger.getClass)

  private class ParserThread(inputStream: InputStream, metadata: Metadata) extends Thread {
    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser

    override def run(): Unit = {
      parser.parse(inputStream, handler, metadata, context)
    }
  }

  def tag(commandLine: ADBCommandLine, file: AndroidRegularFile): TrackRecord = {
    logger.trace(s"Extracting record for file ($file).")
    val hash: String = commandLine.sha1sum(file.path)

    val metadata = new Metadata

    val outPipe = new PipedOutputStream()
    val inputPipe = new PipedInputStream(outPipe)

    val parserThread = new ParserThread(inputPipe, metadata)

    val tagReaderBuilder: ADBProcessListenerBuilder =
      inputStream => {
        val startParser: () => Unit = () => parserThread.start()
        val onRead: (Array[Byte], Int) => Unit =
          (buffer, readLength) => {
            outPipe.write(buffer, 0, readLength)
          }
        val onClose: () => Unit = () => {
          outPipe.flush()
          outPipe.close()

          parserThread.join();
          inputPipe.close()
        }

        ADBProcessListener(startParser, onRead, onClose)(inputStream)
      }

    val start: Long = System.currentTimeMillis()

    // commandLine | tagReader | tagParser
    val transferExitCode: Int = commandLine.transferViaCat(file.path, tagReaderBuilder)
    if (transferExitCode != 0) {
      throw new RuntimeException(s"Transfer of file ($file) is unsuccessful: ${transferExitCode}")
    }

    logger.debug(s"Read file in ${System.currentTimeMillis() - start}")
    val record: TrackRecord = fromMetadata(hash, file, metadata)
    logger.debug(s"Record for file ($file) is: $record")
    record
  }

  def tag(path: WindowsPath): TrackRecord = {
    System.out.println(s"Parsing: ${path.raw}")

    val handler = new DefaultHandler
    val context = new ParseContext
    val parser = new Mp3Parser

    val metadata = new Metadata
    val sha1 = new DigestUtils(SHA_1)
    val hash: String = sha1.digestAsHex(path.toJavaFile)
    parser.parse(Files.newInputStream(path.toJavaPath), handler, metadata, context)

    val record: TrackRecord = fromMetadata(hash, WindowsFile.of(path.toJavaPath), metadata)
    System.out.println(s"Record is: ${record}")
    record
  }

  private def fromMetadata(hash: String, file: VirtualFile, metadata: Metadata): TrackRecord = {
    val title: String =
      Option(metadata.get("title"))
        .getOrElse(throw new IllegalArgumentException(s"Cannot parse title from file ($file)"))
    val album: String =
      Option(metadata.get("xmpDM:album"))
        .getOrElse(throw new IllegalArgumentException(s"Cannot parse album from file ($file)"))
    val artist: String =
      Option(metadata.get("xmpDM:artist"))
        .getOrElse(throw new IllegalArgumentException(s"Cannot parse artist from file ($file)"))
    val year: Option[Int] =
      Option(metadata.get("xmpDM:releaseDate"))
        .filter(_.nonEmpty)
        .map(_.toInt)

    val (trackNumber, trackCount) =
      Option(metadata.get("xmpDM:trackNumber")).map(TrackRecord.slashSplit).getOrElse((None, None))
    val (discNumber, discCount) =
      Option(metadata.get("xmpDM:discNumber")).map(TrackRecord.slashSplit).getOrElse((None, None))

    TrackRecord(hash, file, Tag(title, album, artist, year, trackNumber, trackCount, discNumber, discCount))
  }
}
