package com.hydrangea.music.tagger

import java.io.{FileInputStream, InputStream, PipedInputStream, PipedOutputStream}
import java.nio.file.{Files, Path}

import com.hydrangea.android.adb.{ADBCommandLine, ADBProcessListener, ADBProcessListenerBuilder}
import com.hydrangea.android.file.AndroidRegularFile
import com.hydrangea.music.library.{Tag, TrackRecord}
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

//  def tag(commandLine: ADBCommandLine, file: AndroidRegularFile): TrackRecord = {
//    logger.trace(s"Extracting record for file ($file).")
//    val hash: String = commandLine.sha1sum(file.path)
//
//    // commandLine | tagReader | tagParser
//    val start: Long = System.currentTimeMillis()
//    val tempFile: Path = Files.createTempFile("abd-tag", ".mp3")
//    val transferExitCode: Int = commandLine.transferViaCat2(file.path, tempFile)
//    if (transferExitCode != 0) {
//      throw new RuntimeException(s"Transfer of file ($file) is unsuccessful: ${transferExitCode}")
//    }
//
//    println(s"Wrote to file: $tempFile in ${System.currentTimeMillis() - start}")
//    val parser = new Mp3Parser
//    val metadata = new Metadata
//
//    parser.parse(new FileInputStream(tempFile.toFile), new DefaultHandler, metadata, new ParseContext)
//
//    val title: String = Option(metadata.get("title"))
//      .getOrElse(throw new IllegalArgumentException(s"Cannot parse title from file ($file)"))
//    val album: String = Option(metadata.get("xmpDM:album"))
//      .getOrElse(throw new IllegalArgumentException(s"Cannot parse album from file ($file)"))
//    val artist: String = Option(metadata.get("xmpDM:artist"))
//      .getOrElse(throw new IllegalArgumentException(s"Cannot parse artist from file ($file)"))
//
//    val record: TrackRecord = TrackRecord(hash, file, Tag(title, album, artist))
//    logger.debug(s"Record for file ($file) is: $record")
//    record
//  }

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
//            count = count + readLength
//            println(s"Piped ${readLength} bytes / ${count}")
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

    println(s"Read file in ${System.currentTimeMillis() - start}")

    val title: String = Option(metadata.get("title"))
      .getOrElse(throw new IllegalArgumentException(s"Cannot parse title from file ($file)"))
    val album: String = Option(metadata.get("xmpDM:album"))
      .getOrElse(throw new IllegalArgumentException(s"Cannot parse album from file ($file)"))
    val artist: String = Option(metadata.get("xmpDM:artist"))
      .getOrElse(throw new IllegalArgumentException(s"Cannot parse artist from file ($file)"))

    val record: TrackRecord = TrackRecord(hash, file, Tag(title, album, artist))
    logger.debug(s"Record for file ($file) is: $record")
    record
  }
}
