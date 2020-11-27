package com.hydrangea.music.library.record

import java.io.{BufferedWriter, InputStream}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.Configuration
import com.hydrangea.music.library.IndexName
import org.apache.commons.io.IOUtils
import org.slf4j.Logger

import scala.jdk.CollectionConverters._

abstract class IndexRecordService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(getClass)

  def getRecord(name: IndexName): Option[IndexRecord] =
    getRecordFile(name)
      .flatMap(index => {
        val fileInput: InputStream = Files.newInputStream(index)
        val fileContents: String = IOUtils.readLines(fileInput, Charset.defaultCharset()).asScala.mkString(" ")
        fileInput.close()

        // TODO: Don't flatten away errors
        Parse.decodeOption[IndexRecord](fileContents)
      })

  def writeRecord(name: IndexName, record: IndexRecord): Unit = {
    val path: Path = getOrCreateRecordFile(name)
    logger.info(s"Writing record to $path")
    val fileOutput: BufferedWriter = Files.newBufferedWriter(path)
    fileOutput.write(record.asJson.toString())
    fileOutput.close()
  }

  def deleteRecord(name: IndexName): Unit = {
    val path: Path = getOrCreateRecordFile(name)
    logger.info(s"Deleting record at $path")
    if (Files.exists(path)) {
      Files.delete(path)
    } else {
      logger.warn(s"No record exists for index ${name.value}")
    }
  }

  protected def getRecordFile(name: IndexName): Option[Path] =
    Option(filePath(name)).filter(path => Files.exists(path))

  private def getOrCreateRecordFile(name: IndexName): Path =
    getRecordFile(name).getOrElse(Files.createFile(filePath(name)))

  protected def getRecordFolder: Path = {
    val indexPath = Configuration.indexRecordDirectory
    if (!Files.exists(indexPath)) {
      Files.createDirectory(indexPath)
    }

    indexPath
  }

  private def filePath(name: IndexName): Path =
    Paths.get(getRecordFolder.toString, name.value + s"-$fileTag.json")

  protected def fileTag: String
}
