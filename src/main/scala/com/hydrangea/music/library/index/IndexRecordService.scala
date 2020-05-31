package com.hydrangea.music.library.index

import java.io.{BufferedWriter, FileNotFoundException, InputStream}
import java.nio.charset.Charset
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.android.adb.Device
import org.apache.commons.io.IOUtils
import org.slf4j.Logger

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

object IndexRecordService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(IndexRecordService.getClass)

  def getIndexRecordForDevice(device: Device): Option[DeviceIndexRecord] =
    try {
      val fileInput: InputStream = Files.newInputStream(indexPath(device))
      val fileContents: String = IOUtils.readLines(fileInput, Charset.defaultCharset()).asScala.mkString(" ")
      fileInput.close()

      // TODO: Don't flatten away errors
      Parse.decodeOption[DeviceIndexRecord](fileContents)
    } catch {
      case _: FileNotFoundException | _: NoSuchFileException => None
      case NonFatal(e)                                       => throw e
    }

  def writeIndex(device: Device, record: DeviceIndexRecord): Unit = {
    val path: Path = indexPath(device)
    logger.info(s"Writing index to $path")
    val fileOutput: BufferedWriter = Files.newBufferedWriter(path)
    fileOutput.write(record.asJson.toString())
    fileOutput.close()
  }

  def indexPath(device: Device): Path = Paths.get(s"${device.serial}-index-record.json")
}
