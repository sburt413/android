package com.hydrangea.music.library.index

import java.io.{BufferedWriter, InputStream}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.android.adb.Device
import org.apache.commons.io.IOUtils
import org.slf4j.Logger

import scala.jdk.CollectionConverters._

object DeviceIndexRecordService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(DeviceIndexRecordService.getClass)

  def getRecordForDevice(device: Device): Option[DeviceIndexRecord] =
    getRecord(device)
      .flatMap(index => {
        val fileInput: InputStream = Files.newInputStream(index)
        val fileContents: String = IOUtils.readLines(fileInput, Charset.defaultCharset()).asScala.mkString(" ")
        fileInput.close()

        // TODO: Don't flatten away errors
        Parse.decodeOption[DeviceIndexRecord](fileContents)
      })

  def writeRecord(device: Device, record: DeviceIndexRecord): Unit = {
    val path: Path = getOrCreateRecord(device)
    logger.info(s"Writing record to $path")
    val fileOutput: BufferedWriter = Files.newBufferedWriter(path)
    fileOutput.write(record.asJson.toString())
    fileOutput.close()
  }

  def deleteRecord(device: Device): Unit = {
    val path: Path = getOrCreateRecord(device)
    logger.info(s"Deleting record at $path")
    if (Files.exists(path)) {
      Files.delete(path)
    } else {
      logger.warn(s"No record exists for device ${device.serial}")
    }
  }

  private def getRecord(device: Device): Option[Path] =
    Option(getRecordName(device)).filter(path => Files.exists(path))

  private def getOrCreateRecord(device: Device): Path =
    getRecord(device).getOrElse(Files.createFile(getRecordName(device)))

  private def getRecordFolder: Path = {
    val indexPath = Paths.get("record")
    if (!Files.exists(indexPath)) {
      Files.createDirectory(indexPath)
    }

    indexPath
  }

  private def getRecordName(device: Device): Path =
    Paths.get(getRecordFolder.toString, s"${device.serial}-index-record.json")
}
