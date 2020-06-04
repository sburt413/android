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

  def getIndexRecordForDevice(device: Device): Option[DeviceIndexRecord] =
    getDeviceIndex(device)
      .flatMap(index => {
        val fileInput: InputStream = Files.newInputStream(index)
        val fileContents: String = IOUtils.readLines(fileInput, Charset.defaultCharset()).asScala.mkString(" ")
        fileInput.close()

        // TODO: Don't flatten away errors
        Parse.decodeOption[DeviceIndexRecord](fileContents)
      })

  def writeIndex(device: Device, record: DeviceIndexRecord): Unit = {
    val path: Path = getOrCreateDeviceIndex(device)
    logger.info(s"Writing index to $path")
    val fileOutput: BufferedWriter = Files.newBufferedWriter(path)
    fileOutput.write(record.asJson.toString())
    fileOutput.close()
  }

  private def getDeviceIndex(device: Device): Option[Path] =
    Option(getDeviceIndexName(device)).filter(path => Files.exists(path))

  private def getOrCreateDeviceIndex(device: Device): Path =
    getDeviceIndex(device).getOrElse(Files.createFile(getDeviceIndexName(device)))

  private def getIndexFolder: Path = {
    val indexPath = Paths.get("index")
    if (!Files.exists(indexPath)) {
      Files.createDirectory(indexPath)
    }

    indexPath
  }

  private def getDeviceIndexName(device: Device): Path =
    Paths.get(getIndexFolder.toString, s"${device.serial}-index-record.json")
}
