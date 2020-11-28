package com.hydrangea

import java.io.InputStream

import com.hydrangea.android.adb.{ADB, ADBCommandLine, Device}
import com.hydrangea.file.{AbsolutePath, AndroidLocation, FileSystemService}
import com.hydrangea.music.tagger.TikaTagger
import com.hydrangea.music.track.Tag
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec

class ADBDriver extends AnyFlatSpec {
  import com.hydrangea.file.FilePath._

  "Driver" should "run" in {
    val filePath: AbsolutePath =
      "/storage/0123-4567/Music/Whitesnake/The Best Of Whitesnake/02 - Still Of The Night.mp3".toAbsolutePath.getOrElse(
        throw new IllegalStateException("Illegal Path"))

    val device: Device = ADB.firstDevice
    val commandline: ADBCommandLine = device.commandline()

    val location: AndroidLocation = AndroidLocation(device, filePath)
    val tag: Tag = TikaTagger.tag(location)
    println(s"Parsed tag: $tag")

    val srcPath: AbsolutePath = filePath.raw.toUnixPath
    val start: Long = System.currentTimeMillis()
    val srcLocation: AndroidLocation = AndroidLocation(device, srcPath)
    FileSystemService.copyFromDevice(srcLocation, "F:\\output.mp3".toLocalWindowsPath)
    System.out.println("Copied in " + (System.currentTimeMillis() - start) + "ms")
  }
}

class Copier(inputStream: InputStream) extends Thread {
  private var bytes: Array[Byte] = _

  override def run(): Unit = {
    bytes = IOUtils.toByteArray(inputStream)
    inputStream.close()
  }

  def getBytes: Array[Byte] = bytes
}
