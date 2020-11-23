package com.hydrangea.music.track.merge

import java.io.InputStream

import com.hydrangea.android.adb.{ADB, ADBCommandLine, Device}
import com.hydrangea.android.file.{AndroidPath, AndroidRegularFile}
import com.hydrangea.file.{AndroidLocation, FileSystemService, UnixPath}
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.tagger.TikaTagger
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec

class ADBDriver extends AnyFlatSpec {
  import com.hydrangea.file.FilePath._

  "Driver" should "run" in {
    val filePath: AndroidPath =
      AndroidPath("/storage/0123-4567/Music/Whitesnake/The Best Of Whitesnake/02 - Still Of The Night.mp3")

    val device: Device = ADB.firstDevice
    val commandline: ADBCommandLine = device.commandline()
    val file: AndroidRegularFile =
      commandline
        .stat(filePath)
        .flatMap(_.to[AndroidRegularFile])
        .getOrElse(throw new IllegalStateException("No file!"))

    val record: TrackRecord = TikaTagger.tag(commandline, file)
    println(s"Parsed record: $record")

    val srcPath: UnixPath = filePath.raw.toUnixPath
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
