package com.hydrangea.android.adb

import java.io.StringWriter
import java.nio.charset.Charset
import java.time.Instant
import java.util.concurrent.TimeUnit

import com.hydrangea.android.adb.find.{FindDepth, FindOption}
import com.hydrangea.android.adb.ls.LsParser
import com.hydrangea.android.file.AndroidPath._
import com.hydrangea.android.file._
import org.apache.commons.io.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.TimeUnit

case class Device(serial: String) {
  def commandline(timeout: Timeout = Device.defaultTimeout): ADBCommandline = new ADBCommandline(this, timeout)
}

object Device {
  val defaultTimeout: Timeout = Timeout(10, TimeUnit.MINUTES)
}

case class Timeout(count: Long, units: TimeUnit)

object Command {
  private[adb] val logger: Logger = LoggerFactory.getLogger(Command.getClass)

  def run(timeout: Timeout)(args: String*): (Int, Seq[String], Seq[String]) = {
    logger.info("EXECUTING COMMAND: " + args)
    val process: Process = new ProcessBuilder(args: _*).start()

    val finished: Boolean = process.waitFor(timeout.count, timeout.units)
    if (!finished) {
      logger.warn(s"COMMAND did not finish in $timeout, attempting to kill process")

      val killed: Boolean = process.destroyForcibly().waitFor(1, TimeUnit.MINUTES)
      if (killed) {
        logger.warn(s"COMMAND was not killed, manually kill the process for: ${args.mkString(" ")}")
      }

      throw new RuntimeException(s"Process killed due to timeout: ${args.mkString(" ")}")
    }

    val exitValue: Int = process.exitValue()
    if (exitValue != 0) {
      logger.warn(s"NON ZERO EXIT VALUE ($exitValue) for $args")
    }

    val output = new StringWriter()
    IOUtils.copy(process.getInputStream, output, Charset.defaultCharset)

    val err = new StringWriter()
    IOUtils.copy(process.getErrorStream, err, Charset.defaultCharset)

    val outputLines: Seq[String] = output.toString.split("\r\n")
    logger.debug(s"OUTPUT => ${outputLines.mkString("\n")}")

    val errLines: Seq[String] = err.toString.split("\r\n").filterNot(_.isEmpty)
    if (errLines.nonEmpty) {
      logger.debug(s"ERROR => ${outputLines.mkString("\n")}")
    }

    (exitValue, outputLines, errLines)
  }
}

object ADB {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[Device])

  def commandline(device: Device, timeout: Timeout): ADBCommandline =
    new ADBCommandline(device, timeout)

  def devices: Seq[Device] = {
    val (_, stdout, _) = Command.run(Timeout(10, TimeUnit.SECONDS))("adb", "devices")

    // Skip 'List of devices attached'
    stdout.tail.map(_.split("\\s+")(0)).map(serial => Device(serial))
  }

  def firstDevice: Device = {
    val device: Device = devices.headOption.getOrElse(throw new IllegalStateException("No Android devices found"))
    logger.info(s"Using device: $device")
    device
  }
}

class ADBCommandline(device: Device, timeout: Timeout) {
  import ADBCommandline.logger

  private def cmd(command: String*): (Int, Seq[String], Seq[String]) = {
    val args: Seq[String] = Seq("adb", "-s", device.serial) ++ command
    Command.run(timeout)(args: _*)
  }

  private def shellCmd(command: String*): (Int, Seq[String], Seq[String]) = {
    cmd(Seq("shell") ++ command: _*)
  }

  private def buildAndroidFile(path: AndroidPath, lastModified: Instant): RemoteFile = {
    if (path.isDirectoryPath) {
      AndroidDirectory(path, lastModified)
    } else {
      AndroidFile(path, lastModified)
    }
  }

  def list(directoryPath: AndroidPath): Seq[RemoteFile] = {
    val (_, listing, err) = shellCmd("ls", "-pla", "--full-time", directoryPath.quoted)
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `list`: " + err.mkString("\n"))
    }

    for {
      (fileName, lastModified) <- LsParser.parseDirectory(listing)
    } yield buildAndroidFile(directoryPath ++ fileName, lastModified)
  }

  def listRecursive(directoryPath: AndroidPath): Seq[RemoteFile] = {
    val (_, listings, err) = shellCmd("ls", "-Rpla", "--full-time", directoryPath.quoted)
    if (err.nonEmpty) {
      throw new RuntimeException("Error running recursive `list`: " + err.mkString("\n"))
    }

    for {
      (path, fileName, lastModified) <- LsParser.parseDirectories(listings)
    } yield buildAndroidFile(path ++ fileName, lastModified)
  }

  def find(directoryPath: AndroidPath, options: FindOption*): Seq[VirtualPath] = {
    findCmd(directoryPath, options.flatMap(opt => Seq(opt.param, opt.value)))
  }

  def findDirectories(directoryPath: AndroidPath, options: FindOption*): Seq[AndroidPath] = {
    val directoryType = Seq("-type", "d")
    val findOpts: Seq[String] = options.flatMap(opt => Seq(opt.param, opt.value))
    findCmd(directoryPath, directoryType ++ findOpts)
  }

  private def findCmd(directoryPath: AndroidPath, params: Seq[String]): Seq[AndroidPath] = {
    val args: Seq[String] = Seq("find") ++ Seq(directoryPath.quoted) ++ params
    val (_, found, err) = shellCmd(args: _*)
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `find`: " + err.mkString("\n"))
    }

    found.map(AndroidPath.apply)
  }

  def stat(path: AndroidPath): Option[RemoteFile] = {
    // adb -d shell stat -c '%Y %F' '/storage/0123-4567/Music/'
    val args = Seq("stat", "-c", "'%Y %F'", path.quoted)
    val (_, output, err) = shellCmd(args: _*)
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `stat`: " + err.mkString("\n"))
    }

    Option(output)
      .filter(_.size == 1)
      .map(_.head)
      .filterNot(_.contains("No such file or directory"))
      .map(statLine => {
        val columns: Array[String] = statLine.replace("'", "").split("\\s+")
        val modifiedEpochSeconds: String = columns(0)
        val modifiedTime: Instant = Instant.ofEpochSecond(modifiedEpochSeconds.toLong)
        val fileType: String = columns(1)

        if (fileType == "directory") {
          AndroidDirectory(path, modifiedTime)
        } else {
          AndroidFile(path, modifiedTime)
        }
      })
  }

  def pull(sourceDirectory: AndroidDirectory, targetDirectory: LocalDirectory): Map[AndroidPath, WindowsPath] = {
    // adb -d pull -a '/storage/0123-4567/Music/August\ Burns\ Red/' 'C:\adb'
    val args: Seq[String] = Seq("pull", "-a", sourceDirectory.path.quoted, targetDirectory.path.quoted)
    // For some reason the pull command spit output to the error stream
    val (_, _, output) = cmd(args: _*)
    // skipping output:
    // 'pull: building file list...'
    // 'x files pulled. y files skipped.'
    // '2938 KB/s (97352327 bytes in 32.355s)'
    val fileLines: Seq[String] = output.slice(1, output.size - 2)
    fileLines
      .map(line => {
        val split: Array[String] = line.replace("pull: ", "").split(" -> ")
        val sourcePath = AndroidPath(split(0))

        val destPath: WindowsPath = AndroidPath(split(1)).toWindows
        sourcePath -> destPath
      })
      .toMap

//    $ adb -d pull /storage/0123-4567/Music/August\ Burns\ Red/Guardians/
//    pull: building file list...
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/11 - Three Fountains.mp3 -> ./11 - Three Fountains.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/10 - Empty Heaven.mp3 -> ./10 - Empty Heaven.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/09 - Extinct By Instinct.mp3 -> ./09 - Extinct By Instinct.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/08 - Bloodletter.mp3 -> ./08 - Bloodletter.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/07 - Ties That Bind.mp3 -> ./07 - Ties That Bind.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/06 - Dismembered Memory.mp3 -> ./06 - Dismembered Memory.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/05 - Lighthouse.mp3 -> ./05 - Lighthouse.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/04 - Defender.mp3 -> ./04 - Defender.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/03 - Paramount.mp3 -> ./03 - Paramount.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/02 - Bones.mp3 -> ./02 - Bones.mp3
//    pull: /storage/0123-4567/Music/August Burns Red/Guardians/01 - The Narrative.mp3 -> ./01 - The Narrative.mp3
//    11 files pulled. 0 files skipped.
//    2938 KB/s (97352327 bytes in 32.355s)

  }

  def pull(source: AndroidFile, target: LocalFile): Option[VirtualPath] = {
    // adb -d pull -a '/storage/0123-4567/Music/August Burns Red/Guardians/02 - Bones.mp3' /cygdrive/d/adb/02\ -\ Bones.mp3
    // For some reason the pull command spits its output to the error stream
    val (_, _, output) = cmd("pull", "-a", source.path.quoted, target.path.quoted)
    // 2938 KB/s (97352327 bytes in 32.355s)
    Option(output.head).filter(_.contains("bytes in")).map(_ => target.path)
  }

  def sha1sum(path: AndroidPath): String = {
    val (_, output, err) = shellCmd("sha1sum", path.quoted)
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `sha1sum`: " + err.mkString("\n"))
    }

    output.head.split("\\s+")(0)
  }

  def scan(path: AndroidPath): Seq[RemoteFile] = {
    val directories: Seq[AndroidPath] = findDirectories(path, FindDepth(1))
    directories.indices.flatMap { index =>
      val directoryPath: AndroidPath = directories(index)
      if ((index + 1) % 10 == 0 || index == 0 || index == directories.size - 1) {
        logger.info(s"Scanning: ${index + 1}/${directories.size}")
      }

      listRecursive(directoryPath)
    }
  }

}

object ADBCommandline {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[ADBCommandline])
}
