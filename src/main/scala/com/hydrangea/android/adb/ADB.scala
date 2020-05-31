package com.hydrangea.android.adb

import java.nio.charset.Charset
import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.TimeUnit

import com.hydrangea.android.adb.ADBProcess.WaitFor
import com.hydrangea.android.adb.find.{FindDepth, FindOption}
import com.hydrangea.android.adb.ls.LsParser
import com.hydrangea.android.file.VirtualPath._
import com.hydrangea.android.file._
import org.slf4j.{Logger, LoggerFactory}

case class Device(serial: String) {
  def commandline(timeout: Timeout = Device.defaultTimeout): ADBCommandLine = new ADBCommandLine(this, timeout)
  def withCommandLine[A](timeout: Timeout = Device.defaultTimeout)(fn: ADBCommandLine => A): A =
    fn(new ADBCommandLine(this, timeout))
}

object Device {
  val defaultTimeout: Timeout = Timeout(10, TimeUnit.MINUTES)
}

object ADB {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[Device])

  def commandLine(device: Device, timeout: Timeout): ADBCommandLine =
    new ADBCommandLine(device, timeout)

  def devices: Seq[Device] = {
    val (_, stdout, _) = ADBProcess.runAndParse(Timeout(10, TimeUnit.SECONDS), "adb", "devices")

    // Skip 'List of devices attached'
    stdout.tail.map(_.split("\\s+")(0)).map(serial => Device(serial))
  }

  def firstDevice: Device = {
    val device: Device = devices.headOption.getOrElse(throw new IllegalStateException("No Android devices found"))
    logger.info(s"Using device: $device")
    device
  }
}

class ADBCommandLine(device: Device, timeout: Timeout) {
  import ADBCommandLine.logger

  /*
  val toEval = s"'ls -la ${path.raw.replace(" ", "\\ ").replace("!", "\\!")}'"
      val cmd = "\"" + "eval " + toEval + "\""
      val (_, output, _) = commandLine.execOutCmdAndParse(cmd)
   */

//  private def evalWrap(command: Seq[String]): Seq[String] = {
//    Seq("eval") ++ command
////    "\"" + s"eval '${command.mkString(" ")}'" + "\""
//  }

  private def adbCmd(command: String*): Seq[String] = Seq("adb", "-s", device.serial) ++ command

  private def shellCmd(command: String*): Seq[String] = Seq("shell") ++ command

  private def execOutCmd(command: String*): Seq[String] = Seq("exec-out") ++ command

  private def runAndParse(command: Seq[String]): (Int, Seq[String], Seq[String]) = {
    val args: Seq[String] = adbCmd(command: _*)
    ADBProcess.runAndParse(timeout, args: _*)
  }

  private def run(command: Seq[String],
                  stdoutReader: ADBProcessListenerBuilder,
                  stderrReader: ADBProcessListenerBuilder): Int = {
    val args: Seq[String] = adbCmd(command: _*)
    ADBProcess(args, stdoutReader, stderrReader, Some(timeout)).run()
  }

  def run(command: Seq[String], destPath: Path): Int = {
    val args: Seq[String] = adbCmd(command: _*)
    new ADBProcess2(args, destPath, Some(timeout)).run()
  }

  private def runAsync(command: Seq[String],
                       stdoutReader: ADBProcessListenerBuilder,
                       stderrReader: ADBProcessListenerBuilder): WaitFor = {
    val args: Seq[String] = adbCmd(command: _*)
    ADBProcess(args, stdoutReader, stderrReader, Some(timeout)).runAsync()
  }

  //  private def cmd(command: String*): (Int, InputStream, InputStream) = {
//    val args: Seq[String] = Seq("adb", "-s", device.serial) ++ command
//    Command.run(timeout)(args: _*)
//  }
//

//

//
//  private def execOutCmd(command: String*): (Int, InputStream, InputStream) = {
//    cmd(Seq("exec-out") ++ command: _*)
//  }
//
//  // TODO: DELETE
//  def execOutCmdAndParse(command: String*): (Int, Seq[String], Seq[String]) = {
//    cmdAndParse(Seq("exec-out") ++ command: _*)
//  }

  private def buildAndroidFile(path: AndroidPath, lastModified: Instant): AndroidFile = {
    if (path.isDirectoryPath) {
      AndroidDirectory(path, lastModified)
    } else {
      AndroidRegularFile(path, lastModified)
    }
  }

  def list(directoryPath: AndroidPath): Seq[AndroidFile] = {
    val (_, listing, err) = runAndParse(shellCmd("ls", "-pla", "--full-time", directoryPath.singleQuoted))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `list`: " + err.mkString("\n"))
    }

    for {
      (fileName, lastModified) <- LsParser.parseDirectory(listing)
    } yield buildAndroidFile(directoryPath :+ fileName, lastModified)
  }

  def listRecursive(directoryPath: AndroidPath): Seq[AndroidFile] = {
    val (_, listings, err) = runAndParse(shellCmd("ls", "-Rpla", "--full-time", directoryPath.singleQuoted))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running recursive `list`: " + err.mkString("\n"))
    }

    for {
      (path, fileName, lastModified) <- LsParser.parseDirectories(listings)
    } yield buildAndroidFile(path :+ fileName, lastModified)
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
    val args: Seq[String] = Seq("find") ++ Seq(directoryPath.singleQuoted) ++ params
    val (_, found, err) = runAndParse(shellCmd(args: _*))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `find`: " + err.mkString("\n"))
    }

    found.map(AndroidPath.apply)
  }

  def stat(path: AndroidPath): Option[AndroidFile] = {
    // adb -d shell stat -c '%Y %F' '/storage/0123-4567/Music/'
    val args = Seq(s"stat -c '%Y %F' ${path.singleQuoted}")
    val (_, output, err) = runAndParse(shellCmd(args: _*))
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
          AndroidRegularFile(path, modifiedTime)
        }
      })
  }

  def statOrThrow(path: AndroidPath): AndroidFile =
    stat(path).getOrElse(throw new IllegalStateException(s"No file found for path: $path"))

  def pull(sourceDirectory: AndroidDirectory, targetDirectory: WindowsDirectory): Map[AndroidPath, WindowsPath] = {
    // adb -d pull -a '/storage/0123-4567/Music/August\ Burns\ Red/' 'C:\adb'
    val args: Seq[String] = Seq("pull", "-a", sourceDirectory.path.singleQuoted, targetDirectory.path.singleQuoted)
    // For some reason the pull command spit output to the error stream
    val (_, _, output) = runAndParse(args)
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

  def pull(source: AndroidRegularFile, target: WindowsFile): Option[VirtualPath] = {
    // adb -d pull -a '/storage/0123-4567/Music/August Burns Red/Guardians/02 - Bones.mp3' /cygdrive/d/adb/02\ -\ Bones.mp3
    // For some reason the pull command spits its output to the error stream
    val command = Seq("pull", "-a", source.path.singleQuoted, target.path.singleQuoted)
    val (_, _, output) = runAndParse(command)
    // 2938 KB/s (97352327 bytes in 32.355s)
    Option(output.head).filter(_.contains("bytes in")).map(_ => target.path)
  }

  def sha1sum(path: AndroidPath): String = {
    val (_, output, err) = runAndParse(shellCmd("sha1sum", path.singleQuoted))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `sha1sum`: " + err.mkString("\n"))
    }

    output.head.split("\\s+")(0)
  }

  def scan(path: AndroidPath): Seq[AndroidFile] = {
    val directories: Seq[AndroidPath] = findDirectories(path, FindDepth(1))
    directories.indices.flatMap { index =>
      val directoryPath: AndroidPath = directories(index)
      if ((index + 1) % 10 == 0 || index == 0 || index == directories.size - 1) {
        logger.info(s"Scanning: ${index + 1}/${directories.size}")
      }

      listRecursive(directoryPath)
    }
  }

  def mostRecentUpdate(path: AndroidPath): Option[Instant] = {
    val removeParentDirectories = "grep -v '\\.\\./'"
    val topEntry = "tail -n 1"
    val (_, output, err) =
      runAndParse(shellCmd("ls", "-Rplart", "--full-time", path.escaped, "|", removeParentDirectories, "|", topEntry))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `sha1sum`: " + err.mkString("\n"))
    }

    LsParser
      .parseFile(output.head)
      .map({
        case (_, mostRecentUpdate) =>
          logger.info(s"Most recent update inside $path: $mostRecentUpdate")
          mostRecentUpdate
      })
  }

  def transferViaBase64(path: AndroidPath,
                        base64StdoutListener: ADBProcessListenerBuilder,
                        stderrListener: ADBProcessListenerBuilder = ADBProcessListener.pipedBuilder(System.err)): Int =
    run(execOutCmd("base64", path.quoted), base64StdoutListener, stderrListener)

  def transferViaCat(path: AndroidPath,
                     stdoutListener: ADBProcessListenerBuilder,
                     stderrListener: ADBProcessListenerBuilder = ADBProcessListener.pipedBuilder(System.err)): Int =
    run(execOutCmd("cat", path.quoted), stdoutListener, stderrListener)

  def transferViaCat2(srcPath: AndroidPath, destPath: Path): Int =
    run(execOutCmd("cat", srcPath.quoted), destPath)

//  def transferViaBase64(path: AndroidPath, dest: WindowsPath, lineSeparator: String = LineSeparator.Windows): Unit = {
//    // `exec-out` has differing quoting requirements than `shell`
//    val (_, stdout, stderr) = shellCmd("eval", s"base64 ${path.raw}")
//    if (stderr.nonEmpty) {
//      throw new RuntimeException("Error running cat transfer: " + stderr.mkString("\n"))
//    }
//
//    println(stdout)
//  }
//
//  def transferViaBase64Tunnel(path: AndroidPath, lineSeparator: String = LineSeparator.Windows): InputStream = {
//    // `exec-out` has differing quoting requirements than `shell`
//    val (_, encodedFileStream, stderr) = execOutCmd("echo", "base64", path.quoted)
//    val stderrLines: Seq[String] = Command.toLines(stderr, lineSeparator)
//    if (stderrLines.nonEmpty) {
//      throw new RuntimeException("Error running base64 transfer: " + stderrLines.mkString("\n"))
//    }
//
//////    Base64.getDecoder.wrap(encodedFileStream)
//    encodedFileStream
//  }
}

object ADBCommandLine {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[ADBCommandLine])

  val UTF_8: java.nio.charset.Charset = Charset.forName("UTF-8")
}
