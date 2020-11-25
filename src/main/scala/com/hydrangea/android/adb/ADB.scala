package com.hydrangea.android.adb

import java.nio.charset.Charset
import java.time.Instant
import java.util.concurrent.TimeUnit

import com.hydrangea.android.adb.find.{FindDepth, FindOption, ForDirectories, ForRegularFiles}
import com.hydrangea.android.adb.ls.LsParser
import com.hydrangea.android.file.VirtualPath._
import com.hydrangea.android.file._
import com.hydrangea.file.AbsolutePath
import com.hydrangea.process.{CLIProcess, Timeout}
import org.slf4j.{Logger, LoggerFactory}

/**
  * An Android device capable of of communicating with the program via ADB.
  *
  * @param serial the serial number of the device
  */
case class Device(serial: String) {

  /**
    * Returns an abstract [[ADBCommandLine]] for communicating with the device.
    *
    * @param timeout the timeout for any commands run on the commandline
    * @return an abstract [[ADBCommandLine]] for communicating with the device
    */
  def commandline(timeout: Timeout = Device.defaultTimeout,
                  charset: Charset = Charset.defaultCharset()): ADBCommandLine =
    new ADBCommandLine(this, timeout, charset)

  /**
    * Executes the given function, passing it a commandline.
    *
    * @param timeout the timeout for any commands run on the commandline
    * @param fn the function to execute using the terminal
    * @tparam A the type of output from the function
    * @return the result of running the function
    */
  def withCommandLine[A](timeout: Timeout = Device.defaultTimeout, charset: Charset = ADBCommandLine.UTF_16)(
      fn: ADBCommandLine => A): A =
    fn(new ADBCommandLine(this, timeout, charset))
}

object Device {
  val defaultTimeout: Timeout = Timeout(10, TimeUnit.MINUTES)
}

/**
  * The entry point for accessing devices for ADB (Android Debug Bridge).
  */
object ADB {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[Device])

  def commandLine(device: Device, timeout: Timeout, charset: Charset): ADBCommandLine =
    new ADBCommandLine(device, timeout, charset)

  def devices: Seq[Device] = {
    val (_, stdout, _) =
      CLIProcess.runAndParse(Seq("adb", "devices"), Timeout(10, TimeUnit.SECONDS), Charset.defaultCharset())

    // Skip 'List of devices attached'
    stdout.tail.map(_.split("\\s+")(0)).map(serial => Device(serial))
  }

  /**
    * Returns the first device listed by the `adb devices` command.  This may be arbitrary if multiple devices are reachable.
    * @return the first device listed by the `adb` command
    */
  def firstDevice: Device = {
    val device: Device = devices.headOption.getOrElse(throw new IllegalStateException("No Android devices found"))
    logger.info(s"Using device: $device")
    device
  }
}

/**
  * An abstraction over running `adb`.  Most commands are bashed on underlying shell commands, with built in processing
  * of the results.
  *
  * @param device  the device to run the commands on
  * @param timeout the maximum runtime for a single underlying command
  */
class ADBCommandLine(val device: Device, timeout: Timeout, charset: Charset) {
  import ADBCommandLine.logger

  private val adbExec: Seq[String] = Seq("adb", "-s", device.serial)

  /**
    * Wraps the given command in an `adb` command for this commandline's device.
    *
    * @param command the command to run through adb
    * @return the wrapped command
    */
  private def adbCmd(command: String*): Seq[String] = adbExec ++ command

  /**
    * Wraps the given command in `adb shell` command for this commandline's device.
    *
    * @param command the shell command to run through adb
    * @return the wrapped command
    */
  private def adbShellCmd(command: String*): Seq[String] = adbExec ++ Seq("shell") ++ command

  /**
    * Wraps the given command in `adb exec-out` command for this commandline's device.  This is suitable for binary
    * output.
    *
    * @param command the command to run through adb
    * @return the wrapped command
    */
  private def execOutCmd(command: String*): Seq[String] = adbExec ++ Seq("exec-out") ++ command

  private def runAndParse(command: Seq[String]): (Int, Seq[String], Seq[String]) =
    CLIProcess.runAndParse(command, Some(timeout), charset)

  /**
    * Runs an underlying `ls` command and returns the output as files.
    *
    * @param directoryPath the path to the directory to list
    * @return the files immediately in the directory
    */
  def list(directoryPath: AndroidPath): Seq[AndroidFile] = {
    val (_, listing, err) = runAndParse(adbShellCmd("ls", "-pla", "--full-time", directoryPath.commandLine))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `list`: " + err.mkString("\n"))
    }

    for {
      (fileName, lastModified) <- LsParser.parseDirectory(listing)
    } yield AndroidFile(directoryPath :+ fileName, lastModified)
  }

  /**
    * Run and underlying recursive `ls` coomand and returns all contained files as output.
    *
    * @param directoryPath the path to the directory to list
    * @return all files and folders in the given folder's directory tree
    */
  def listRecursive(directoryPath: AndroidPath): Seq[AndroidFile] = {
    val (_, listings, err) = runAndParse(adbShellCmd("ls", "-Rpla", "--full-time", directoryPath.commandLine))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running recursive `list`: " + err.mkString("\n"))
    }

    for {
      (path, fileName, lastModified) <- LsParser.parseDirectories(listings)
    } yield AndroidFile(path :+ fileName, lastModified)
  }

  /**
    * Runs an underlying `find` command with the given options, returning the found values as paths.
    *
    * @param directoryPath the path to the directory to find from
    * @param option0 the criteria to apply to the find command
    * @param options any additional criteria to add to the find command
    * @return all paths found by the command
    */
  def find(directoryPath: AndroidPath, option0: FindOption, options: FindOption*): Seq[AndroidPath] =
    findCmd(directoryPath, (Seq(option0) ++ options).flatMap(opt => Seq(opt.param, opt.value)))

  /**
    * Runs a find command with directories type already applied.
    *
    * @param directoryPath the path to the directory to find from
    * @param options any additional criteria to add to the find command
    * @return all paths found by the command
    */
  def findDirectories(directoryPath: AndroidPath, options: FindOption*): Seq[AndroidPath] =
    find(directoryPath, ForDirectories, options: _*)

  /**
    * Runs a find command with regular files type already applied.
    *
    * @param directoryPath the path to the directory to find from
    * @return all paths found by the command
    */
  def findRegularFiles(directoryPath: AndroidPath): Seq[AndroidPath] =
    find(directoryPath, ForRegularFiles)

  /**
    * Runs the `find` command in an ADB shell and parses [[AndroidPath]]s from the output.
    *
    * @param directoryPath the path to the directory to find from
    * @param params        the commandline parameters to apply
    * @return [[AndroidPath]]s found by the shell command
    */
  private def findCmd(directoryPath: AndroidPath, params: Seq[String]): Seq[AndroidPath] = {
    val args: Seq[String] = Seq("find") ++ Seq(directoryPath.commandLine) ++ params
    val (_, found, err) = runAndParse(adbShellCmd(args: _*))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `find`: " + err.mkString("\n"))
    }

    found.map(AndroidPath.apply)
  }

  /**
    * Runs file status (stat) on a given [[AndroidPath]] and returns the [[AndroidFile]] found.
    *
    * @param path the path to check
    * @return the file, if present; otherwise [[None]] if no file is found
    */
  def stat(path: AndroidPath): Option[AndroidFile] = {
    // adb -d shell stat -c '%Y %F' '/storage/0123-4567/Music/'
    val args = Seq(s"stat -c '%Y %F' ${path.commandLine}")
    val (_, output, err) = runAndParse(adbShellCmd(args: _*))
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

  /**
    * Runs stat, considering the absence of the file to be an exception.
    *
    * @param path the path to check
    * @return the file, if present; otherwise throws an error
    */
  def statOrThrow(path: AndroidPath): AndroidFile =
    stat(path).getOrElse(throw new IllegalStateException(s"No file found for path: $path"))

  /**
    * Run's ADB's native pull command on the device.  The underlying adb implementation is unreliable on Windows
    * computers.
    *
    * @param sourceDirectory the directory on the device to pull from
    * @param targetDirectory the target directory on the host computer
    * @return a map of the file paths pulled from the device to their destination
    */
  def pull(sourceDirectory: AndroidDirectory, targetDirectory: WindowsDirectory): Map[AndroidPath, WindowsPath] = {
    val args: Seq[String] = Seq("pull", "-a", sourceDirectory.path.commandLine, targetDirectory.path.commandLine)
    // For some reason the pull command spit output to the error stream
    val (_, _, output) = runAndParse(args)
    val fileLines: Seq[String] = output.slice(1, output.size - 2)
    fileLines
      .map(line => {
        val split: Array[String] = line.replace("pull: ", "").split(" -> ")
        val sourcePath = AndroidPath(split(0))

        val destPath: WindowsPath = AndroidPath(split(1)).toWindows
        sourcePath -> destPath
      })
      .toMap
  }

  /**
    * Pulls a single file using ADB's native pull.  The underlying adb implementation is unreliable on Windows computers.
    *
    * @param source the directory on the device to pull from
    * @param target the target directory on the host computer
    * @return the target path if the pull was successful
    */
  def pull(source: AndroidRegularFile, target: WindowsFile): Option[VirtualPath] = {
    // For some reason the pull command spits its output to the error stream
    val command = Seq("pull", "-a", source.path.commandLine, target.path.commandLine)
    val (_, _, output) = runAndParse(command)
    // 2938 KB/s (97352327 bytes in 32.355s)
    Option(output.head).filter(_.contains("bytes in")).map(_ => target.path)
  }

  /**
    * Runs a hash on the file at the given path using sha1.
    *
    * @param path the path to the file to hash
    * @return the sha1 hash of the file
    */
  def sha1sum(path: AndroidPath): String = {
    val (_, output, err) = runAndParse(adbShellCmd("sha1sum", path.commandLine))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `sha1sum`: " + err.mkString("\n"))
    }

    output.head.split("\\s+")(0)
  }

  /**
    * Performs a directory by directly recursive listing of all files under the given path.
    *
    * @param path the path to search under
    * @return all files under the given path
    */
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

  /**
    * Return the time of the most recently updated file under that given path.
    *
    * @param path the path to search under
    * @return the most recently updated, if any files exist under the path
    */
  def mostRecentUpdate(path: AndroidPath): Option[Instant] = {
    val removeParentDirectories = "grep -v '\\.\\./'"
    val topEntry = "tail -n 1"
    val (_, output, err) =
      runAndParse(
        adbShellCmd("ls", "-Rplart", "--full-time", path.escaped, "|", removeParentDirectories, "|", topEntry))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running mostRecentUpdate: " + err.mkString("\n"))
    }

    LsParser
      .parseFile(output.head)
      .map({
        case (_, mostRecentUpdate) =>
          logger.info(s"Most recent update inside $path: $mostRecentUpdate")
          mostRecentUpdate
      })
  }

  /**
    * Creates a [[Process]] to transfer the raw data from the given path.  The caller will then attach to the
    * [[Process]]'s stdout to receive the data.
    *
    * @param path the path of the file to transfer
    * @return the [[Process]] to transfer the data
    */
  def transferProcess(path: AbsolutePath): CLIProcess =
    CLIProcess(execOutCmd("cat", path.quoted))
}

object ADBCommandLine {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[ADBCommandLine])

  val UTF_16: java.nio.charset.Charset = Charset.forName("UTF-16")
}
