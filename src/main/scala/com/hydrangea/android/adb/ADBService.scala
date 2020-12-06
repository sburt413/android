package com.hydrangea.android.adb

import java.nio.charset.Charset
import java.time.Instant
import java.util.concurrent.TimeUnit

import com.google.inject.Inject
import com.hydrangea.android.adb.find.{FindDepth, FindOption, ForDirectories, ForRegularFiles}
import com.hydrangea.android.adb.ls.LsParser
import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, AndroidDirectoryData, AndroidFileData, AndroidLocation, AndroidRegularFileData}
import com.hydrangea.process.{CLIProcess, CLIProcessFactory, Timeout}
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
  def commandline(cliProcessFactory: CLIProcessFactory,
                  timeout: Timeout = Device.defaultTimeout,
                  charset: Charset = Charset.defaultCharset()): ADBCommandLine =
    new ADBCommandLine(cliProcessFactory, this, timeout, charset)

  /**
    * Executes the given function, passing it a commandline.
    *
    * @param timeout the timeout for any commands run on the commandline
    * @param fn the function to execute using the terminal
    * @tparam A the type of output from the function
    * @return the result of running the function
    */
  def withCommandLine[A](cliProcessFactory: CLIProcessFactory,
                         timeout: Timeout = Device.defaultTimeout,
                         charset: Charset = Charset.defaultCharset())(fn: ADBCommandLine => A): A =
    fn(new ADBCommandLine(cliProcessFactory, this, timeout, charset))
}

object Device {
  val defaultTimeout: Timeout = Timeout(10, TimeUnit.MINUTES)
}

/**
  * The entry point for accessing devices for ADB (Android Debug Bridge).
  */
class ADBService @Inject()(cliProcessFactory: CLIProcessFactory) {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[Device])

  def commandLine(device: Device,
                  timeout: Timeout = Device.defaultTimeout,
                  charset: Charset = Charset.defaultCharset()): ADBCommandLine =
    new ADBCommandLine(cliProcessFactory, device, timeout, charset)

  /**
    * Executes the given function, passing it a commandline.
    *
    * @param timeout the timeout for any commands run on the commandline
    * @param fn the function to execute using the terminal
    * @tparam A the type of output from the function
    * @return the result of running the function
    */
  def withCommandLine[A](device: Device,
                         timeout: Timeout = Device.defaultTimeout,
                         charset: Charset = Charset.defaultCharset())(fn: ADBCommandLine => A): A =
    fn(commandLine(device, timeout, charset))

  def devices: Seq[Device] = {
    val (_, stdout, _) =
      cliProcessFactory
        .create(Seq("adb", "devices"), Some(Timeout(10, TimeUnit.SECONDS)))
        .runAndParse(Charset.defaultCharset())

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

object ADBService {
  def apply(cliProcessFactory: CLIProcessFactory): ADBService =
    new ADBService(cliProcessFactory)
}

/**
  * An abstraction over running `adb`.  Most commands are bashed on underlying shell commands, with built in processing
  * of the results.
  *
  * @param device  the device to run the commands on
  * @param timeout the maximum runtime for a single underlying command
  */
class ADBCommandLine(cliProcessFactory: CLIProcessFactory, val device: Device, timeout: Timeout, charset: Charset) {
  import ADBCommandLine.logger

  private val adbExec: Seq[String] = Seq("adb", "-s", device.serial)

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

  /**
    * Creates a new process, runs the process and returns the result parsed into lines.
    *
    * @param command the command to run
    * @return the return code, stdout line and stderr lines from the running command
    */
  private def runAndParse(command: Seq[String]): (Int, Seq[String], Seq[String]) =
    cliProcessFactory
      .create(command, Some(timeout))
      .runAndParse(charset)

  /**
    * Runs an underlying `ls` command and returns the output as files.
    *
    * @param directoryPath the path to the directory to list
    * @return the files immediately in the directory
    */
  def list(directoryPath: AbsolutePath): Seq[AndroidFileData] = {
    val (_, listing, err) = runAndParse(adbShellCmd("ls", "-pla", "--full-time", directoryPath.commandLine))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `list`: " + err.mkString("\n"))
    }

    for {
      (fileName, isDirectory, lastModified) <- LsParser.parseDirectory(listing)
    } yield {
      val path: AbsolutePath = directoryPath ++ fileName
      val location = AndroidLocation(device, path)
      if (isDirectory) {
        AndroidDirectoryData(location, lastModified)
      } else {
        AndroidRegularFileData(location, lastModified)
      }
    }
  }

  /**
    * Run and underlying recursive `ls` coomand and returns all contained files as output.
    *
    * @param directoryPath the path to the directory to list
    * @return all files and folders in the given folder's directory tree
    */
  def listRecursive(directoryPath: AbsolutePath): Seq[AndroidFileData] = {
    val (_, listings, err) = runAndParse(adbShellCmd("ls", "-Rpla", "--full-time", directoryPath.commandLine))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running recursive `list`: " + err.mkString("\n"))
    }

    for {
      (path, isDirectory, lastModified) <- LsParser.parseDirectories(listings)
    } yield {
      val location = AndroidLocation(device, path)
      if (isDirectory) {
        AndroidDirectoryData(location, lastModified)
      } else {
        AndroidRegularFileData(location, lastModified)
      }
    }
  }

  /**
    * Runs an underlying `find` command with the given options, returning the found values as paths.
    *
    * @param directoryPath the path to the directory to find from
    * @param option0 the criteria to apply to the find command
    * @param options any additional criteria to add to the find command
    * @return all paths found by the command
    */
  def find(directoryPath: AbsolutePath, option0: FindOption, options: FindOption*): Seq[AbsolutePath] =
    findCmd(directoryPath, (Seq(option0) ++ options).flatMap(opt => Seq(opt.param, opt.value)))

  /**
    * Runs a find command with directories type already applied.
    *
    * @param directoryPath the path to the directory to find from
    * @param options any additional criteria to add to the find command
    * @return all paths found by the command
    */
  def findDirectories(directoryPath: AbsolutePath, options: FindOption*): Seq[AbsolutePath] =
    find(directoryPath, ForDirectories, options: _*)

  /**
    * Runs a find command with regular files type already applied.
    *
    * @param directoryPath the path to the directory to find from
    * @return all paths found by the command
    */
  def findRegularFiles(directoryPath: AbsolutePath): Seq[AbsolutePath] =
    find(directoryPath, ForRegularFiles)

  /**
    * Runs the `find` command in an ADB shell and parses [[AbsolutePath]]s from the output.
    *
    * @param directoryPath the path to the directory to find from
    * @param params        the commandline parameters to apply
    * @return [[AbsolutePath]]s found by the shell command
    */
  private def findCmd(directoryPath: AbsolutePath, params: Seq[String]): Seq[AbsolutePath] = {
    val args: Seq[String] = Seq("find") ++ Seq(directoryPath.commandLine) ++ params
    val (_, found, err) = runAndParse(adbShellCmd(args: _*))
    if (err.nonEmpty) {
      throw new RuntimeException("Error running `find`: " + err.mkString("\n"))
    }

    found.map(_.toUnixPath)
  }

  /**
    * Runs file status (stat) on a given [[AbsolutePath]] and returns the [[AndroidFile]] found.
    *
    * @param path the path to check
    * @return the file, if present; otherwise [[None]] if no file is found
    */
  def stat(path: AbsolutePath): Option[AndroidFileData] = {
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

        val location: AndroidLocation = AndroidLocation(device, path)
        if (fileType == "directory") {
          AndroidDirectoryData(location, modifiedTime)
        } else {
          AndroidRegularFileData(location, modifiedTime)
        }
      })
  }

  /**
    * Runs stat, considering the absence of the file to be an exception.
    *
    * @param path the path to check
    * @return the file, if present; otherwise throws an error
    */
  def statOrThrow(path: AbsolutePath): AndroidFileData =
    stat(path).getOrElse(throw new IllegalStateException(s"No file found for path: $path"))

  /**
    * Runs a hash on the file at the given path using sha1.
    *
    * @param path the path to the file to hash
    * @return the sha1 hash of the file
    */
  def sha1sum(path: AbsolutePath): String = {
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
  def scan(path: AbsolutePath): Seq[AndroidFileData] = {
    val directoryPaths: Seq[AbsolutePath] = findDirectories(path, FindDepth(1))
    directoryPaths.indices.flatMap { index =>
      val directoryPath: AbsolutePath = directoryPaths(index)
      if ((index + 1) % 10 == 0 || index == 0 || index == directoryPaths.size - 1) {
        logger.info(s"Scanning: ${index + 1}/${directoryPaths.size}")
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
  def mostRecentUpdate(path: AbsolutePath): Option[Instant] = {
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
        case (_, _, mostRecentUpdate) =>
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
    cliProcessFactory.create(execOutCmd("cat", path.quoted), Some(timeout))
}

object ADBCommandLine {
  private[adb] val logger: Logger = LoggerFactory.getLogger(classOf[ADBCommandLine])

  val UTF_16: java.nio.charset.Charset = Charset.forName("UTF-16")
}
