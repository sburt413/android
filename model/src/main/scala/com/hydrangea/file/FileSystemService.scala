package com.hydrangea.file

import java.io.{IOException, InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.time.Instant

import com.google.inject.{AbstractModule, Inject}
import com.hydrangea.DisjunctionOps._
import com.hydrangea.android.adb.{ADBCommandLine, ADBService}
import com.hydrangea.file.FileSystemService.ReadLambda
import com.hydrangea.process.CLIProcess
import net.codingwell.scalaguice.ScalaModule
import org.apache.commons.io.IOUtils
import scalaz.{-\/, Disjunction, \/-}

import scala.jdk.StreamConverters._
import scala.reflect.ClassTag

/**
  * A service that can read file data from an arbitrary [[FileLocation]].
  */
trait FileSystemService {

  /**
    * Read the content of the file at the given {{location}} and processes it with the given [[ReadLambda]].
    *
    * @param location the location of the file to read
    * @param readerFn the method to process the file data
    * @tparam A the resulting type of the function
    * @return the result of {{readerFn}} applied to the file
    */
  def read[A](location: FileLocation)(readerFn: ReadLambda[A]): Disjunction[String, A]

  /**
    * Copies the contents of the file at the given location and writes it to the given path on the local filesystem.
    *
    * @param location   the location of the file to copy from
    * @param targetPath the path on the local file system to copy to
    */
  def copyFromDevice(location: FileLocation, targetPath: AbsolutePath): Unit = {
    val result: Disjunction[String, Int] = read(location) { inputStream =>
      val targetJavaPath: Path = Path.of(targetPath.raw)
      val outputStream: OutputStream = Files.newOutputStream(targetJavaPath)
      IOUtils.copy(inputStream, outputStream)
    }

    result match {
      case \/-(_)   => ()
      case -\/(msg) => throw new IOException(msg)
    }
  }

  /**
    * Write the given {{data}} to the given [[FileLocation]].
    *
    * @param location the [[FileLocation]] to write to
    * @param data the content to write
    */
  def write(location: FileLocation, data: Array[Byte]): Unit

  /**
    * Returns [[FileData]] for all files found directly under the given [[FileLocation]].
    *
    * @param location the [[FileLocation]] to list all child files for
    * @return [[FileData]] for all files found directly under the given [[FileLocation]]
    */
  def list(location: FileLocation): Seq[FileData]

  /**
    * Returns all ancestor [[FileData]] for all files found under the given [[FileLocation]].
    *
    * @param location the [[FileLocation]] to list all ancestor files for
    * @return all ancestor [[FileData]] for all files found under the given [[FileLocation]]
    */
  def scan(location: FileLocation): Seq[FileData]

  /**
    * Returns all locations of all files under the given [[FileLocation]].
    *
    * @param location the [[FileLocation]] to list all ancestor locations for
    * @tparam L the type of location
    * @return all locations of all files under the given [[FileLocation]]
    */
  def ancestorLocations[L <: FileLocation](location: L)(implicit classTag: ClassTag[L]): Seq[L]

  /**
    * Returns a count of all ancestor regular files found under the given [[FileLocation]].  This method will take
    * advantage of commands on the target file system to be more efficient than just a filtered [[scan]].
    *
    * @param location the [[FileLocation]] to count ancestor regular files under
    * @return a count of all regular files found under the given [[FileLocation]]
    */
  def regularFileCount(location: FileLocation): Int

  /**
    * Returns the newest most recent update time found under any ancestor file under the given [[FileLocation]].  This
    * method will take advantage of commands on the target file system to be more efficient than just a [[scan]].
    *
    * @param location the [[FileLocation]] to count ancestor regular files under
    * @return a count of all regular files found under the given [[FileLocation]]
    */
  def mostRecentUpdate(location: FileLocation): Option[Instant]
}

class FileSystemServiceImpl @Inject()(adbService: ADBService) extends FileSystemService {
  import FileSystemService._

  def read[A](location: FileLocation)(readerFn: ReadLambda[A]): Disjunction[String, A] =
    location match {
      case localLocation: LocalFileLocation =>
        if (Files.exists(localLocation.toJavaPath)) {
          readerFn(Files.newInputStream(localLocation.toJavaPath)).toRightDisjunction
        } else {
          "No such file".toLeftDisjunction
        }
      case androidLocation: AndroidLocation =>
        readFromDevice(androidLocation)(readerFn)
    }

  private def readFromDevice[A](androidLocation: AndroidLocation)(readStdout: ReadLambda[A]): Disjunction[String, A] = {
    val commandLine: ADBCommandLine = adbService.commandLine(androidLocation.device)
    val process: CLIProcess = commandLine.transferProcess(androidLocation.path)
    val (stdout, stderr) = process.createStreamHandlers()

    val stdoutReader: DeviceReader[A] = DeviceReader(stdout, readStdout)
    val stderrReader: DeviceReader[Unit] = DeviceReader(stderr, blackhole)

    stdoutReader.start()
    stderrReader.start()
    val exitValue: Int = process.run()

    stdoutReader.join(10000)
    stderrReader.join(10000)

    if (exitValue != 0) {
      s"Error reading from device: $exitValue".toLeftDisjunction
    } else {
      stdoutReader.output.toRightDisjunction
    }
  }

  def write(location: FileLocation, data: Array[Byte]): Unit =
    location match {
      case LocalFileLocation(path) => {
        val javaPath: Path = path.toJavaPath
        Files.createDirectories(javaPath.getParent)
        Files.write(javaPath, data)
      }
      case AndroidLocation(device, path) => ???
    }

  def list(location: FileLocation): Seq[FileData] =
    location match {
      case LocalFileLocation(path)       => listLocal(path)
      case AndroidLocation(device, path) => adbService.commandLine(device).list(path)
    }

  private def listLocal(path: AbsolutePath): Seq[FileData] =
    Files
      .list(path.toJavaPath)
      .toScala(LazyList)
      .flatMap(javaPath => LocalFileData(javaPath))

  def scan(location: FileLocation): Seq[FileData] =
    location match {
      case LocalFileLocation(path)       => scanLocal(path)
      case AndroidLocation(device, path) => adbService.commandLine(device).scan(path)
    }

  private def scanLocal(path: AbsolutePath): Seq[LocalFileData] =
    Files
      .walk(path.toJavaPath)
      .toScala(LazyList)
      .flatMap(javaPath => LocalFileData(javaPath))

  def ancestorLocations[L <: FileLocation](location: L)(implicit classTag: ClassTag[L]): Seq[L] =
    location match {
      case LocalFileLocation(path) => localAncestorLocations(path).flatMap(_.to[L])
      case AndroidLocation(device, path) =>
        adbService
          .commandLine(device)
          .find(path)
          .map(ancestorPath => AndroidLocation(device, ancestorPath))
          .flatMap(_.to[L])
    }

  private def localAncestorLocations(path: AbsolutePath): Seq[LocalFileLocation] =
    Files
      .walk(path.toJavaPath)
      .toScala(LazyList)
      .flatMap(javaPath => LocalFileLocation(javaPath))

  def regularFileCount(location: FileLocation): Int =
    location match {
      case LocalFileLocation(path)       => countLocalFiles(path)
      case AndroidLocation(device, path) => adbService.commandLine(device).countFiles(path)
    }

  private def countLocalFiles(path: AbsolutePath): Int =
    Files.list(path.toJavaPath).filter(Files.isRegularFile(_)).count().toInt

  def mostRecentUpdate(location: FileLocation): Option[Instant] =
    location match {
      case LocalFileLocation(path)       => mostRecentLocalUpdate(path)
      case AndroidLocation(device, path) => adbService.commandLine(device).mostRecentUpdate(path)
    }

  private def mostRecentLocalUpdate(path: AbsolutePath): Option[Instant] = {
    val javaPath: Path = path.toJavaPath
    val fileTimes: Seq[Instant] =
      Files
        .list(javaPath)
        .map(Files.getLastModifiedTime(_))
        .map(_.toInstant)
        .toScala(Seq)

    val folderTime: Instant = Files.getLastModifiedTime(javaPath).toInstant
    val times: Seq[Instant] = fileTimes :+ folderTime
    val ordering: Ordering[Instant] = (lhs: Instant, rhs: Instant) => lhs.compareTo(rhs)
    val mostRecentUpdate: Instant = times.max(ordering)
    Some(mostRecentUpdate)
  }

  private case class DeviceReader[A](inputStream: InputStream, fn: ReadLambda[A]) extends Thread {
    var output: A = _

    override def run(): Unit = {
      output = fn(inputStream)
    }
  }
}

object FileSystemService {
  type ReadLambda[A] = InputStream => A
  val blackhole: ReadLambda[Unit] = _ => ()
}

object DefaultFileSystemServiceModule extends AbstractModule with ScalaModule {
  override def configure(): Unit = {
    bind[FileSystemService].to(classOf[FileSystemServiceImpl])
  }
}
