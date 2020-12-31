package com.hydrangea.file

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.time.Instant

import com.google.inject.Inject
import com.hydrangea.DisjunctionOps._
import com.hydrangea.android.adb.ADBCommandLine
import com.hydrangea.process.{CLIProcess, CLIProcessFactory}
import org.apache.commons.io.IOUtils
import scalaz.Disjunction

import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

class FileSystemService @Inject()(cliProcessFactory: CLIProcessFactory) {
  import FileSystemService._

  def read[A](location: FileLocation)(readerFn: ReadLambda[A]): Disjunction[String, A] =
    location match {
      case localLocation: LocalFileLocation =>
        readerFn(Files.newInputStream(localLocation.toJavaPath)).toRightDisjunction
      case androidLocation: AndroidLocation =>
        readFromDevice(androidLocation)(readerFn)
    }

  def readFromDevice[A](androidLocation: AndroidLocation)(readStdout: ReadLambda[A]): Disjunction[String, A] = {
    val commandLine: ADBCommandLine = androidLocation.device.commandline(cliProcessFactory)
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

  def copyFromDevice(androidLocation: AndroidLocation, targetPath: AbsolutePath): Unit =
    readFromDevice(androidLocation) { inputStream =>
      val targetJavaPath: Path = Path.of(targetPath.raw)
      val outputStream: OutputStream = Files.newOutputStream(targetJavaPath)
      System.out.println("Writing to: " + targetJavaPath)
      IOUtils.copy(inputStream, outputStream)
    }

  def write(location: FileLocation, data: Array[Byte]): Unit =
    location match {
      case LocalFileLocation(path)       => Files.write(path.toJavaPath, data)
      case AndroidLocation(device, path) => ???
    }

  def listLocal(path: AbsolutePath): Seq[FileData] =
    Files
      .list(path.toJavaPath)
      .toScala(LazyList)
      .flatMap(javaPath => LocalFileData(javaPath))

  def list(location: FileLocation): Seq[FileData] =
    location match {
      case LocalFileLocation(path)       => listLocal(path)
      case AndroidLocation(device, path) => device.commandline(cliProcessFactory).list(path)
    }

  def scan(location: FileLocation): Seq[FileData] =
    location match {
      case LocalFileLocation(path)       => scanLocal(path)
      case AndroidLocation(device, path) => device.commandline(cliProcessFactory).scan(path)
    }

  def scanLocal(path: AbsolutePath): Seq[LocalFileData] =
    Files
      .walk(path.toJavaPath)
      .toScala(LazyList)
      .flatMap(javaPath => LocalFileData(javaPath))

  def fileCount(location: FileLocation): Int =
    location match {
      case LocalFileLocation(path)       => countLocalFiles(path)
      case AndroidLocation(device, path) => device.commandline(cliProcessFactory).countFiles(path)
    }

  private def countLocalFiles(path: AbsolutePath): Int =
    Files.list(path.toJavaPath).filter(Files.isRegularFile(_)).count().toInt

  def mostRecentUpdate(location: FileLocation): Option[Instant] =
    location match {
      case LocalFileLocation(path)       => mostRecentLocalUpdate(path)
      case AndroidLocation(device, path) => device.commandline(cliProcessFactory).mostRecentUpdate(path)
    }

  private def mostRecentLocalUpdate(path: AbsolutePath): Option[Instant] =
    Files
      .list(path.toJavaPath)
      .map(Files.getLastModifiedTime(_))
      .map(_.toInstant)
      .max(_.compareTo(_))
      .toScala

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
