package com.hydrangea.file

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}

import com.hydrangea.DisjunctionOps._
import com.hydrangea.android.adb.ADBCommandLine
import com.hydrangea.process.CLIProcess
import org.apache.commons.io.IOUtils
import scalaz.Disjunction

object FileSystemService {
  type ReadLambda[A] = InputStream => A
  val blackhole: ReadLambda[Unit] = _ => ()

  def read[A](location: FileLocation)(readerFn: ReadLambda[A]): Disjunction[String, A] =
    location match {
      case localLocation: LocalFileLocation =>
        readerFn(Files.newInputStream(localLocation.toJavaPath)).toRightDisjunction
      case androidLocation: AndroidLocation =>
        readFromDevice(androidLocation)(readerFn)
    }

  def readFromDevice[A](androidLocation: AndroidLocation)(readStdout: ReadLambda[A]): Disjunction[String, A] = {
    val commandLine: ADBCommandLine = androidLocation.device.commandline()
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

  private case class DeviceReader[A](inputStream: InputStream, fn: ReadLambda[A]) extends Thread {
    var output: A = _

    override def run(): Unit = {
      output = fn(inputStream)
    }
  }
}
