package com.hydrangea.android.adb

import java.io._
import java.nio.charset.Charset

import com.hydrangea.android.adb.ADBCommandLine.UTF_16
import org.apache.commons.exec.{CommandLine, DefaultExecutor, ExecuteWatchdog, PumpStreamHandler}
import org.apache.commons.io.output.ByteArrayOutputStream
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit
import scala.util.control.NonFatal

case class Timeout(count: Long, units: TimeUnit) {
  def inMills: Long = units.toMillis(count)
}

class ADBProcess(args: Seq[String],
                 stdoutBuilder: ADBProcessListenerBuilder = ADBProcessListener.blackhole,
                 stderrBuilder: ADBProcessListenerBuilder = ADBProcessListener.blackhole,
                 timeout: Option[Timeout] = None) {
  import ADBProcess._

  val commandLine: CommandLine = buildCommandLine(args)

  val stdoutPipe = new PipedOutputStream()
  val stderrPipe = new PipedOutputStream()

  val stdoutListener: ADBProcessListener = stdoutBuilder(new PipedInputStream(stdoutPipe))
  val stderrListener: ADBProcessListener = stderrBuilder(new PipedInputStream(stderrPipe))

  val executor = new DefaultExecutor
  executor.setExitValue(0)
  executor.setStreamHandler(new PumpStreamHandler(stdoutPipe, stderrPipe))
  timeout.foreach(maxRuntime => {
    val watchdog = new ExecuteWatchdog(maxRuntime.inMills)
    executor.setWatchdog(watchdog)
  })

  /**
    * Runs the process for the arguments asynchronously and returns a function that for the program and output to terminate and provides the return code.
    *
    * @return a function that waits for this process and output to terminate
    */
  def runAsync(): WaitFor = {
    logger.debug("APACHE EXECUTING COMMAND: " + commandLine.toString)
    stdoutListener.start()
    stderrListener.start()
    executor.getStreamHandler.start()

    val waitFor: WaitFor = () => {
      try {
        val exitValue: Int = executor.execute(commandLine)
        if (exitValue != 0) {
          logger.warn(s"NON ZERO EXIT VALUE ($exitValue) for $args")
        } else {
          logger.trace("NORMAL PROCESS TERMINATION")
        }

        executor.getStreamHandler.stop()

        stdoutListener.join()
        stderrListener.join()

        exitValue
      } catch {
        case NonFatal(e) =>
          logger.warn(s"Exception running process (${args.mkString(" ")}).", e)
          -999
      }
    }

    waitFor
  }

  def run(): Int = {
    val waitFor: WaitFor = runAsync()
    waitFor()
  }
}

object ADBProcess {
  private[adb] val logger: Logger = LoggerFactory.getLogger(ADBProcess.getClass)

  type WaitFor = () => Int

  def buildCommandLine(args: Seq[String]): CommandLine = {
    @tailrec
    def build(commandLine: CommandLine, args: Seq[String]): CommandLine =
      if (args.isEmpty) {
        commandLine
      } else {
        build(commandLine.addArgument(args.head, false), args.tail)
      }

    build(new CommandLine(args.head), args.tail)
  }

  def apply(args: Seq[String],
            stdoutBuilder: ADBProcessListenerBuilder,
            stderrBuilder: ADBProcessListenerBuilder,
            timeout: Option[Timeout] = None): ADBProcess =
    new ADBProcess(args, stdoutBuilder, stderrBuilder, timeout)

  private def runAndParse(timeout: Option[Timeout], args: String*): (Int, Seq[String], Seq[String]) = {
    //val charset: Charset = UTF_16
    val charset: Charset = Charset.defaultCharset()
    val stdoutBytes = new ByteArrayOutputStream()
    val stdoutBuilder: ADBProcessListenerBuilder =
      ADBProcessListener.pipedBuilder(new PrintStream(stdoutBytes, true, charset))

    val stderrBytes = new ByteArrayOutputStream()
    val stderrBuilder: ADBProcessListenerBuilder =
      ADBProcessListener.pipedBuilder(new PrintStream(stderrBytes, true, charset))

    val exitCode: Int = new ADBProcess(args, stdoutBuilder, stderrBuilder, timeout).run()
    val stdoutLines: Seq[String] = stdoutBytes.toString(charset).split("\r?\n")
    // Require at least one non-empty line for stderr to be considered nonempty
    val stderrLines: Seq[String] =
      Option(stderrBytes.toString(charset).split("\r?\n").toSeq).filter(_.exists(_.nonEmpty)).getOrElse(Nil)
    (exitCode, stdoutLines, stderrLines)
  }

  def runAndParse(timeout: Timeout, args: String*): (Int, Seq[String], Seq[String]) =
    runAndParse(Some(timeout), args: _*)

  def runAndParse(args: String*): (Int, Seq[String], Seq[String]) =
    runAndParse(None, args: _*)

}

abstract class ADBProcessListener(fromExec: InputStream) extends Thread {
  private val pipeSize = 200 * 1024
  override def run(): Unit = {
    onStart()

    val buffer: Array[Byte] = new Array(pipeSize)
    val fromExecBuffered = new BufferedInputStream(fromExec, pipeSize)
    LazyList.continually(fromExecBuffered.read(buffer)).takeWhile(_ != -1).foreach { readLength =>
      onRead(buffer, readLength)
    }

    fromExec.close()
    onClose()
  }

  def onStart(): Unit

  def onRead(buffer: Array[Byte], readLength: Int): Unit

  def onClose(): Unit
}

class ADBProcessPipe(fromExec: InputStream, outputStream: OutputStream) extends ADBProcessListener(fromExec) {
  override def onStart(): Unit = {}
  def onRead(buffer: Array[Byte], readLength: Int): Unit = outputStream.write(buffer, 0, readLength)
  def onClose(): Unit = outputStream.close()
}

trait ADBProcessListenerBuilder extends Function1[InputStream, ADBProcessListener]

object ADBProcessListener {
  import ADBProcess._

  def apply(onStartFn: () => Unit, onReadFn: (Array[Byte], Int) => Unit, onCloseFn: () => Unit)(
      fromExec: InputStream): ADBProcessListener = {
    new ADBProcessListener(fromExec) {
      override def onStart(): Unit = onStartFn()
      override def onRead(buffer: Array[Byte], readLength: Int): Unit = onReadFn(buffer, readLength)
      override def onClose(): Unit = onCloseFn()
    }
  }

  def pipedBuilder(outputStream: OutputStream): ADBProcessListenerBuilder = {
    def onRead(buffer: Array[Byte], readLength: Int): Unit = {
      outputStream.write(buffer, 0, readLength)
    }

    ADBProcessListener(() => {}, onRead, () => {})
  }

  def debuggingPipedBuilder(label: String, outputStream: OutputStream): ADBProcessListenerBuilder = {
    def onStart(): Unit = {
      logger.debug(s"Started reading $label.")
    }

    def onRead(buffer: Array[Byte], readLength: Int): Unit = {
      if (readLength > 0) {
        logger.debug(s"$label: " + new String(buffer, 0, readLength, UTF_16))
      }

      outputStream.write(buffer, 0, readLength)
    }

    def onClose(): Unit = {
      logger.debug(s"Finished reading $label.")
    }

    ADBProcessListener(onStart, onRead, onClose)
  }

  val blackhole: ADBProcessListenerBuilder = ADBProcessListener(() => {}, (_, _) => {}, () => {})
}
