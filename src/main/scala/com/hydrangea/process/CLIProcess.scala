package com.hydrangea.process

import java.io.{InputStream, PipedInputStream, PipedOutputStream, StringWriter}
import java.nio.charset.Charset

import com.hydrangea.android.adb.Timeout
import org.apache.commons.exec.{CommandLine, DefaultExecutor, ExecuteWatchdog, PumpStreamHandler}
import org.apache.commons.io.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.util.control.NonFatal

class CLIProcess(args: Seq[String],
                 timeout: Option[Timeout] = None,
                 expectedExitCodes: Option[Set[Int]] = Some(Set(0))) {
  import CLIProcess._

  println(s"Building process for: $args")
  val commandLine: CommandLine = buildCommandLine(args)
  println(s"Commandline: ${commandLine.getArguments.mkString("Array(", ", ", ")")}")

  val executor = new DefaultExecutor
  expectedExitCodes.foreach(exitCodes => executor.setExitValues(exitCodes.toArray))

  timeout.foreach(maxRuntime => {
    val watchdog = new ExecuteWatchdog(maxRuntime.inMills)
    executor.setWatchdog(watchdog)
  })

  /**
    * Creates a new pair of pipes for this process's stdout and stderr streams (respectively) and returns them as input
    * stream.  These should be created before the process is run.  Ideally, you may wish to read from these streams in
    * their own thread.
    *
    * @return {{InputStream}}s for the stdout and stderr of the process
    */
  def createStreamHandlers(): (InputStream, InputStream) = {
    val stdoutPipe = new PipedOutputStream()
    val stderrPipe = new PipedOutputStream()
    executor.setStreamHandler(new PumpStreamHandler(stdoutPipe, stderrPipe))

    (new PipedInputStream(stdoutPipe), new PipedInputStream(stderrPipe))
  }

  /**
    * Runs the process for the arguments asynchronously and returns a function that for the program and output to terminate and provides the return code.
    *
    * @return a function that waits for this process and output to terminate
    */
  def runAsync(): WaitFor = {
    logger.debug("APACHE EXECUTING COMMAND: " + commandLine.toString)
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

class ReaderThread(inputStream: InputStream, charset: Charset) extends Thread {
  private val pipeSize = 200 * 1024

  private val output = new StringWriter()

  override def run(): Unit = {
    output.write(IOUtils.toCharArray(inputStream, charset))
    inputStream.close()
  }

  def getOutput: String = output.toString
}

object CLIProcess {
  def apply(args: Seq[String],
            timeout: Option[Timeout] = None,
            expectedExitCodes: Option[Set[Int]] = Some(Set(0))): CLIProcess =
    new CLIProcess(args, timeout, expectedExitCodes)

  private[process] val logger: Logger = LoggerFactory.getLogger(CLIProcess.getClass)

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
}
