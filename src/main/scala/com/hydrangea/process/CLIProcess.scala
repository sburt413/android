package com.hydrangea.process

import java.io.{InputStream, PipedInputStream, PipedOutputStream, StringWriter}
import java.nio.charset.Charset

import org.apache.commons.exec.{CommandLine, DefaultExecutor, ExecuteWatchdog, PumpStreamHandler}
import org.apache.commons.io.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit
import scala.util.control.NonFatal

case class Timeout(count: Long, units: TimeUnit) {
  def inMills: Long = units.toMillis(count)
}

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
    * @return [[InputStream]]s for the stdout and stderr of the process
    */
  def createStreamHandlers(): (InputStream, InputStream) = {
    val stdoutPipe = new PipedOutputStream()
    val stderrPipe = new PipedOutputStream()
    executor.setStreamHandler(new PumpStreamHandler(stdoutPipe, stderrPipe))

    (new PipedInputStream(stdoutPipe), new PipedInputStream(stderrPipe))
  }

  /**
    * Create a new pair of pipes for the process's stdout and stderr streams and creates [[ReaderThread]]s for both of
    * them.  These should be created before the process is run.  The caller must manually start these threads.
    *
    * @param charset charset to encode the output as
    * @return the [[ReaderThread]] for stdout and stderr (respectively)
    */
  def createReaderThreadHandlers(charset: Charset): (ReaderThread, ReaderThread) = {
    val (stdout, stderr) = createStreamHandlers()
    (CLIProcess.readerThread(stdout, charset), CLIProcess.readerThread(stderr, charset))
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
  private val output = new StringWriter()

  override def run(): Unit = {
    output.write(IOUtils.toCharArray(inputStream, charset))
    inputStream.close()
  }

  def getOutput: String = output.toString
}

object CLIProcess {
  private[process] val logger: Logger = LoggerFactory.getLogger(CLIProcess.getClass)

  def apply(args: Seq[String],
            timeout: Option[Timeout] = None,
            expectedExitCodes: Option[Set[Int]] = Some(Set(0))): CLIProcess =
    new CLIProcess(args, timeout, expectedExitCodes)

  def readerThread(inputStream: InputStream, charset: Charset): ReaderThread = new ReaderThread(inputStream, charset)

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

  def runAndParse(command: Seq[String], timeout: Timeout, charset: Charset): (Int, Seq[String], Seq[String]) =
    runAndParse(command, Some(timeout), charset)

  def runAndParse(command: Seq[String],
                  timeout: Option[Timeout] = None,
                  charset: Charset): (Int, Seq[String], Seq[String]) = {
    val process: CLIProcess = CLIProcess(command, timeout)

    val (stdoutReader, stderrReader) = process.createReaderThreadHandlers(charset)

    stdoutReader.start()
    stderrReader.start()
    val exitCode: Int = process.run()

    stdoutReader.join()
    stderrReader.join()

    stdoutReader.getOutput.split(System.lineSeparator())

    val stdout: Seq[String] = splitFilterLines(stdoutReader.getOutput)
    val stderr: Seq[String] = splitFilterLines(stderrReader.getOutput)
    (exitCode, stdout, stderr)
  }

  private def splitFilterLines(str: String): Seq[String] = {
    val lines: Seq[String] = str.split(System.lineSeparator())
    if (lines.forall(_.isBlank)) {
      Nil
    } else {
      lines
    }
  }
}
