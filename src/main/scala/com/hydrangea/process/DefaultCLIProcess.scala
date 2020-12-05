package com.hydrangea.process

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.Charset

import com.hydrangea.process.CLIProcess.WaitFor
import org.apache.commons.exec.{CommandLine, DefaultExecutor, ExecuteWatchdog, Executor, PumpStreamHandler}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.util.control.NonFatal

class DefaultCLIProcessFactory extends CLIProcessFactory {
  import com.hydrangea.process.DefaultCLIProcess._

  override def create(args: Seq[String], timeout: Option[Timeout], expectedExitCodes: Option[Set[Int]]): CLIProcess = {
    println(s"Building process for: $args")
    val commandLine: CommandLine = buildCommandLine(args)
    println(s"Commandline: ${commandLine.getArguments.mkString("Array(", ", ", ")")}")

    val executor = new DefaultExecutor
    expectedExitCodes.foreach(exitCodes => executor.setExitValues(exitCodes.toArray))

    timeout.foreach(maxRuntime => {
      val watchdog = new ExecuteWatchdog(maxRuntime.inMills)
      executor.setWatchdog(watchdog)
    })

    new DefaultCLIProcess(executor, commandLine)
  }
}

object DefaultCLIProcessFactory {
  def instance: DefaultCLIProcessFactory = new DefaultCLIProcessFactory

  def runAndParse(command: Seq[String],
                  timeout: Option[Timeout] = None,
                  charset: Charset): (Int, Seq[String], Seq[String]) = {
    val process: CLIProcess = instance.create(command, timeout)
    process.runAndParse(charset)
  }
}

class DefaultCLIProcess(executor: Executor, commandLine: CommandLine) extends CLIProcess {
  import DefaultCLIProcess._

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
    (DefaultCLIProcess.readerThread(stdout, charset), DefaultCLIProcess.readerThread(stderr, charset))
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
          logger.warn(s"NON ZERO EXIT VALUE ($exitValue) for ${commandLine.toString}")
        } else {
          logger.trace("NORMAL PROCESS TERMINATION")
        }

        executor.getStreamHandler.stop()

        exitValue
      } catch {
        case NonFatal(e) =>
          logger.warn(s"Exception running process (${commandLine.toString}).", e)
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

object DefaultCLIProcess {
  private[process] val logger: Logger = LoggerFactory.getLogger(CLIProcess.getClass)

  def readerThread(inputStream: InputStream, charset: Charset): ReaderThread = new ReaderThread(inputStream, charset)

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
