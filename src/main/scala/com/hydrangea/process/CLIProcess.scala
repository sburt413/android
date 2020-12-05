package com.hydrangea.process

import java.io.InputStream
import java.nio.charset.Charset

import com.hydrangea.process.CLIProcess.WaitFor

import scala.concurrent.duration.TimeUnit

case class Timeout(count: Long, units: TimeUnit) {
  def inMills: Long = units.toMillis(count)
}

trait CLIProcess {
  def runAsync(): WaitFor
  def run(): Int

  /**
    * Creates a new pair of pipes for this process's stdout and stderr streams (respectively) and returns them as input
    * stream.  These should be created before the process is run.  Ideally, you may wish to read from these streams in
    * their own thread.
    *
    * @return [[InputStream]]s for the stdout and stderr of the process
    */
  def createStreamHandlers(): (InputStream, InputStream)

  /**
    * Create a new pair of pipes for the process's stdout and stderr streams and creates [[ReaderThread]]s for both of
    * them.  These should be created before the process is run.  The caller must manually start these threads.
    *
    * @param charset charset to encode the output as
    * @return the [[ReaderThread]] for stdout and stderr (respectively)
    */
  def createReaderThreadHandlers(charset: Charset): (ReaderThread, ReaderThread)

  def runAndParse(charset: Charset): (Int, Seq[String], Seq[String]) = {
    val (stdoutReader, stderrReader) = createReaderThreadHandlers(charset)

    stdoutReader.start()
    stderrReader.start()
    val exitCode: Int = run()

    stdoutReader.join()
    stderrReader.join()

    stdoutReader.getOutput.split(System.lineSeparator())

    val stdout: Seq[String] = splitFilterLines(stdoutReader.getOutput)
    val stderr: Seq[String] = splitFilterLines(stderrReader.getOutput)
    (exitCode, stdout, stderr)
  }

  protected def splitFilterLines(str: String): Seq[String] = {
    val lines: Seq[String] = str.split(System.lineSeparator())
    if (lines.forall(_.isBlank)) {
      Nil
    } else {
      lines
    }
  }
}

trait CLIProcessFactory {
  def create(args: Seq[String],
             timeout: Option[Timeout] = None,
             expectedExitCodes: Option[Set[Int]] = Some(Set(0))): CLIProcess
}

object CLIProcess {
  type WaitFor = () => Int
}
