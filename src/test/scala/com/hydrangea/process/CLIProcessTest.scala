package com.hydrangea.process

import java.nio.charset.Charset

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CLIProcessTest extends AnyFlatSpec {
  "Process" should "run" in {
    val args = List("adb")
    val process: CLIProcess = CLIProcess(args, expectedExitCodes = Some(Set(0, 1)))
    val (stdout, stderr) = process.createStreamHandlers()

    val stdoutReader = new ReaderThread(stdout, Charset.defaultCharset())
    val stderrReader = new ReaderThread(stderr, Charset.defaultCharset())

    stdoutReader.start()
    stderrReader.start()

    val errorCode: Int = process.run()

    stdoutReader.join()
    stderrReader.join()

    errorCode should be(1)
    stdoutReader.getOutput should not be Symbol("empty")
    stderrReader.getOutput should be(Symbol("empty"))
  }
}
