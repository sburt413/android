package com.hydrangea.process

import com.hydrangea.android.adb.ADBCommandLine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CLIProcessTest extends AnyFlatSpec {
  "Process" should "run" in {
    val args = List("adb")

    val factory: DefaultCLIProcessFactory = DefaultCLIProcessFactory.instance
    val process: CLIProcess = factory.create(args, expectedExitCodes = Some(Set(0, 1)))
    val (stdout, stderr) = process.createStreamHandlers()

    val stdoutReader = new ReaderThread(stdout, ADBCommandLine.UTF_8)
    val stderrReader = new ReaderThread(stderr, ADBCommandLine.UTF_8)

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
