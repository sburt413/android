package com.hydrangea.android.adb

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.Charset
import java.time.Instant

import com.hydrangea.android.adb.find.ByName
import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, AndroidDirectoryData, AndroidFileData, AndroidLocation, AndroidRegularFileData}
import com.hydrangea.process.CLIProcess.WaitFor
import com.hydrangea.process.{CLIProcess, CLIProcessFactory, ReaderThread, Timeout}
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.RandomStringUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.ListBuffer

class ADBServiceTest extends AnyFlatSpec {

  "ADBCommandLine" should "run stat on regular files" in {
    /*
      $ adb -d shell "stat -c '%Y %F' /etc/hosts"
      1230735600 regular file
     */
    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(
        MockCLIProcess("adb -s 123456789 shell stat -c '%Y %F' '/etc/hosts'",
                       "1230735600 regular file".getBytes(ADBCommandLine.UTF_8)))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/etc/hosts".toUnixPath
    val result: Option[AndroidFileData] = commandLine.stat(path)

    val fileTime = Instant.ofEpochSecond(1230735600)
    val expected = AndroidRegularFileData(AndroidLocation(fakeDevice, path), fileTime)
    result should equal(Some(expected))
  }

  it should "run stat on directories" in {
    /*
     $ adb -d shell "stat -c '%Y %F' /etc"
     1230735600 directory
     */
    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(
        MockCLIProcess("adb -s 123456789 shell stat -c '%Y %F' '/etc'",
                       "1230735600 directory".getBytes(ADBCommandLine.UTF_8)))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    // Trailing / gets sanitized
    val path: AbsolutePath = "/etc/".toUnixPath
    val result: Option[AndroidFileData] = commandLine.stat(path)

    val fileTime = Instant.ofEpochSecond(1230735600)
    val expected = AndroidDirectoryData(AndroidLocation(fakeDevice, path), fileTime)
    result should equal(Some(expected))
  }

  it should "run list on directories" in {
    /*
     $ adb -d shell ls -pla --full-time '/test/dir-1'
     */

    val stdout: Array[Byte] =
      """total 27
        |drwxrwx--x 5 root sdcard_rw 3488 2014-06-06 03:00:00.000000000 -0500 ./
        |drwxrwx--x 4 root sdcard_rw 3488 2015-07-06 04:00:00.000000000 -0500 ../
        |drwxrwx--x 2 root sdcard_rw 3488 2016-08-06 05:00:00.000000000 -0500 dir-1-1/
        |drwxrwx--x 3 root sdcard_rw 3488 2017-09-06 06:00:00.000000000 -0500 dir-1-2/
        |drwxrwx--x 2 root sdcard_rw 3488 2018-10-06 07:00:00.000000000 -0500 dir-1-3/
        |-rw-rw---- 1 root sdcard_rw   27 2019-11-06 08:00:00.000000000 -0500 dir-1-listing.txt
        |-rw-rw---- 1 root sdcard_rw 5028 2020-12-06 09:00:00.000000000 -0500 info-1.txt""".stripMargin.getBytes(
        ADBCommandLine.UTF_8)

    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(MockCLIProcess("adb -s 123456789 shell ls -pla --full-time '/test/dir-1'", stdout))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val result: Seq[AndroidFileData] = commandLine.list("/test/dir-1".toUnixPath)

    val first = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-1".toUnixPath),
                                     Instant.parse("2016-08-06T05:00:00-05:00"))
    val second = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2".toUnixPath),
                                      Instant.parse("2017-09-06T06:00:00-05:00"))
    val third = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-3".toUnixPath),
                                     Instant.parse("2018-10-06T07:00:00-05:00"))
    val fourth = AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-listing.txt".toUnixPath),
                                        Instant.parse("2019-11-06T08:00:00-05:00"))
    val fifth = AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/info-1.txt".toUnixPath),
                                       Instant.parse("2020-12-06T09:00:00-05:00"))
    result should equal(Seq(first, second, third, fourth, fifth))
  }

  it should "run recursive list on directories" in {
    /*
     $ adb -d shell ls -Rpla --full-time '/test'
     */

    val stdout: Array[Byte] =
      """/test:
        |total 12
        |drwxrwx--x  4 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 23 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |drwxrwx--x  5 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 dir-1/
        |drwxrwx--x  2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 dir-2/
        |-rw-rw----  1 root sdcard_rw   14 2020-12-06 09:00:00.000000000 -0500 root.txt
        |
        |/test/dir-1:
        |total 27
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 4 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 dir-1-1/
        |drwxrwx--x 3 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 dir-1-2/
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 dir-1-3/
        |-rw-rw---- 1 root sdcard_rw   27 2020-12-06 09:00:00.000000000 -0500 dir-1-listing.txt
        |-rw-rw---- 1 root sdcard_rw 5028 2020-12-06 09:00:00.000000000 -0500 info-1.txt
        |
        |/test/dir-1/dir-1-1:
        |total 6
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |
        |/test/dir-1/dir-1-2:
        |total 21
        |drwxrwx--x 3 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |-rw-rw---- 1 root sdcard_rw 1404 2020-12-06 09:00:00.000000000 -0500 Hamlet.txt
        |-rw-rw---- 1 root sdcard_rw 1459 2020-12-06 09:00:00.000000000 -0500 JuliusCaesar.txt
        |-rw-rw---- 1 root sdcard_rw  755 2020-12-06 09:00:00.000000000 -0500 Tempest.html
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 dir-1-2-1/
        |
        |/test/dir-1/dir-1-2/dir-1-2-1:
        |total 14
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 3 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |-rw-rw---- 1 root sdcard_rw 5028 2020-12-06 09:00:00.000000000 -0500 info-1-2-1.txt
        |
        |/test/dir-1/dir-1-3:
        |total 6
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |
        |/test/dir-2:
        |total 14
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 4 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |-rw-rw---- 1 root sdcard_rw 5028 2020-12-06 09:00:00.000000000 -0500 info-2.txt
        |""".stripMargin.getBytes(ADBCommandLine.UTF_8)

    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(MockCLIProcess("adb -s 123456789 shell ls -Rpla --full-time '/test'", stdout))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val results: Seq[AndroidFileData] = commandLine.listRecursive("/test".toUnixPath)

    val dirTime: Instant = Instant.parse("2020-12-05T08:00:00-05:00")
    val fileTime: Instant = Instant.parse("2020-12-06T09:00:00-05:00")
    val expected1 = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1".toUnixPath), dirTime)
    val expected2 = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-2".toUnixPath), dirTime)
    val expected3 = AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/root.txt".toUnixPath), fileTime)
    val expected4 = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-1".toUnixPath), dirTime)
    val expected5 = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2".toUnixPath), dirTime)
    val expected6 = AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-3".toUnixPath), dirTime)
    val expected7 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-listing.txt".toUnixPath), fileTime)
    val expected8 = AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/info-1.txt".toUnixPath), fileTime)
    val expected9 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/Hamlet.txt".toUnixPath), fileTime)
    val expected10 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/JuliusCaesar.txt".toUnixPath), fileTime)
    val expected11 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/Tempest.html".toUnixPath), fileTime)
    val expected12 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/dir-1-2-1".toUnixPath), dirTime)
    val expected13 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt".toUnixPath),
                             fileTime)
    val expected14 = AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-2/info-2.txt".toUnixPath), fileTime)
    results should equal(
      Seq(expected1,
          expected2,
          expected3,
          expected4,
          expected5,
          expected6,
          expected7,
          expected8,
          expected9,
          expected10,
          expected11,
          expected12,
          expected13,
          expected14))
  }

  it should "run find" in {
    /*
      $ adb -s 123456789 shell find '/test' -name info-\*.txt
      /test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt
      /test/dir-1/info-1.txt
      /test/dir-2/info-2.txt
     */
    val stdout: Array[Byte] =
      """/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt
        |/test/dir-1/info-1.txt
        |/test/dir-2/info-2.txt""".stripMargin.getBytes(ADBCommandLine.UTF_8)

    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(MockCLIProcess("adb -s 123456789 shell find '/test' -name info-\\*.txt", stdout))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/test".toUnixPath
    val results: Seq[AbsolutePath] = commandLine.find(path, ByName("info-*.txt"))

    val first: AbsolutePath = "/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt".toUnixPath
    val second: AbsolutePath = "/test/dir-1/info-1.txt".toUnixPath
    val third: AbsolutePath = "/test/dir-2/info-2.txt".toUnixPath
    results should equal(Seq(first, second, third))
  }

  it should "run findRegularFiles" in {
    val stdout: Array[Byte] =
      """/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt
        |/test/dir-1/info-1.txt
        |/test/dir-2/info-2.txt""".stripMargin.getBytes(ADBCommandLine.UTF_8)

    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(MockCLIProcess("adb -s 123456789 shell find '/test' -type f", stdout))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/test".toUnixPath
    val results: Seq[AbsolutePath] = commandLine.findRegularFiles(path)

    val first: AbsolutePath = "/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt".toUnixPath
    val second: AbsolutePath = "/test/dir-1/info-1.txt".toUnixPath
    val third: AbsolutePath = "/test/dir-2/info-2.txt".toUnixPath
    results should equal(Seq(first, second, third))
  }

  it should "run findDirectories" in {
    val stdout: Array[Byte] =
      """/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt
        |/test/dir-1/info-1.txt
        |/test/dir-2/info-2.txt""".stripMargin.getBytes(ADBCommandLine.UTF_8)

    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(MockCLIProcess("adb -s 123456789 shell find '/test' -type d", stdout))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/test".toUnixPath
    val results: Seq[AbsolutePath] = commandLine.findDirectories(path)

    val first: AbsolutePath = "/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt".toUnixPath
    val second: AbsolutePath = "/test/dir-1/info-1.txt".toUnixPath
    val third: AbsolutePath = "/test/dir-2/info-2.txt".toUnixPath
    results should equal(Seq(first, second, third))
  }

  it should "run most recent update" in {
    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(
        MockCLIProcess(
          "adb -s 123456789 shell ls -Rplart --full-time \"/test\" | grep -v '\\.\\./' | tail -n 1",
          "drwxrwx--x 2 root sdcard_rw 3488 2020-12-06 09:00:00.000000000 -0500 ./".getBytes(ADBCommandLine.UTF_8)
        ))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/test".toUnixPath
    val results: Option[Instant] = commandLine.mostRecentUpdate(path)

    results should equal(Some(Instant.parse("2020-12-06T09:00:00-05:00")))
  }

  it should "run sha1sum" in {
    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(
        MockCLIProcess(
          "adb -s 123456789 shell sha1sum '/test/dir-1/dir-1-listing.txt'",
          "256540527d5b9602387cbcc593caeb52c4ef1ff0  /test/dir-1/dir-1-listing.txt".getBytes(ADBCommandLine.UTF_8)
        ))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/test/dir-1/dir-1-listing.txt".toUnixPath
    val sha1: String = commandLine.sha1sum(path)
    sha1 should equal("256540527d5b9602387cbcc593caeb52c4ef1ff0")
  }

  it should "run scan" in {
    val statRoot = MockCLIProcess("adb -s 123456789 shell stat -c '%Y %F' '/test'",
                                  "1607173200 directory".getBytes(ADBCommandLine.UTF_8))

    val listStdout: Array[Byte] =
      """total 12
        |drwxrwx--x  4 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ./
        |drwxrwx--x 23 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |drwxrwx--x  5 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 dir-1/
        |drwxrwx--x  2 root sdcard_rw 3488 2020-12-07 08:00:00.000000000 -0500 dir-2/
        |-rw-rw----  1 root sdcard_rw   14 2020-12-08 08:00:00.000000000 -0500 root.txt
        |""".stripMargin.getBytes(ADBCommandLine.UTF_8)
    val listRoot = MockCLIProcess("adb -s 123456789 shell ls -pla --full-time '/test'", listStdout)

    val recursiveListDir1Stdout: Array[Byte] =
      """/test/dir-1:
        |total 27
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 ./
        |drwxrwx--x 4 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 dir-1-1/
        |drwxrwx--x 3 root sdcard_rw 3488 2020-12-07 08:00:00.000000000 -0500 dir-1-2/
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-08 08:00:00.000000000 -0500 dir-1-3/
        |-rw-rw---- 1 root sdcard_rw   27 2020-12-09 08:00:00.000000000 -0500 dir-1-listing.txt
        |-rw-rw---- 1 root sdcard_rw 5030 2020-12-10 08:00:00.000000000 -0500 info-1.txt
        |
        |/test/dir-1/dir-1-1:
        |total 6
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 ./
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 ../
        |
        |/test/dir-1/dir-1-2:
        |total 21
        |drwxrwx--x 3 root sdcard_rw 3488 2020-12-07 08:00:00.000000000 -0500 ./
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 ../
        |-rw-rw---- 1 root sdcard_rw 1404 2020-12-07 08:00:00.000000000 -0500 Hamlet.txt
        |-rw-rw---- 1 root sdcard_rw 1459 2020-12-08 08:00:00.000000000 -0500 JuliusCaesar.txt
        |-rw-rw---- 1 root sdcard_rw  755 2020-12-09 08:00:00.000000000 -0500 Tempest.html
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-10 08:00:00.000000000 -0500 dir-1-2-1/
        |
        |/test/dir-1/dir-1-2/dir-1-2-1:
        |total 14
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-10 08:00:00.000000000 -0500 ./
        |drwxrwx--x 3 root sdcard_rw 3488 2020-12-07 08:00:00.000000000 -0500 ../
        |-rw-rw---- 1 root sdcard_rw 5028 2020-12-08 08:00:00.000000000 -0500 info-1-2-1.txt
        |
        |/test/dir-1/dir-1-3:
        |total 6
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-08 08:00:00.000000000 -0500 ./
        |drwxrwx--x 5 root sdcard_rw 3488 2020-12-06 08:00:00.000000000 -0500 ../
        |""".stripMargin.getBytes(ADBCommandLine.UTF_8)
    val recursiveListDir1 =
      MockCLIProcess("adb -s 123456789 shell ls -Rpla --full-time '/test/dir-1'", recursiveListDir1Stdout)

    val recursiveListDir2Stdout: Array[Byte] =
      """/test/dir-2:
        |total 14
        |drwxrwx--x 2 root sdcard_rw 3488 2020-12-07 08:00:00.000000000 -0500 ./
        |drwxrwx--x 4 root sdcard_rw 3488 2020-12-05 08:00:00.000000000 -0500 ../
        |-rw-rw---- 1 root sdcard_rw 5028 2020-12-06 08:00:00.000000000 -0500 info-2.txt""".stripMargin.getBytes(
        ADBCommandLine.UTF_8)
    val recursiveListDir2 =
      MockCLIProcess("adb -s 123456789 shell ls -Rpla --full-time '/test/dir-2'", recursiveListDir2Stdout)

    val processes: ListBuffer[MockCLIProcess] = ListBuffer(statRoot, listRoot, recursiveListDir1, recursiveListDir2)

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val results: Seq[AndroidFileData] = commandLine.scan("/test".toUnixPath)

    val expected1 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test".toUnixPath), Instant.parse("2020-12-05T08:00:00-05:00"))
    val expected2 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1".toUnixPath),
                           Instant.parse("2020-12-06T08:00:00-05:00"))
    val expected3 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-2".toUnixPath),
                           Instant.parse("2020-12-07T08:00:00-05:00"))
    val expected4 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/root.txt".toUnixPath),
                             Instant.parse("2020-12-08T08:00:00-05:00"))
    val expected5 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-1".toUnixPath),
                           Instant.parse("2020-12-06T08:00:00-05:00"))
    val expected6 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2".toUnixPath),
                           Instant.parse("2020-12-07T08:00:00-05:00"))
    val expected7 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-3".toUnixPath),
                           Instant.parse("2020-12-08T08:00:00-05:00"))
    val expected8 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-listing.txt".toUnixPath),
                             Instant.parse("2020-12-09T08:00:00-05:00"))
    val expected9 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/info-1.txt".toUnixPath),
                             Instant.parse("2020-12-10T08:00:00-05:00"))
    val expected10 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/Hamlet.txt".toUnixPath),
                             Instant.parse("2020-12-07T08:00:00-05:00"))
    val expected11 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/JuliusCaesar.txt".toUnixPath),
                             Instant.parse("2020-12-08T08:00:00-05:00"))
    val expected12 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/Tempest.html".toUnixPath),
                             Instant.parse("2020-12-09T08:00:00-05:00"))
    val expected13 =
      AndroidDirectoryData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/dir-1-2-1".toUnixPath),
                           Instant.parse("2020-12-10T08:00:00-05:00"))
    val expected14 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt".toUnixPath),
                             Instant.parse("2020-12-08T08:00:00-05:00"))
    val expected15 =
      AndroidRegularFileData(AndroidLocation(fakeDevice, "/test/dir-2/info-2.txt".toUnixPath),
                             Instant.parse("2020-12-06T08:00:00-05:00"))

    results should equal(
      Seq(
        expected1,
        expected2,
        expected3,
        expected4,
        expected5,
        expected6,
        expected7,
        expected8,
        expected9,
        expected10,
        expected11,
        expected12,
        expected13,
        expected14,
        expected15
      ))
  }

  it should "count regular files" in {
    val stdout: Array[Byte] =
      """/test/root.txt
         |/test/dir-1/info-1.txt
         |/test/dir-1/dir-1-2/Tempest.html
         |/test/dir-1/dir-1-2/dir-1-2-1/info-1-2-1.txt
         |/test/dir-1/dir-1-2/Hamlet.txt
         |/test/dir-1/dir-1-2/JuliusCaesar.txt
         |/test/dir-1/dir-1-listing.txt
         |/test/dir-2/info-2.txt
         |""".stripMargin.getBytes(ADBCommandLine.UTF_8)

    val processes: ListBuffer[MockCLIProcess] =
      ListBuffer(MockCLIProcess("adb -s 123456789 shell find '/test' -type f", stdout))

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val path: AbsolutePath = "/test".toUnixPath
    val fileCount: Int = commandLine.countFiles(path)
    fileCount should equal(8)
  }

  it should "transfer files" in {
    val expectedOutput: Array[Byte] = RandomStringUtils.randomAlphanumeric(1000).getBytes(ADBCommandLine.UTF_8)
    val transferCommand = MockCLIProcess("adb -s 123456789 exec-out cat \"/test\"", expectedOutput)

    val processes: ListBuffer[MockCLIProcess] = ListBuffer(transferCommand)

    val fakeDevice: Device = Device("123456789")
    val commandLine: ADBCommandLine =
      new ADBCommandLine(new TestCLIFactory(processes), fakeDevice, Device.defaultTimeout, ADBCommandLine.UTF_8)

    val transferProcess: CLIProcess = commandLine.transferProcess("/test".toUnixPath)
    val (stdout, stderr) = transferProcess.createStreamHandlers()

    val stdoutBytes: Array[Byte] = IOUtils.toByteArray(stdout)
    val stderrBytes: Array[Byte] = IOUtils.toByteArray(stderr)
    stdoutBytes should equal(expectedOutput)
    stderrBytes.length should equal(0)
  }
}

case class MockCLIProcess(expectedCommand: String,
                          exitValue: Int,
                          stdout: Array[Byte],
                          stderr: Array[Byte] = Array.empty)

object MockCLIProcess {
  def apply(expectedCommand: String, stdout: Array[Byte]): MockCLIProcess =
    MockCLIProcess(expectedCommand, 0, stdout, Array.empty)
}

class TestCLI(values: MockCLIProcess) extends CLIProcess {
  override def runAsync(): WaitFor = () => run()

  override def run(): Int = values.exitValue

  override def createStreamHandlers(): (InputStream, InputStream) = {
    val stdout: ByteArrayInputStream = new ByteArrayInputStream(values.stdout)
    val stderr: ByteArrayInputStream = new ByteArrayInputStream(values.stderr)
    (stdout, stderr)
  }

  override def createReaderThreadHandlers(charset: Charset): (ReaderThread, ReaderThread) = {
    val (stdout, stderr) = createStreamHandlers()
    (new ReaderThread(stdout, charset), new ReaderThread(stderr, charset))
  }
}

class TestCLIFactory(processes: ListBuffer[MockCLIProcess]) extends CLIProcessFactory {
  override def create(args: Seq[String], timeout: Option[Timeout], expectedExitCodes: Option[Set[Int]]): CLIProcess = {
    if (processes.isEmpty) {
      fail(s"Unexpected new process request: ${args.mkString(", ")}")
    }

    val values: MockCLIProcess = processes.remove(0)
    args.mkString(" ") should equal(values.expectedCommand)
    new TestCLI(values)
  }
}
