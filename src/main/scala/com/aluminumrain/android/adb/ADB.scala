package com.aluminumrain.android.adb

import java.io.StringWriter
import java.nio.charset.Charset
import java.time.Instant

import com.aluminumrain.android.adb.find.{FindDepth, FindOption}
import com.aluminumrain.android.adb.ls.LsParser
import com.aluminumrain.android.file.{AndroidFile, Directory, VirtualFile, VirtualPath}
import enumeratum._
import org.apache.commons.io.IOUtils

import org.log4s._

object ADB {
  def usb: Shell = new ADBUsbShell
}

sealed abstract class Shell {
  import Shell._

  def arg: String

  private def cmd(command: String*): Seq[String] = {
    val args: Seq[String] = Seq("adb", arg, "shell") ++ command

    logger.info("EXECUTING SHELL COMMAND: " + args)
    val process: Process = new ProcessBuilder(args: _*).start()

    val output = new StringWriter()
    IOUtils.copy(process.getInputStream, output, Charset.defaultCharset)

    val exitValue: Int = process.waitFor();
    if (exitValue != 0) {
      logger.warn(s"NON ZERO EXIT VALUE ($exitValue) for $args")
    }

    val outputLines: Seq[String] = output.toString.split("\r\n")
    logger.debug(s"OUTPUT => ${outputLines.mkString("\n")}")

    outputLines
  }

  private def buildFile(path: VirtualPath, lastModified: Instant): VirtualFile = {
    if (path.isDirectoryPath) {
      Directory(path, lastModified)
    } else {
      AndroidFile(path, lastModified)
    }
  }

  def list(directoryPath: VirtualPath): Seq[VirtualFile] = {
    val listing: Seq[String] = cmd("ls", "-pla", "--full-time", directoryPath.quoted)
    for {
      (fileName, lastModified) <- LsParser.parseDirectory(listing)
    } yield buildFile(directoryPath ++ fileName, lastModified)
  }

  def listRecursive(directoryPath: VirtualPath): Seq[VirtualFile] = {
    val listings: Seq[String] = cmd("ls", "-Rpla", "--full-time", directoryPath.quoted)
    for {
      (path, fileName, lastModified) <- LsParser.parseDirectories(listings)
    } yield buildFile(path ++ fileName, lastModified)
  }

  def find(directoryPath: VirtualPath, options: FindOption*): Seq[VirtualPath] = {
    findCmd(directoryPath, options.flatMap(opt => Seq(opt.param, opt.value)))
  }

  def findDirectories(directoryPath: VirtualPath, options: FindOption*): Seq[VirtualPath] = {
    val directoryType = Seq("-type", "d")
    val findOpts: Seq[String] = options.flatMap(opt => Seq(opt.param, opt.value))
    findCmd(directoryPath, directoryType ++ findOpts)
  }

  //    val execStat: Seq[String] = Seq("-exec", "stat", "-c", "%F %y %n", "{}", "\\;")
  //    val found: Seq[String] = cmd((args ++ execStat): _*)

  private def findCmd(directoryPath: VirtualPath, params: Seq[String]): Seq[VirtualPath] = {
    val args: Seq[String] = Seq("find") ++ Seq(directoryPath.quoted) ++ params
    val found: Seq[String] = cmd(args: _*)
    found.map(VirtualPath.apply)
  }
}

object Shell {
  private[adb] val logger: Logger = getLogger
}

object ShellHelper {
  import Shell._

  implicit class ShellUtils(shell: Shell) {
    def scan(path: VirtualPath): Seq[VirtualFile] = {
      val directories: Seq[VirtualPath] = shell.findDirectories(path, FindDepth(1))
      directories.indices.flatMap { index =>
        val directoryPath: VirtualPath = directories(index)
        if ((index+1) % 10 == 0 || index == 0 || index == directories.size-1) {
          logger.info(s"Scanning: ${index+1}/${directories.size}")
        }

        shell.listRecursive(directoryPath)
      }
    }
  }
}

private class ADBUsbShell extends Shell {
  val arg = "-d"
}