package com.hydrangea.file

import java.nio.file.{Path, Paths}

import argonaut.Argonaut._
import argonaut._
import org.apache.commons.exec.util.StringUtils

case class AbsolutePath(base: PathBase, segments: List[String]) {
  import FilePath._

  def raw: String = base.root ++ segments.mkString(base.pathSeparator.toString)
  def escaped: String = "\"" + raw.escape(' ').escape('&').escape('\'').escape('(').escape(')') + "\""
  def quoted: String = "\"" + raw + "\""

  def commandLine: String =
    if (raw.contains("'")) {
      StringUtils.quoteArgument(raw).escape(' ').escape('&').escape('\'').escape('`').escape('(').escape(')')
    } else {
      "\'" + raw + "\'"
    }

  def fileName: String =
    if (raw.endsWith(base.pathSeparator.toString)) {
      val withoutTrailingSlash: JsonField = raw.substring(0, raw.length - 1)
      val split: Int = withoutTrailingSlash.lastIndexOf(base.pathSeparator)
      withoutTrailingSlash.substring(split + 1)
    } else {
      val split: Int = raw.lastIndexOf(base.pathSeparator)
      if (split > 0) {
        raw.substring(split + 1)
      } else {
        raw
      }
    }

  def relativePath(from: AbsolutePath): RelativePath =
    if (!base.root.equals(base.root)) {
      throw new IllegalArgumentException(s"File root does not match: ${this.base.root} != ${from.base.root}")
    } else {
      RelativePath(this.segments).relativeTo(from.segments)
    }

  def ++(relativePath: RelativePath): AbsolutePath =
    copy(segments = segments ++ relativePath.segments)

  def rebase(from: AbsolutePath, to: AbsolutePath): AbsolutePath =
    to ++ this.relativePath(from)

  def toJavaPath: Path = Paths.get(raw)

  override def toString: String = raw
}

sealed trait PathBase {
  def root: String
  def pathSeparator: Char
}

sealed trait WindowsPathBase extends PathBase {
  val pathSeparator: Char = FilePath.WindowsSeparator
}

case class LocalWindowsPathBase(driveLetter: Char) extends WindowsPathBase {
  val root = driveLetter + ":\\"
}

case class WindowsNetworkPathBase(host: String) extends WindowsPathBase {
  val root = "\\\\" + host + "\\"
}

object UnixPathBase extends PathBase {
  val pathSeparator: Char = FilePath.UnixSeparator
  val root = "/"
}

object AbsolutePath {
  import FilePath._

  // TODO: These are all actually optionals
  def apply(str: String): AbsolutePath =
    if (str.charAt(0) == '/') {
      str.toUnixPath
    } else if (str.charAt(0) == '\\') {
      str.toWindowsNetworkPath
    } else if (Character.isLetter(str.charAt(0))) {
      str.toLocalWindowsPath
    } else {
      // TODO: Is actually optional
      throw new IllegalArgumentException("Is not an AbsolutePath")
    }

  def localWindowsFile(driveLetter: Char, segments: List[String]): AbsolutePath =
    AbsolutePath(LocalWindowsPathBase(driveLetter), segments)

  def localWindowsFile(pathStr: String): AbsolutePath = {
    val splitIndex =
      if (pathStr.contains(WindowsSeparator)) {
        pathStr.indexOf(WindowsSeparator)
      } else {
        pathStr.length
      }

    val (driveColon, tail) = pathStr.splitAt(splitIndex)
    val driveLetter: Char = driveColon.charAt(0)
    val segments: List[String] = trim(tail, WindowsSeparator).split(WindowsSeparator).filterNot(_.isBlank).toList
    localWindowsFile(driveLetter, segments)
  }

  def windowsNetworkFile(host: String, segments: List[String]): AbsolutePath =
    AbsolutePath(WindowsNetworkPathBase(host), segments)

  def windowsNetworkFile(pathStr: String): AbsolutePath = {
    val splitIndex =
      if (pathStr.indexOf(WindowsSeparator, 2) >= 0) {
        pathStr.indexOf(WindowsSeparator, 2)
      } else {
        pathStr.length
      }

    val (root, segmentStr) = pathStr.splitAt(splitIndex)
    val host: String = root.substring(2)
    val segments: List[String] = trim(segmentStr, WindowsSeparator).split(WindowsSeparator).filterNot(_.isBlank).toList
    windowsNetworkFile(host, segments)
  }

  def unixFile(segments: List[String]): AbsolutePath =
    AbsolutePath(UnixPathBase, segments)

  def unixFile(pathStr: String): AbsolutePath = {
    val sanitized: String = trim(pathStr.substring(1), UnixSeparator)
    unixFile(sanitized.split(UnixSeparator).filterNot(_.isBlank).toList)
  }

  implicit def codex: CodecJson[AbsolutePath] = casecodec2(AbsolutePath.apply, AbsolutePath.unapply)("base", "segments")
}

object PathBase {
  implicit def encode: EncodeJson[PathBase] =
    base => Json("root" := base.root)

  implicit def decode: DecodeJson[PathBase] =
    cursor => {
      for {
        root <- (cursor --\ "root").as[String]
      } yield {
        if (root.startsWith("/")) {
          UnixPathBase
        } else if (root.startsWith("\\")) {
          val host: JsonField = root.substring(2, root.length - 1)
          WindowsNetworkPathBase(host)
        } else if (Character.isLetter(root.charAt(0))) {
          LocalWindowsPathBase(root.charAt(0))
        } else {
          // TODO
          throw new RuntimeException(s"Could not parse root: $root")
        }
      }
    }
}

case class RelativePath(segments: Seq[String]) {
  def relativeTo(base: Seq[String]): RelativePath = {
    if (base.isEmpty) {
      this
    } else {
      val next: String = base.head

      if (base.isEmpty) {
        this
      } else if (segments.isEmpty) {
        throw new IllegalArgumentException(s"Base path segments not found: $base")
      } else if (!segments.head.equals(segments.head)) {
        throw new IllegalArgumentException(s"Unexpected path segment: $next != ${segments.head}")
      } else {
        RelativePath(segments.tail).relativeTo(base.tail)
      }
    }
  }

  def isCurrentDirectory: Boolean =
    segments.equals(Seq("."))

  def isParentDirectory: Boolean =
    segments.equals(Seq(".."))
}

object FilePath {
  val WindowsSeparator: Char = '\\'
  val UnixSeparator: Char = '/'

  def extensionFilter(extension: String): AbsolutePath => Boolean =
    path => path.raw.endsWith(extension)

  val mp3Extension = ".mp3"
  val mp3Filter = extensionFilter(mp3Extension)

  implicit class StringPathOps(str: String) {
    def escape(ch: Char): String = str.replace("" + ch, s"\\$ch")

    def toLocalWindowsPath: AbsolutePath = AbsolutePath.localWindowsFile(str)

    def toWindowsNetworkPath: AbsolutePath = AbsolutePath.windowsNetworkFile(str)

    def toUnixPath: AbsolutePath = AbsolutePath.unixFile(str)

    def toAbsolutePath: AbsolutePath = AbsolutePath(str)

    def toRelativePath: RelativePath = {
      val separator =
        if (str.contains(WindowsSeparator)) {
          WindowsSeparator
        } else {
          UnixSeparator
        }

      val pathStr: String = trim(str, separator)
      RelativePath(pathStr.split(separator).filterNot(_.isBlank))
    }
  }

  implicit class JavaPathOps(javaPath: Path) {
    def asLocalWindowsPath: AbsolutePath = javaPath.toAbsolutePath.toString.toLocalWindowsPath

    def asWindowsNetworkPath: AbsolutePath = javaPath.toAbsolutePath.toString.toWindowsNetworkPath

    def asUnixPath: AbsolutePath = javaPath.toAbsolutePath.toString.toUnixPath

    def asAbsolutePath: AbsolutePath = javaPath.toAbsolutePath.toString.toAbsolutePath
  }

  def trim(str: String, separator: Char): String = {
    val startIndex =
      if (str.startsWith(separator.toString)) {
        1
      } else {
        0
      }

    val endIndex =
      if (str.endsWith(separator.toString)) {
        str.length - 1
      } else {
        str.length
      }

    str.substring(startIndex, endIndex)
  }
}
