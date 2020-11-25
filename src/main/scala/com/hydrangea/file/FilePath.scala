package com.hydrangea.file

case class AbsolutePath(base: PathBase, segments: Seq[String]) {
  import FilePath._

  def raw: String = base.root ++ segments.mkString(base.pathSeparator.toString)
  def escaped: String = "\"" + raw.escape(' ').escape('&').escape('\'').escape('(').escape(')') + "\""
  def quoted: String = "\"" + raw + "\""

  def relativePath(from: AbsolutePath): RelativePath = {
    if (!base.root.equals(base.root)) {
      throw new IllegalArgumentException(s"File root does not match: ${this.base.root} != ${from.base.root}")
    } else {
      RelativePath(this.segments).relativeTo(from.segments)
    }
  }

  def ++(relativePath: RelativePath): AbsolutePath =
    copy(segments = segments ++ relativePath.segments)

  def rebase(from: AbsolutePath, to: AbsolutePath): AbsolutePath =
    to ++ this.relativePath(from)

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

  def localWindowsFile(driveLetter: Char, segments: Seq[String]): AbsolutePath =
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
    val segments: Seq[String] = trim(tail, WindowsSeparator).split(WindowsSeparator).filterNot(_.isBlank)
    localWindowsFile(driveLetter, segments)
  }

  def windowsNetworkFile(host: String, segments: Seq[String]): AbsolutePath =
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
    val segments: Seq[String] = trim(segmentStr, WindowsSeparator).split(WindowsSeparator).filterNot(_.isBlank)
    windowsNetworkFile(host, segments)
  }

  def unixFile(segments: Seq[String]): AbsolutePath =
    AbsolutePath(UnixPathBase, segments)

  def unixFile(pathStr: String): AbsolutePath = {
    val sanitized: String = trim(pathStr.substring(1), UnixSeparator)
    unixFile(sanitized.split(UnixSeparator).filterNot(_.isBlank))
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
}

object FilePath {
  val WindowsSeparator: Char = '\\'
  val UnixSeparator: Char = '/'

  implicit class StringPathOps(str: String) {
    def escape(ch: Char): String = str.replace("" + ch, s"\\$ch")

    def toLocalWindowsPath: AbsolutePath = AbsolutePath.localWindowsFile(str)

    def toWindowsNetworkPath: AbsolutePath = AbsolutePath.windowsNetworkFile(str)

    def toUnixPath: AbsolutePath = AbsolutePath.unixFile(str)

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
