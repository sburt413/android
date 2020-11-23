package com.hydrangea.file

import java.nio.file.Path

sealed abstract class AbsolutePath(val segments: Seq[String], separator: Char) {
  import FilePath._

  type thisType >: this.type <: AbsolutePath

  def root: String

  def raw: String = root ++ segments.mkString(separator.toString)
  def escaped: String = "\"" + raw.escape(' ').escape('&').escape('\'').escape('(').escape(')') + "\""
  def quoted: String = "\"" + raw + "\""

  def relativePath(from: thisType): RelativePath = {
    if (!this.root.equals(from.root)) {
      throw new IllegalArgumentException(s"File root does not match: ${this.root} != ${from.root}")
    } else {
      RelativePath(this.segments).relativeTo(from.segments)
    }
  }

  def ++(relativePath: RelativePath): thisType

  def rebase[P <: AbsolutePath](from: thisType, to: P)(implicit builder: AbsolutePathBuilder[P]): P =
    builder.build(to, this.relativePath(from))

  override def toString: String = raw
}

sealed trait WindowsPath2 extends AbsolutePath {
  def toJava: Path = Path.of(raw)
}

case class LocalWindowsPath(driveLetter: String, override val segments: Seq[String])
    extends AbsolutePath(segments, FilePath.WindowsSeparator)
    with WindowsPath2 {
  override type thisType = LocalWindowsPath

  val root = driveLetter + ":\\"

  override def ++(relativePath: RelativePath): LocalWindowsPath =
    LocalWindowsPath(driveLetter, segments ++ relativePath.segments)
}

case class WindowsNetworkPath(host: String, override val segments: Seq[String])
    extends AbsolutePath(segments, FilePath.WindowsSeparator)
    with WindowsPath2 {
  override type thisType = WindowsNetworkPath

  val root = "\\\\" + host + "\\"

  override def ++(relativePath: RelativePath): WindowsNetworkPath =
    WindowsNetworkPath(host, segments ++ relativePath.segments)
}

case class UnixPath(override val segments: Seq[String]) extends AbsolutePath(segments, FilePath.UnixSeparator) {
  override type thisType = UnixPath

  val root = "/"

  override def ++(relativePath: RelativePath): UnixPath =
    UnixPath(segments ++ relativePath.segments)
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

  def from[P <: AbsolutePath](root: P)(implicit builder: AbsolutePathBuilder[P]): P = builder.build(root, this)
}

protected[file] trait AbsolutePathBuilder[P <: AbsolutePath] {
  def build(base: P, relativePath: RelativePath): P
}

object LocalWindowsPath {
  implicit val windowsBuilder: AbsolutePathBuilder[LocalWindowsPath] = (base, relativePath) => base ++ relativePath
}

object WindowsNetworkPath {
  implicit val networkPathBuilder: AbsolutePathBuilder[WindowsNetworkPath] = (base, relativePath) =>
    base ++ relativePath
}

object UnixPath {
  implicit val unixBuilder: AbsolutePathBuilder[UnixPath] = (base, relativePath) => base ++ relativePath
}

object FilePath {
  val WindowsSeparator: Char = '\\'
  val UnixSeparator: Char = '/'

  implicit class StringPathOps(str: String) {
    def escape(ch: Char): String = str.replace("" + ch, s"\\$ch")

    def toUnixPath: UnixPath = {
      val sanitized: String = sanitize(str.substring(1), UnixSeparator)
      UnixPath(sanitized.split(UnixSeparator).filterNot(_.isBlank))
    }

    def toLocalWindowsPath: LocalWindowsPath = {
      val splitIndex =
        if (str.contains(WindowsSeparator)) {
          str.indexOf(WindowsSeparator)
        } else {
          str.length
        }

      val (driveColon, tail) = str.splitAt(splitIndex)
      val driveLetter: Char = driveColon.charAt(0)
      val segments: Seq[String] = sanitize(tail, WindowsSeparator).split(WindowsSeparator).filterNot(_.isBlank)
      LocalWindowsPath(driveLetter.toString, segments)
    }

    def toWindowsNetworkPath: WindowsNetworkPath = {
      val splitIndex =
        if (str.indexOf(WindowsSeparator, 2) >= 0) {
          str.indexOf(WindowsSeparator, 2)
        } else {
          str.length
        }

      val (root, segmentStr) = str.splitAt(splitIndex)
      val host: String = root.substring(2)
      val segments: Seq[String] = sanitize(segmentStr, WindowsSeparator).split(WindowsSeparator).filterNot(_.isBlank)
      WindowsNetworkPath(host, segments)
    }

    def toRelativePath: RelativePath = {
      val separator =
        if (str.contains(WindowsSeparator)) {
          WindowsSeparator
        } else {
          UnixSeparator
        }

      val pathStr: String = sanitize(str, separator)
      RelativePath(pathStr.split(separator).filterNot(_.isBlank))
    }
  }

  private def sanitize(str: String, separator: Char): String = {
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
