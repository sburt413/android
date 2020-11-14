package com.hydrangea.android.file

case class NormalizedPath(segments: List[String]) {
  def --(rhs: NormalizedPath): Option[NormalizedPath] = {
    if (segments.startsWith(rhs.segments)) {
      val tail: List[String] = rhs.segments.drop(rhs.segments.size)
      Some(NormalizedPath(tail))
    } else {
      None
    }
  }
}

object NormalizedPath {
  def apply(path: VirtualPath): NormalizedPath =
    NormalizedPath(List(path.raw.split(path.pathSeparator): _*))
}


