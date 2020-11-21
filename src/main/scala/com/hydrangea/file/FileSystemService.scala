package com.hydrangea.file

import scalaz.Disjunction

trait FileSystemService {
  def read[F: FileSystem[P], P <: AbsolutePath](fileSystem: FileSystem[F], path: P): Disjunction[String, LazyList[Byte]]
  def write[F: FileSystem[P], P <: AbsolutePath](fileSystem: FileSystem[F],
                                                 path: P,
                                                 content: LazyList[Byte]): Disjunction[String, Void]
}
