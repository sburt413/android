package com.hydrangea.repository

import com.hydrangea.file.FileLocation

case class MusicRepository[L <: FileLocation](location: L, records: Seq[RepositoryRecord]) {
  def absolutePath(record: RepositoryRecord): FileLocation =
    location ++ record.path
}
