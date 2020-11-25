package com.hydrangea.repository

import com.hydrangea.file.FileLocation

case class MusicRepository[L <: FileLocation](location: L, records: Seq[TrackRecord2]) {
  def absolutePath(record: TrackRecord2): FileLocation =
    location ++ record.path
}
