package com.hydrangea.repository

import com.hydrangea.file.{AbsolutePath, RelativePath}
import com.hydrangea.music.track.Track

case class RepositoryRecord(path: RelativePath, track: Track)

object RepositoryRecord {
  def apply(repositoryRoot: AbsolutePath, track: Track): RepositoryRecord =
    RepositoryRecord(track.path.relativePath(repositoryRoot), track)
}
