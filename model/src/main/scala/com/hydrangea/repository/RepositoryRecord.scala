package com.hydrangea.repository

import java.time.Instant

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.codec.Codecs._
import com.hydrangea.file.{AbsolutePath, RelativePath}
import com.hydrangea.music.track.Track

case class RepositoryRecord(path: RelativePath, track: Track, lastIndexed: Instant)

object RepositoryRecord {
  def apply(repositoryRoot: AbsolutePath, track: Track, lastIndexed: Instant = Instant.now()): RepositoryRecord =
    RepositoryRecord(track.path.relativePath(repositoryRoot), track, lastIndexed)

  implicit def encode: CodecJson[RepositoryRecord] = {
    def disambiguatedApply(path: RelativePath, track: Track, lastIndexed: Instant): RepositoryRecord =
      RepositoryRecord(path, track, lastIndexed)
    casecodec3(disambiguatedApply, RepositoryRecord.unapply)("path", "track", "lastIndexed")
  }
}
