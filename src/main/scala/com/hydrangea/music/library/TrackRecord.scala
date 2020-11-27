package com.hydrangea.music.library

import java.time.Instant

import com.hydrangea.file.{AbsolutePath, FileData}
import com.hydrangea.music.track.{Tag, Track}
import com.mpatric.mp3agic.ID3v2

/**
  * A record of an mp3 track.
  */
case class TrackRecord(track: Track, lastIndexed: Instant) {
  def hash: String = track.hash
  def path: AbsolutePath = track.path
  def lastModified: Instant = track.lastModified
  def tag: Tag = track.tag
}

object TrackRecord {
  def apply(hash: String, path: AbsolutePath, lastModified: Instant, lastIndexed: Instant, tag: Tag): TrackRecord =
    TrackRecord(Track(hash: String, path: AbsolutePath, lastModified: Instant, tag), lastIndexed)

  def apply(hash: String, file: FileData, tag: Tag): TrackRecord =
    TrackRecord(hash, file.location.path, file.modifyTime, Instant.now(), tag)

  def apply(hash: String, file: FileData, tag: ID3v2): TrackRecord = {
    val year: Option[Int] = Option(tag.getYear).map(_.toInt)
    val (trackNumber, trackCount) = Option(tag.getTrack).filterNot(_.isEmpty).map(slashSplit).getOrElse((None, None))
    val (discNumber, discCount) =
      Option(tag.getPartOfSet).filterNot(_.isEmpty).map(slashSplit).getOrElse((None, None))
    TrackRecord(hash,
                file.location.path,
                file.modifyTime,
                Instant.now(),
                Tag(tag.getTitle, tag.getAlbum, tag.getArtist, year, trackNumber, trackCount, discNumber, discCount))
  }

  def slashSplit(value: String): (Option[Int], Option[Int]) = {
    if (value.contains('/')) {
      val (number, total) = value.splitAt(value.indexOf('/'))
      (Some(number.toInt), Some(total.substring(1).toInt))
    } else {
      (Some(value.toInt), None)
    }
  }
}
