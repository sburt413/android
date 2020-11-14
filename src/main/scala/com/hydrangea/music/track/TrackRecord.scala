package com.hydrangea.music

import java.time.Instant

import com.hydrangea.android.file.{VirtualFile, VirtualPath}
import com.mpatric.mp3agic.ID3v2

/**
 * A record of an mp3 track.
 *
 * @param hash         the sha1 hash of the file
 * @param path         the path to the file on the device
 * @param lastModified the last modified time of the file according to the filesystem
 * @param tag          the tag information for the file
 */
case class TrackRecord(hash: String, path: VirtualPath, lastModified: Instant, lastIndexed: Instant, tag: Tag)

/**
  * Tag information for an mp3 track.
  *
  * @param title       the title of the track
  * @param album       the album the track is from
  * @param artist      the artist of the track
  * @param year        the release year of the track, if known
  * @param trackNumber the track number of track in the album, if known
  * @param trackCount  the total number of tracks for the album, if known
  * @param discNumber  the the disc number in the album, if known
  * @param discCount   the number of discs in the album
  */
case class Tag(title: String,
               album: String,
               artist: String,
               year: Option[Int],
               trackNumber: Option[Int],
               trackCount: Option[Int],
               discNumber: Option[Int],
               discCount: Option[Int])

object TrackRecord {
  def apply(hash: String, file: VirtualFile, tag: Tag): TrackRecord =
    TrackRecord(hash, file.path, file.modifyTime, Instant.now(), tag)

  def apply(hash: String, file: VirtualFile, tag: ID3v2): TrackRecord = {
    val year: Option[Int] = Option(tag.getYear).map(_.toInt)
    val (trackNumber, trackCount) = Option(tag.getTrack).filterNot(_.isEmpty).map(slashSplit).getOrElse((None, None))
    val (discNumber, discCount) =
      Option(tag.getPartOfSet).filterNot(_.isEmpty).map(slashSplit).getOrElse((None, None))
    TrackRecord(hash,
                file.path,
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
