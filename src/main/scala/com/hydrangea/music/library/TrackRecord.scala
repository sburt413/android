package com.hydrangea.music.library

import java.time.Instant

import com.hydrangea.android.file.{VirtualFile, VirtualPath}
import com.mpatric.mp3agic.ID3v2

/**
  * Tag information for an mp3 track.
  *
  * @param title        the title of the track
  * @param album        the album the track is from
  * @param artist       the artist of the track
  */
case class Tag(title: String, album: String, artist: String)

/**
  * A record of an mp3 track.
  *
  * @param hash         the sha1 hash of the file
  * @param path         the path to the file on the device
  * @param lastModified the last modified time of the file according to the filesystem
  * @param tag          the tag information for the file
  */
case class TrackRecord(hash: String, path: VirtualPath, lastModified: Instant, tag: Tag)

object TrackRecord {
  def apply(hash: String, file: VirtualFile, tag: ID3v2): TrackRecord =
    TrackRecord(hash, file.path, file.modifyTime, Tag(tag.getTitle, tag.getAlbum, tag.getArtist))

//  def fileDiff(trackRecord0: TrackRecord, trackRecords: TrackRecord*): DiffResult = {
//    Seq(trackRecord0) ++ trackRecords
//
//    val sameModifiedDate: Boolean = trackRecords.map(_.lastModified).distinct.size == 1
//    sameModifiedDate
//  }
}
