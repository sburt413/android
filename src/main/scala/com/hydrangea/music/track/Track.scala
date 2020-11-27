package com.hydrangea.music.track

import java.time.Instant

import com.hydrangea.file.AbsolutePath

/**
  * An object representing a mp3 track.
  *
  * @param hash         the sha1 hash of the file
  * @param path         the path to the file on the device
  * @param tag          the tag information for the file
  */
case class Track(hash: String, path: AbsolutePath, lastModified: Instant, tag: Tag)

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
