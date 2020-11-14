package com.hydrangea.music.library

import java.time.Instant

import com.hydrangea.music.{Tag, TrackRecord}

object Diff {
  def mostRecent(tracks: TrackRecord*): DiffResult = {
    val latestUpdate: Instant = tracks.map(_.lastModified).max

    val latestUpdated: Seq[TrackRecord] = tracks.filter(_.lastModified.equals(latestUpdate))
    if (latestUpdated.size == 1) {
      Latest(latestUpdated.head)
    } else {
      val tags: Seq[Tag] = tracks.map(_.tag)
      val tagsMatch: Boolean = tags.tail.forall(_.equals(tags.head))
      val hashs: Seq[String] = tracks.map(_.hash)
      val hashsMatch: Boolean = hashs.tail.forall(_.equals(hashs.head))

      if (tagsMatch && hashsMatch) {
        Same
      } else {
        Conflict
      }
    }
  }

  def fileDiff(lhs: TrackRecord, rhs: TrackRecord): DiffResult = {
    if (lhs.lastModified.isAfter(rhs.lastModified)) {
      Latest(lhs)
    } else if (rhs.lastModified.isAfter(lhs.lastModified)) {
      Latest(rhs)
    } else {
      val tagsMatch: Boolean = lhs.tag.equals(rhs.tag)
      val hashMatch: Boolean = lhs.hash.equals(rhs.hash)

      if (tagsMatch && hashMatch) {
        Same
      } else {
        Conflict
      }
    }
  }
}

trait Diff {
  def compare(lhs: TrackRecord, rhs: TrackRecord): DiffResult
}

sealed trait DiffResult

// File times match, but the tag or content does not match
object Conflict extends DiffResult

// File times match, tag and content all match
object Same extends DiffResult

// The given record is the most recent file
case class Latest(trackRecord: TrackRecord) extends DiffResult
