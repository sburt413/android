package com.hydrangea.music.library

object Diff {
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
