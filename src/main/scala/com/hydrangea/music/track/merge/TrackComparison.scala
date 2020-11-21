package com.hydrangea.music.track.merge

import com.hydrangea.music.track.{Track, merge}

import scala.annotation.tailrec

sealed trait TrackComparison {
  def duplicateResult(rhs: TrackComparison): Boolean = {
    if (this.equals(rhs)) {
      true
    } else {
      (this, rhs) match {
        case (lhsConflict: TrackConflict, rhsConflict: TrackConflict)     => lhsConflict.isSymmetricWith(rhsConflict)
        case (lhsDuplicate: DuplicateTrack, rhsDuplicate: DuplicateTrack) => lhsDuplicate.isSymmetricWith(rhsDuplicate)
        case _                                                            => false
      }
    }
  }
}

object TrackComparison {
  def compare(sourceTracks: Set[Track], destinationTracks: Set[Track]): Set[TrackComparison] = {
    val sourceComparisons: Set[TrackComparison] =
      sourceTracks.flatMap(source => {
        matchingByPath(source, destinationTracks)
          .map(matchingRhs => {
            val matchingTag = source.tag.equals(matchingRhs.tag)
            val matchingContent = source.hash.equals(matchingRhs.hash)
            if (!matchingTag || !matchingContent) {
              merge.TrackConflict(source, matchingRhs)
            } else {
              TrackMatch(source)
            }
          })
          .orElse(matchingByTagOrContent(source, destinationTracks).flatMap(matchingRhs => {
            if (source.tag.equals(matchingRhs.tag) && source.hash.equals(matchingRhs.hash)) {
              Some(merge.DuplicateTrack(source, matchingRhs))
            } else {
              // Would be a TrackMatch from above or unrelated
              None
            }
          }))
          .orElse(Some(TrackAdded(source)))
      })

    val destinationComparisons: Set[TrackRemoved] =
      destinationTracks
        .filter(destination =>
          sourceTracks.forall(lhs => {
            !destination.path.equals(lhs.path) && !destination.tag.equals(lhs.tag) && !destination.hash.equals(lhs.hash)
          }))
        .map(TrackRemoved)

    deduplicate(sourceComparisons ++ destinationComparisons)
  }

  private def matchingByPath(track: Track, tracks: Set[Track]): Option[Track] =
    tracks.find(t => track.path.equals(t.path))

  private def matchingByTagOrContent(track: Track, tracks: Set[Track]): Option[Track] =
    tracks.find(t => track.tag.equals(t.tag) || track.hash.equals(t.hash))

  def deduplicate(comparisons: Set[TrackComparison]): Set[TrackComparison] = {
    @tailrec
    def deduplicateHelper(accepted: Set[TrackComparison], remaining: Set[TrackComparison]): Set[TrackComparison] = {
      if (remaining.isEmpty) {
        accepted
      } else {
        val current: TrackComparison = remaining.head
        if (!accepted.exists(_.duplicateResult(current))) {
          deduplicateHelper(accepted + current, remaining.tail)
        } else {
          deduplicateHelper(accepted, remaining.tail)
        }
      }
    }

    deduplicateHelper(Set.empty, comparisons)
  }
}

case class TrackMatch(track: Track) extends TrackComparison

case class TrackAdded(track: Track) extends TrackComparison

case class TrackRemoved(track: Track) extends TrackComparison

case class TrackConflict(lhs: Track, rhs: Track) extends TrackComparison {
  val conflictingTags: Boolean = !lhs.tag.equals(rhs.tag)
  val conflictingContent: Boolean = !lhs.hash.equals(rhs.hash)

  def isSymmetricWith(conflict: TrackConflict): Boolean = lhs.equals(conflict.rhs) && rhs.equals(conflict.lhs)
}

case class DuplicateTrack(lhs: Track, rhs: Track) extends TrackComparison {
  val duplicateTags: Boolean = lhs.tag.equals(rhs.tag)
  val duplicateContent: Boolean = lhs.hash.equals(rhs.hash)

  def isSymmetricWith(duplicate: DuplicateTrack): Boolean = lhs.equals(duplicate.rhs) && rhs.equals(duplicate.lhs)
}
