package com.hydrangea.repository.merge

import com.hydrangea.repository.RepositoryRecord

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
  def compare(sourceRecords: Set[RepositoryRecord], destinationRecords: Set[RepositoryRecord]): Set[TrackComparison] = {
    val sourceComparisons: Set[TrackComparison] =
      sourceRecords.flatMap(source => {
        matchingByPath(source, destinationRecords)
          .map(matchingRhs => {
            val matchingTag = source.track.tag.equals(matchingRhs.track.tag)
            val matchingContent = source.track.hash.equals(matchingRhs.track.hash)
            if (!matchingTag || !matchingContent) {
              TrackConflict(source, matchingRhs)
            } else {
              TrackMatch(source)
            }
          })
          .orElse(matchingByTagOrContent(source, destinationRecords).flatMap(matchingRhs => {
            if (source.track.tag.equals(matchingRhs.track.tag) && source.track.hash.equals(matchingRhs.track.hash)) {
              Some(DuplicateTrack(source, matchingRhs))
            } else {
              // Would be a TrackMatch from above or unrelated
              None
            }
          }))
          .orElse(Some(TrackAdded(source)))
      })

    val destinationComparisons: Set[TrackRemoved] =
      destinationRecords
        .filter(destination =>
          sourceRecords.forall(lhs => {
            !destination.path.equals(lhs.path) && !destination.track.tag
              .equals(lhs.track.tag) && !destination.track.hash.equals(lhs.track.hash)
          }))
        .map(TrackRemoved)

    deduplicate(sourceComparisons ++ destinationComparisons)
  }

  // TODO: This must go by relative paths from the repository, not the track
  private def matchingByPath(record: RepositoryRecord, records: Set[RepositoryRecord]): Option[RepositoryRecord] =
    records.find(t => record.path.equals(t.path))

  private def matchingByTagOrContent(record: RepositoryRecord,
                                     records: Set[RepositoryRecord]): Option[RepositoryRecord] =
    records.find(r => record.track.tag.equals(r.track.tag) || record.track.hash.equals(r.track.hash))

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

case class TrackMatch(record: RepositoryRecord) extends TrackComparison

case class TrackAdded(record: RepositoryRecord) extends TrackComparison

case class TrackRemoved(record: RepositoryRecord) extends TrackComparison

case class TrackConflict(source: RepositoryRecord, destination: RepositoryRecord) extends TrackComparison {
  val conflictingTags: Boolean = !source.track.tag.equals(destination.track.tag)
  val conflictingContent: Boolean = !source.track.hash.equals(destination.track.hash)

  def isSymmetricWith(conflict: TrackConflict): Boolean =
    source.equals(conflict.destination) && destination.equals(conflict.source)
}

case class DuplicateTrack(source: RepositoryRecord, dest: RepositoryRecord) extends TrackComparison {
  val duplicateTags: Boolean = source.track.tag.equals(dest.track.tag)
  val duplicateContent: Boolean = source.track.hash.equals(dest.track.hash)

  def isSymmetricWith(duplicate: DuplicateTrack): Boolean =
    source.equals(duplicate.dest) && dest.equals(duplicate.source)
}
