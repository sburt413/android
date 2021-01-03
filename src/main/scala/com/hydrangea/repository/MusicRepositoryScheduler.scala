package com.hydrangea.repository

import java.time.Instant

import com.google.inject.Inject
import com.hydrangea.file.{DirectoryFileData, FileLocation, FileSystemService}

import scala.reflect.ClassTag

class MusicRepositoryScheduler @Inject()(fileSystemService: FileSystemService) {
  def createSchedule[L <: FileLocation](source: MusicRepository[L])(implicit classTag: ClassTag[L]): Schedule[L] =
    updateSchedule(source, Schedule(Nil))

  def updateSchedule[L <: FileLocation](source: MusicRepository[L], schedule: Schedule[L])(
      implicit classTag: ClassTag[L]): Schedule[L] = {
    val directories: Seq[DirectoryFileData] =
      fileSystemService
        .list(source.root)
        .collect({
          case directory: DirectoryFileData => directory
        })

    val newRecords: Seq[ScheduleRecord[L]] =
      for {
        directory <- directories
        location <- directory.location.to[L]
      } yield {
        val mostRecentUpdate: Instant =
          fileSystemService
            .mostRecentUpdate(directory.location)
            .getOrElse(
              throw new IllegalStateException(s"No most recent update time for location: ${directory.location}"))

        ScheduleRecord(location, None, mostRecentUpdate)
      }

    schedule.merge(newRecords)
  }
}

/**
  * A record of a top level directory in a [[MusicRepository]].  This is an agregation of the newest {{mostRecentUpdate}}
  * time for any file in the directory in the file system, and the last time this file was indexed in the
  * [[MusicRepository]].
  *
  * @param directoryLocation the location under the repository for the directory
  * @param lastIndexed       when the records in the were last indexed for this location
  * @param mostRecentUpdate  when the most recent update in the file system was for any file under the directory
  * @tparam L                the type of location
  */
case class ScheduleRecord[L <: FileLocation](directoryLocation: L,
                                             lastIndexed: Option[Instant],
                                             mostRecentUpdate: Instant) {

  /**
    * Returns whether the [[RepositoryRecord]]s for this [[MusicRepository]] for this given {{directoryLocation}} are
    * stale and need to be reindexed.
    *
    * @return whether the records under {{directoryLocation}} are stale
    */
  def needsUpdate: Boolean = lastIndexed.forall(indexed => mostRecentUpdate.isAfter(indexed))
}

/**
  * A [[Schedule]] is the aggregation of when a repository system was updated on the file system and when the file was
  * last indexed.  This allows the schedule to determine which sets of records in a [[MusicRepository]] are stale and
  * need updates.  [[Schedule]]s track groups of records by top level directories in the repository rather than by
  * individual files.  File systems generally can be more efficient in finding the most recent update date for a whole
  * directory structure than for determining the most recent update date for all files in the structure.
  *
  * @param records the records for this schedule
  * @tparam L      the type of locations for these records
  */
case class Schedule[L <: FileLocation](records: List[ScheduleRecord[L]]) {
  def merge(newRecords: Seq[ScheduleRecord[L]]): Schedule[L] = {
    val sourceRecordsByLocation: Map[L, ScheduleRecord[L]] = records.map(r => r.directoryLocation -> r).toMap
    val newRecordsByLocation: Map[L, ScheduleRecord[L]] = newRecords.map(r => r.directoryLocation -> r).toMap
    val mergedRecords: Set[ScheduleRecord[L]] =
      (sourceRecordsByLocation.keySet ++ newRecordsByLocation.keySet).map(location => {
        val sourceLastIndexed: Option[Instant] = sourceRecordsByLocation.get(location).flatMap(_.lastIndexed)
        val newLastIndexed: Option[Instant] = newRecordsByLocation.get(location).flatMap(_.lastIndexed)
        val lastIndexed: Option[Instant] = mostRecent(sourceLastIndexed, newLastIndexed)

        // This currently ignore the oddity if the _new_ record has the less recently update time.
        // This may ignore updates if there happens to be a clock skew.
        val sourceMostRecentUpdate: Option[Instant] = sourceRecordsByLocation.get(location).map(_.mostRecentUpdate)
        val newMostRecentUpdate: Option[Instant] = newRecordsByLocation.get(location).map(_.mostRecentUpdate)
        val mostRecentUpdate: Instant = mostRecent(sourceMostRecentUpdate, newMostRecentUpdate).getOrElse(
          throw new IllegalStateException("No most recent update"))

        ScheduleRecord(location, lastIndexed, mostRecentUpdate)
      })

    Schedule(mergedRecords.toList)
  }

  private def mostRecent(lhs: Option[Instant], rhs: Option[Instant]): Option[Instant] =
    compare(_.compareTo(_) > 0)(lhs, rhs)

  private def compare(fn: (Instant, Instant) => Boolean)(lhs: Option[Instant], rhs: Option[Instant]): Option[Instant] =
    (lhs, rhs) match {
      case (Some(lhsInstant), Some(rhsInstant)) =>
        if (fn(lhsInstant, rhsInstant)) {
          Some(lhsInstant)
        } else {
          Some(rhsInstant)
        }
      case (Some(instant), None) => Some(instant)
      case (None, Some(instant)) => Some(instant)
      case (None, None)          => None
    }
}
