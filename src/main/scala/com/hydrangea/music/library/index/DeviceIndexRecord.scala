package com.hydrangea.music.library.index

import java.time.Instant

import com.hydrangea.android.file.{AndroidDirectory, AndroidPath}
import argonaut._
import Argonaut._
import com.hydrangea.codec.Codecs._

case class DeviceIndexRecord(rootDirectoryPath: AndroidDirectory, childRecords: List[LastIndexedRecord]) {

  /**
    * Merges the given [[RecordCandidate]] with any existing record.  If any record exists, then the most recent
    * modified time between the existing record and candidate is used for the merged record.  Otherwise, a new record is
    * created and returned.
    *
    * @param candidate the candidate record
    * @return the updated [[LastIndexedRecord]]; otherwise, a new record for the directory
    */
  private def mergedRecord(candidate: RecordCandidate): LastIndexedRecord = {
    val existingRecordForDir: Option[LastIndexedRecord] =
      childRecords.find(_.directoryPath.equals(candidate.directoryPath))

    existingRecordForDir
      .map(_.update(candidate.lastUpdated))
      .getOrElse(candidate.toLastIndexedRecord)
  }

  /**
    * Merges all the given [[AndroidDirectory]] objects into this index and returns the updated value.
    *
    * @param candidates the values to merge into this index.
    * @return an updated [[DeviceIndexRecord]] containing the given directories
    */
  def addListings(candidates: List[RecordCandidate]): DeviceIndexRecord = {
    val directoriesRecords: List[LastIndexedRecord] =
      candidates.map(mergedRecord)

    updateRecords(directoriesRecords)
  }

  /**
    * Merges all the given directories into this index and discards any existing record that was not found in the given
    * directories.
    *
    * @param candidates all the directories this index should contain and the most recent update time for it or any
    *                        ancestor
    * @return a new [[DeviceIndexRecord]] containing only the given directories
    */
  def reindex(candidates: List[RecordCandidate]): DeviceIndexRecord = {
    val records: List[LastIndexedRecord] =
      candidates.map(mergedRecord)

    DeviceIndexRecord(rootDirectoryPath, records)
  }

  /**
    * Returns an updated [[DeviceIndexRecord]] containing the given record, replacing any existing record for that path.
    *
    * @param newChildRecord the record to update
    * @return an updated [[DeviceIndexRecord]] containing the given record
    */
  def updateRecord(newChildRecord: LastIndexedRecord): DeviceIndexRecord =
    updateRecords(List(newChildRecord))

  /**
    * Returns an updated [[DeviceIndexRecord]] containing the given records, replacing any existing records for that
    * path.
    *
    * @param newChildRecords the records to update
    * @return an updated [[DeviceIndexRecord]] containing the given records
    */
  def updateRecords(newChildRecords: List[LastIndexedRecord]): DeviceIndexRecord = {
    val newChildPaths: List[AndroidPath] = newChildRecords.map(_.directoryPath)
    val updatedRecords: List[LastIndexedRecord] =
      childRecords.filterNot(existing => newChildPaths.contains(existing.directoryPath)) ++ newChildRecords
    DeviceIndexRecord(rootDirectoryPath, updatedRecords)
  }

  /**
    * Returns all records for which the source directory was more recently updated than this index.
    *
    * @return all records for which the source directory was more recently updated than this index
    */
  def needsUpdating: List[LastIndexedRecord] = childRecords.filter(_.needsUpdate)

  /**
    * Returns whether any records are currently out of date with the source directory.
    *
    * @return whether any records are currently out of date with the source directory
    */
  def needsUpdate: Boolean = needsUpdating.nonEmpty

  /**
    * Returns a [[List]] of all records that need updating, base on least recently updated.
    * @return
    */
  def calculateUpdateSchedule: List[AndroidPath] = needsUpdating.sortBy(_.lastUpdate).map(_.directoryPath)
}

object DeviceIndexRecord {
  def create(rootDirectory: AndroidDirectory, candidates: List[RecordCandidate]): DeviceIndexRecord = {
    val records: List[LastIndexedRecord] = candidates.map(_.toLastIndexedRecord)
    new DeviceIndexRecord(rootDirectory, records)
  }

  implicit def codex: CodecJson[DeviceIndexRecord] =
    casecodec2(DeviceIndexRecord.apply, DeviceIndexRecord.unapply)("rootDirectoryPath", "childRecords")
}

case class LastIndexedRecord(directoryPath: AndroidPath, lastUpdate: Instant, lastIndexed: Option[Instant]) {
  def needsUpdate: Boolean = lastIndexed.exists(indexed => lastUpdate.isAfter(indexed))

  /**
    * Returns an updated record with the most recent of either this record's lastUpdate or the given modify time.
    *
    * @param modifyTime the potential new update time
    * @return an updated record with the most recent update time
    */
  def update(modifyTime: Instant): LastIndexedRecord = {
    val newLastUpdate: Instant = Seq(lastUpdate, modifyTime).max
    copy(lastUpdate = newLastUpdate)
  }
}

object LastIndexedRecord {
  implicit def codex: CodecJson[LastIndexedRecord] =
    casecodec3(LastIndexedRecord.apply, LastIndexedRecord.unapply)("directoryPath", "lastUpdate", "lastIndexed")
}

case class RecordCandidate(directoryPath: AndroidPath, lastUpdated: Instant) {
  def toLastIndexedRecord: LastIndexedRecord = LastIndexedRecord(directoryPath, lastUpdated, None)
}
