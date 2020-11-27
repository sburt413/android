package com.hydrangea.music.library.record

import java.time.Instant

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.codec.Codecs._
import com.hydrangea.file.{AbsolutePath, FileData}

/**
  * An object representing the current state of an index of music files.  The record contains the last updated times of
  * the index documents and the source files.  The record can provide which files need to be indexed based on index and
  * update times.  The record can be updated by rescanning the source.
  *
  * @param rootDirectoryPath the root music directory on the device
  * @param childRecords      records of top level folders containing music
  */
case class IndexRecord(rootDirectoryPath: AbsolutePath, childRecords: List[LastIndexedRecord]) {

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
      .map(_.updateLastUpdated(candidate.lastUpdated))
      .getOrElse(candidate.toLastIndexedRecord)
  }

  /**
    * Merges all the given [[AndroidDirectory]] objects into this index and returns the updated value.
    *
    * @param candidates the values to merge into this index.
    * @return an updated [[IndexRecord]] containing the given directories
    */
  def addListings(candidates: List[RecordCandidate]): IndexRecord = {
    val directoriesRecords: List[LastIndexedRecord] =
      candidates.map(mergedRecord)

    updateRecords(directoriesRecords)
  }

  /**
    * Merges all the given directories into this index and discards any existing record that was not found in the given
    * directories.  Any records removed from the index will be returned with the updated index.
    *
    * @param candidates all the directories this index should contain and the most recent update time for it or any
    *                        ancestor
    * @return a new [[IndexRecord]] containing only the given directories and the discarded records
    */
  def reindex(candidates: List[RecordCandidate]): (IndexRecord, List[LastIndexedRecord]) = {
    val records: List[LastIndexedRecord] =
      candidates.map(mergedRecord)

    val candidatePathStrings: List[String] = candidates.map(_.directoryPath.raw)
    val excludedRecords: List[LastIndexedRecord] =
      childRecords.filterNot(record => candidatePathStrings.contains(record.directoryPath.raw))

    (IndexRecord(rootDirectoryPath, records), excludedRecords)
  }

  /**
    * Returns an updated [[IndexRecord]] containing the given record, replacing any existing record for that path.
    *
    * @param newChildRecord the record to update
    * @return an updated [[IndexRecord]] containing the given record
    */
  def updateRecord(newChildRecord: LastIndexedRecord): IndexRecord =
    updateRecords(List(newChildRecord))

  /**
    * Returns an updated [[IndexRecord]] containing the given records, replacing any existing records for that
    * path.
    *
    * @param newChildRecords the records to update
    * @return an updated [[IndexRecord]] containing the given records
    */
  def updateRecords(newChildRecords: List[LastIndexedRecord]): IndexRecord = {
    val newChildPaths: List[AbsolutePath] = newChildRecords.map(_.directoryPath)
    val updatedRecords: List[LastIndexedRecord] =
      childRecords.filterNot(existing => newChildPaths.contains(existing.directoryPath)) ++ newChildRecords
    IndexRecord(rootDirectoryPath, updatedRecords)
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
  def calculateUpdateSchedule: List[AbsolutePath] = needsUpdating.sortBy(_.lastUpdate).map(_.directoryPath)
}

object IndexRecord {
  def create(rootDirectoryPath: AbsolutePath, candidates: List[RecordCandidate]): IndexRecord = {
    val records: List[LastIndexedRecord] = candidates.map(_.toLastIndexedRecord)
    IndexRecord(rootDirectoryPath, records)
  }

  private val rootDirectoryPathKey = "rootDirectoryPath"
  private val childRecordsKey = "childRecords"

  implicit def codec: CodecJson[IndexRecord] =
    casecodec2(IndexRecord.apply, IndexRecord.unapply)(rootDirectoryPathKey, childRecordsKey)

//  implicit def encodeJson[P <: VirtualPath](implicit encodePath: EncodeJson): EncodeJson[IndexRecord] =
//    EncodeJson(record =>
//      (rootDirectoryPathKey := record.rootDirectoryPath) ->: (childRecordsKey := record.childRecords) ->: jEmptyObject)
//
//  implicit def decodeJson[P <: VirtualPath](implicit decodePath: DecodeJson): DecodeJson[IndexRecord] =
//    DecodeJson(record =>
//      for {
//        rootDirectoryPath <- (record --\ rootDirectoryPathKey).as
//        childRecords <- (record --\ childRecordsKey).as[List[LastIndexedRecord]]
//      } yield IndexRecord(rootDirectoryPath, childRecords))
}

//object IndexRecord {
//  def create(rootDirectory: AndroidDirectory, candidates: List[RecordCandidate[AndroidPath]]): IndexRecord[AndroidPath] = {
//    val records: List[LastIndexedRecord[AndroidPath]] = candidates.map(_.toLastIndexedRecord)
//    new IndexRecord(rootDirectory.path, records)
//  }
//
//  def create(rootDirectory: WindowsDirectory, candidates: List[RecordCandidate[WindowsPath]]): IndexRecord[WindowsPath] = {
//    val records: List[LastIndexedRecord[WindowsPath]] = candidates.map(_.toLastIndexedRecord)
//    new IndexRecord(rootDirectory.path, records)
//  }
//
//  implicit def androidCodex: CodecJson[IndexRecord[AndroidPath]] =
//    casecodec2(IndexRecord.apply[AndroidPath], IndexRecord.unapply[AndroidPath])("rootDirectoryPath", "childRecords")
//
//  implicit def windowsCodex: CodecJson[IndexRecord[WindowsPath]] =
//    casecodec2(IndexRecord.apply[WindowsPath], IndexRecord.unapply[WindowsPath])("rootDirectoryPath", "childRecords")
//}

/**
  * A record of when was the last time a top level folder was both updated on the device and updated in the Elasticsearch
  * index.  The record contains only the most recent update time of any descendant in the file tree.
  *
  * @param directoryPath the path to the folder
  * @param fileCount     the number of known files under the folder
  * @param lastUpdate    the latest most recent update time of files under the directory
  * @param lastIndexed   the last time documents for this directory were indexed in Elasticsearch
  */
case class LastIndexedRecord(directoryPath: AbsolutePath,
                             fileCount: Int,
                             lastUpdate: Instant,
                             lastIndexed: Option[Instant]) {
  def needsUpdate: Boolean = lastIndexed.map(indexed => lastUpdate.isAfter(indexed)).getOrElse(true)

  /**
    * Returns an updated record with the most recent of either this record's lastUpdate or the given modify time.
    *
    * @param modifyTime the potential new update time
    * @return an updated record with the most recent update time
    */
  def updateLastUpdated(modifyTime: Instant): LastIndexedRecord = {
    val newLastUpdate: Instant = Seq(lastUpdate, modifyTime).max
    copy(lastUpdate = newLastUpdate)
  }

  /**
    * Returns an updated record with the most recent of either this record's lastIndexed or the given index time.
    *
    * @param indexedTime the potential new index time
    * @return an updated record with the most recent index time
    */
  def updateLastIndexed(indexedTime: Instant): LastIndexedRecord = {
    val newLastIndexed: Instant = Seq(lastIndexed.getOrElse(Instant.EPOCH), indexedTime).max
    copy(lastIndexed = Some(newLastIndexed))
  }

  def updateFileCount(fileCount: Int): LastIndexedRecord =
    copy(fileCount = fileCount)

  /**
    * Returns whether we need to update the elasticsearch index for the given file.  We need to update the file if it
    * was last modified after we last indexed it, or if we have never indexed it.
    *
    * @param file the file to check
    * @return whether we currently need to index the file
    */
  def needsIndexing(file: FileData): Boolean =
    lastIndexed.map(last => file.modifyTime.isAfter(last)).getOrElse(true)
}

object LastIndexedRecord {
  implicit def codex: CodecJson[LastIndexedRecord] =
    casecodec4(LastIndexedRecord.apply, LastIndexedRecord.unapply)("directoryPath",
                                                                   "fileCount",
                                                                   "lastUpdate",
                                                                   "lastIndexed")
}

case class RecordCandidate(directoryPath: AbsolutePath, fileCount: Int, lastUpdated: Instant) {
  def toLastIndexedRecord: LastIndexedRecord = LastIndexedRecord(directoryPath, fileCount, lastUpdated, None)
}
