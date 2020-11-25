package com.hydrangea.music.library.record

import java.time.Instant

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.android.file._
import com.hydrangea.codec.Codecs._

/**
  * An object representing the current state of an index of music files.  The record contains the last updated times of
  * the index documents and the source files.  The record can provide which files need to be indexed based on index and
  * update times.  The record can be updated by rescanning the source.
  *
  * @param rootDirectoryPath the root music directory on the device
  * @param childRecords      records of top level folders containing music
  */
case class IndexRecord[P <: VirtualPath](val rootDirectoryPath: P, val childRecords: List[LastIndexedRecord[P]]) {

  /**
    * Merges the given [[RecordCandidate]] with any existing record.  If any record exists, then the most recent
    * modified time between the existing record and candidate is used for the merged record.  Otherwise, a new record is
    * created and returned.
    *
    * @param candidate the candidate record
    * @return the updated [[LastIndexedRecord]]; otherwise, a new record for the directory
    */
  private def mergedRecord(candidate: RecordCandidate[P]): LastIndexedRecord[P] = {
    val existingRecordForDir: Option[LastIndexedRecord[P]] =
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
  def addListings(candidates: List[RecordCandidate[P]]): IndexRecord[P] = {
    val directoriesRecords: List[LastIndexedRecord[P]] =
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
  def reindex(candidates: List[RecordCandidate[P]]): (IndexRecord[P], List[LastIndexedRecord[P]]) = {
    val records: List[LastIndexedRecord[P]] =
      candidates.map(mergedRecord)

    val candidatePathStrings: List[String] = candidates.map(_.directoryPath.raw)
    val excludedRecords: List[LastIndexedRecord[P]] =
      childRecords.filterNot(record => candidatePathStrings.contains(record.directoryPath.raw))

    (IndexRecord(rootDirectoryPath, records), excludedRecords)
  }

  /**
    * Returns an updated [[IndexRecord]] containing the given record, replacing any existing record for that path.
    *
    * @param newChildRecord the record to update
    * @return an updated [[IndexRecord]] containing the given record
    */
  def updateRecord(newChildRecord: LastIndexedRecord[P]): IndexRecord[P] =
    updateRecords(List(newChildRecord))

  /**
    * Returns an updated [[IndexRecord]] containing the given records, replacing any existing records for that
    * path.
    *
    * @param newChildRecords the records to update
    * @return an updated [[IndexRecord]] containing the given records
    */
  def updateRecords(newChildRecords: List[LastIndexedRecord[P]]): IndexRecord[P] = {
    val newChildPaths: List[P] = newChildRecords.map(_.directoryPath)
    val updatedRecords: List[LastIndexedRecord[P]] =
      childRecords.filterNot(existing => newChildPaths.contains(existing.directoryPath)) ++ newChildRecords
    IndexRecord(rootDirectoryPath, updatedRecords)
  }

  /**
    * Returns all records for which the source directory was more recently updated than this index.
    *
    * @return all records for which the source directory was more recently updated than this index
    */
  def needsUpdating: List[LastIndexedRecord[P]] = childRecords.filter(_.needsUpdate)

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
  def calculateUpdateSchedule: List[P] = needsUpdating.sortBy(_.lastUpdate).map(_.directoryPath)
}

object IndexRecord {
  def create[P <: VirtualPath](rootDirectoryPath: P, candidates: List[RecordCandidate[P]]): IndexRecord[P] = {
    val records: List[LastIndexedRecord[P]] = candidates.map(_.toLastIndexedRecord)
    IndexRecord(rootDirectoryPath, records)
  }

  type DeviceIndexRecord = IndexRecord[AndroidPath]
  type RepositoryIndexRecord = IndexRecord[WindowsPath]

  private val rootDirectoryPathKey = "rootDirectoryPath"
  private val childRecordsKey = "childRecords"

  def codec[P <: VirtualPath](implicit encodePath: EncodeJson[P],
                              decodeJson: DecodeJson[P]): CodecJson[IndexRecord[P]] =
    casecodec2(IndexRecord.apply[P], IndexRecord.unapply[P])(rootDirectoryPathKey, childRecordsKey)

  implicit val androidCodec: CodecJson[IndexRecord[AndroidPath]] = codec[AndroidPath]

  implicit val windowsCodec: CodecJson[IndexRecord[WindowsPath]] = codec[WindowsPath]

//  implicit def encodeJson[P <: VirtualPath](implicit encodePath: EncodeJson[P]): EncodeJson[IndexRecord[P]] =
//    EncodeJson(record =>
//      (rootDirectoryPathKey := record.rootDirectoryPath) ->: (childRecordsKey := record.childRecords) ->: jEmptyObject)
//
//  implicit def decodeJson[P <: VirtualPath](implicit decodePath: DecodeJson[P]): DecodeJson[IndexRecord[P]] =
//    DecodeJson(record =>
//      for {
//        rootDirectoryPath <- (record --\ rootDirectoryPathKey).as[P]
//        childRecords <- (record --\ childRecordsKey).as[List[LastIndexedRecord[P]]]
//      } yield IndexRecord[P](rootDirectoryPath, childRecords))
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
case class LastIndexedRecord[P <: VirtualPath](directoryPath: P,
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
  def updateLastUpdated(modifyTime: Instant): LastIndexedRecord[P] = {
    val newLastUpdate: Instant = Seq(lastUpdate, modifyTime).max
    copy(lastUpdate = newLastUpdate)
  }

  /**
    * Returns an updated record with the most recent of either this record's lastIndexed or the given index time.
    *
    * @param indexedTime the potential new index time
    * @return an updated record with the most recent index time
    */
  def updateLastIndexed(indexedTime: Instant): LastIndexedRecord[P] = {
    val newLastIndexed: Instant = Seq(lastIndexed.getOrElse(Instant.EPOCH), indexedTime).max
    copy(lastIndexed = Some(newLastIndexed))
  }

  def updateFileCount(fileCount: Int): LastIndexedRecord[P] =
    copy(fileCount = fileCount)

  /**
    * Returns whether we need to update the elasticsearch index for the given file.  We need to update the file if it
    * was last modified after we last indexed it, or if we have never indexed it.
    *
    * @param file the file to check
    * @return whether we currently need to index the file
    */
  def needsIndexing(file: VirtualFile): Boolean =
    lastIndexed.map(last => file.modifyTime.isAfter(last)).getOrElse(true)
}

object LastIndexedRecord {
  implicit def codex[P <: VirtualPath](implicit encodeJson: EncodeJson[P],
                                       decodeJson: DecodeJson[P]): CodecJson[LastIndexedRecord[P]] =
    casecodec4(LastIndexedRecord.apply[P], LastIndexedRecord.unapply[P])("directoryPath",
                                                                         "fileCount",
                                                                         "lastUpdate",
                                                                         "lastIndexed")

  implicit val androidCodex: CodecJson[LastIndexedRecord[AndroidPath]] = codex[AndroidPath]

  implicit val windowsCodex: CodecJson[LastIndexedRecord[WindowsPath]] = codex[WindowsPath]
}

case class RecordCandidate[P <: VirtualPath](directoryPath: P, fileCount: Int, lastUpdated: Instant) {
  def toLastIndexedRecord: LastIndexedRecord[P] = LastIndexedRecord[P](directoryPath, fileCount, lastUpdated, None)
}
