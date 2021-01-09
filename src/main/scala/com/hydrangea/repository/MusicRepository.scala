package com.hydrangea.repository

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.file.FileLocation

/**
  * A [[MusicRepository]] is the snapshot of current known state of a directory on an arbitrary file system that contain
  * music in the directory tree.  Each record contain the metadata about a known file in the repository.
  *
  * @param root    the root directory under which all files are found
  * @param records the currently known files under the {{root}}
  * @tparam L      the type of location of the files
  */
case class MusicRepository[L <: FileLocation](root: L, records: List[RepositoryRecord]) {

  /**
    * Returns the actual [[FileLocation]] of the file for a record in this repository.
    *
    * @param record the record to fetch the [[FileLocation]] for
    * @return the [[FileLocation]] for the given record in the repository
    */
  def recordLocation(record: RepositoryRecord): FileLocation =
    root ++ record.path

  def removeDirectory(location: FileLocation): MusicRepository[L] = {
    val remainingRecords: List[RepositoryRecord] =
      records.filterNot(record => recordLocation(record).startsWith(location))
    MusicRepository(root, remainingRecords)
  }

  def addRecords(records: Seq[RepositoryRecord]): MusicRepository[L] =
    copy(records = this.records ++ records)
}

object MusicRepository {
  def empty[L <: FileLocation](location: L): MusicRepository[L] = MusicRepository(location, Nil)

  implicit def codec[L <: FileLocation: EncodeJson: DecodeJson]: CodecJson[MusicRepository[L]] = {
    def create(location: L, records: List[RepositoryRecord]): MusicRepository[L] = MusicRepository(location, records)
    def deconstruct(repository: MusicRepository[L]): Option[(L, List[RepositoryRecord])] =
      Some(repository.root, repository.records)

    casecodec2(create, deconstruct)("location", "records")
  }
}
