package com.hydrangea.music.library.repository

//case class RepositoryIndexRecord(rootDirectoryPath: WindowsPath, childRecords: List[LastIndexedRecord[WindowsPath]])
//    extends IndexRecord[WindowsPath](rootDirectoryPath, childRecords) {
//
//  override type Self = RepositoryIndexRecord
//
//  override protected def build(rootDirectoryPath: WindowsPath,
//                               childRecords: List[LastIndexedRecord[WindowsPath]]): RepositoryIndexRecord =
//    RepositoryIndexRecord(rootDirectoryPath, childRecords)
//}

//object RepositoryIndexRecord {
//  def create(rootDirectory: WindowsDirectory, candidates: List[RecordCandidate[WindowsPath]]): RepositoryIndexRecord = {
//    val records: List[LastIndexedRecord[WindowsPath]] = candidates.map(_.toLastIndexedRecord)
//    new RepositoryIndexRecord(rootDirectory.path, records)
//  }
//
//  implicit def codex: CodecJson[RepositoryIndexRecord] =
//    casecodec2(RepositoryIndexRecord.apply, RepositoryIndexRecord.unapply)("rootDirectoryPath", "childRecords")
//}
