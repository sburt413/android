package com.hydrangea.music.library.device

//case class DeviceIndexRecord(override val rootDirectoryPath: AndroidPath,
//                             override val childRecords: List[LastIndexedRecord[AndroidPath]])
//    extends IndexRecord[AndroidPath](rootDirectoryPath, childRecords) {
//  override type Self = DeviceIndexRecord
//
//  override protected def build(rootDirectoryPath: AndroidPath,
//                               childRecords: List[LastIndexedRecord[AndroidPath]]): DeviceIndexRecord =
//    DeviceIndexRecord(rootDirectoryPath, childRecords)
//}

//object DeviceIndexRecord {
//  def create(rootDirectory: AndroidDirectory, candidates: List[RecordCandidate[AndroidPath]]): DeviceIndexRecord = {
//    val records: List[LastIndexedRecord[AndroidPath]] = candidates.map(_.toLastIndexedRecord)
//    new DeviceIndexRecord(rootDirectory.path, records)
//  }

//  implicit def codex: CodecJson[IndexRecord[AndroidPath]] =
//    (DeviceIndexRecord.apply, DeviceIndexRecord.unapply)("rootDirectoryPath", "childRecords")
//}
