package com.hydrangea.music.library.repository

import java.time.Instant

import com.hydrangea.android.file.AndroidPath
import com.hydrangea.file.LocalRegularFileData
import com.hydrangea.music.library.record.{IndexRecord, LastIndexedRecord, Schedule, SynchronizationJob}
import com.hydrangea.music.library.{IndexName, TrackRecord}
import com.hydrangea.music.tagger.TikaTagger
import com.hydrangea.music.track.Tag
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1

class RepositorySynchronizationJob(indexName: IndexName) extends SynchronizationJob[LocalRegularFileData] {
  override def tag(file: LocalRegularFileData): TrackRecord = {
    val trackTag: Tag = TikaTagger.tag(file.location)

    val sha1 = new DigestUtils(SHA_1)
    val hash: String = sha1.digestAsHex(file.location.toJavaPath.toFile)

    // TODO: Uses old path
    val path: AndroidPath = AndroidPath(file.location.path.raw)
    TrackRecord(hash, path, file.modifyTime, Instant.now(), trackTag)
  }

  override def updateAndWriteIndex(record: IndexRecord, updatedRecord: LastIndexedRecord): IndexRecord = {
    val updatedIndex: IndexRecord = record.updateRecord(updatedRecord)
    RepositoryIndexRecordService.writeRecord(indexName, updatedIndex)
    updatedIndex
  }
}

object RepositorySynchronizationJob {
  def run(schedule: Schedule[LocalRegularFileData], indexRecord: IndexRecord, indexName: IndexName): Unit = {
    new RepositorySynchronizationJob(indexName)
      .run(schedule, indexRecord, indexName)
  }
}
