package com.hydrangea.music.library.repository

import java.time.Instant

import com.hydrangea.file.LocalRegularFileData
import com.hydrangea.music.library.record.{IndexRecord, LastIndexedRecord, Schedule, SynchronizationJob}
import com.hydrangea.music.library.{IndexName, TrackRecord}
import com.hydrangea.music.track.TrackService

class RepositorySynchronizationJob(indexName: IndexName) extends SynchronizationJob[LocalRegularFileData] {
  override def tag(file: LocalRegularFileData): TrackRecord =
    TrackRecord(TrackService.getLocalTrack(file), Instant.now())

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
