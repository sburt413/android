package com.hydrangea.music.library.repository

import com.hydrangea.android.file.{WindowsPath, WindowsRegularFile}
import com.hydrangea.music.TrackRecord
import com.hydrangea.music.library.record.{IndexRecord, LastIndexedRecord, Schedule, SynchronizationJob}
import com.hydrangea.music.library.IndexName
import com.hydrangea.music.tagger.TikaTagger

class RepositorySynchronizationJob(indexName: IndexName) extends SynchronizationJob[WindowsPath, WindowsRegularFile] {
  override def tag(file: WindowsRegularFile): TrackRecord = TikaTagger.tag(file.path)

  override def updateAndWriteIndex(record: IndexRecord[WindowsPath],
                                   updatedRecord: LastIndexedRecord[WindowsPath]): IndexRecord[WindowsPath] = {
    val updatedIndex: IndexRecord[WindowsPath] = record.updateRecord(updatedRecord)
    RepositoryIndexRecordService.writeRecord(indexName, updatedIndex)
    updatedIndex
  }
}

object RepositorySynchronizationJob {
  def run(schedule: Schedule[WindowsPath, WindowsRegularFile],
          indexRecord: IndexRecord[WindowsPath],
          indexName: IndexName): Unit = {
    new RepositorySynchronizationJob(indexName)
      .run(schedule, indexRecord, indexName)
  }
}
