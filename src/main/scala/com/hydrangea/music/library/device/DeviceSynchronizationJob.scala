package com.hydrangea.music.library.device

import java.time.Instant

import com.hydrangea.android.adb.Device
import com.hydrangea.file.AndroidRegularFileData
import com.hydrangea.music.library.record.{IndexRecord, LastIndexedRecord, Schedule, SynchronizationJob}
import com.hydrangea.music.library.{IndexName, TrackRecord}
import com.hydrangea.music.tagger.TikaTagger
import com.hydrangea.music.track.{Tag, Track}

class DeviceSynchronizationJob(device: Device, indexName: IndexName)
    extends SynchronizationJob[AndroidRegularFileData] {
  override def tag(file: AndroidRegularFileData): TrackRecord =
    device.withCommandLine() { commandLine =>
      val hash: String = commandLine.sha1sum(file.location.path)
      val trackTag: Tag = TikaTagger.tag(file.location)

      val path = file.location.path
      val track: Track = Track(hash, path, file.modifyTime, trackTag)
      TrackRecord(track, Instant.now())
    }

  override def updateAndWriteIndex(record: IndexRecord, updatedRecord: LastIndexedRecord): IndexRecord = {
    val updatedIndex: IndexRecord = record.updateRecord(updatedRecord)
    DeviceIndexRecordService.writeRecord(indexName, updatedIndex)
    updatedIndex
  }
}

object DeviceSynchronizationJob {
  def run(device: Device,
          schedule: Schedule[AndroidRegularFileData],
          indexRecord: IndexRecord,
          indexName: IndexName): Unit = {
    new DeviceSynchronizationJob(device, indexName)
      .run(schedule, indexRecord, indexName)
  }
}
