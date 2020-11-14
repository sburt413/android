package com.hydrangea.music.library.device

import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.{AndroidPath, AndroidRegularFile}
import com.hydrangea.music.TrackRecord
import com.hydrangea.music.library.record.{IndexRecord, LastIndexedRecord, Schedule, SynchronizationJob}
import com.hydrangea.music.library.IndexName
import com.hydrangea.music.tagger.TikaTagger

class DeviceSynchronizationJob(device: Device, indexName: IndexName)
    extends SynchronizationJob[AndroidPath, AndroidRegularFile] {
  override def tag(file: AndroidRegularFile): TrackRecord =
    device.withCommandLine() { commandLine =>
      TikaTagger.tag(commandLine, file)
    }

  override def updateAndWriteIndex(record: IndexRecord[AndroidPath],
                                   updatedRecord: LastIndexedRecord[AndroidPath]): IndexRecord[AndroidPath] = {
    val updatedIndex: IndexRecord[AndroidPath] = record.updateRecord(updatedRecord)
    DeviceIndexRecordService.writeRecord(indexName, updatedIndex)
    updatedIndex
  }
}

object DeviceSynchronizationJob {
  def run(device: Device,
          schedule: Schedule[AndroidPath, AndroidRegularFile],
          indexRecord: IndexRecord[AndroidPath],
          indexName: IndexName): Unit = {
    new DeviceSynchronizationJob(device, indexName)
      .run(schedule, indexRecord, indexName)
  }
}
