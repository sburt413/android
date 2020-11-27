package com.hydrangea.music.library.device

import com.hydrangea.music.library.record.IndexRecordService

object DeviceIndexRecordService extends IndexRecordService {
  override protected def fileTag: String = "device"
}
