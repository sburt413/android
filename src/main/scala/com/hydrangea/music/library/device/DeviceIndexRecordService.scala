package com.hydrangea.music.library.device

import com.hydrangea.android.file.AndroidPath
import com.hydrangea.music.library.record.{IndexRecord, IndexRecordService}

object DeviceIndexRecordService extends IndexRecordService[AndroidPath, IndexRecord[AndroidPath]] {
  override protected def fileTag: String = "device"
}
