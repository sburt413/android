package com.hydrangea.music.library.repository

import com.hydrangea.android.file.WindowsPath
import com.hydrangea.music.library.record.{IndexRecord, IndexRecordService}

object RepositoryIndexRecordService extends IndexRecordService[WindowsPath, IndexRecord[WindowsPath]] {
  override protected def fileTag: String = "repository"
}
