package com.hydrangea.music.library.repository

import com.hydrangea.music.library.record.IndexRecordService

object RepositoryIndexRecordService extends IndexRecordService {
  override protected def fileTag: String = "repository"
}
