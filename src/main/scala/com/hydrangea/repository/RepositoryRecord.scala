package com.hydrangea.repository

import com.hydrangea.file.RelativePath
import com.hydrangea.music.track.Track

case class RepositoryRecord(path: RelativePath, track: Track)
