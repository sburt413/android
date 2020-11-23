package com.hydrangea.music.repository

import com.hydrangea.file.AbsolutePath
import com.hydrangea.music.track.Track

trait Repository[P <: AbsolutePath] {
  def basePath: P
  def tracks: Seq[Track]

}
