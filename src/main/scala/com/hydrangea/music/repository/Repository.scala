package com.hydrangea.music.repository

import com.hydrangea.file.AbsolutePath

trait Repository[P <: AbsolutePath] {
  val basePath: P
//  val index: RespositoryIndex
//  def locate(tracks: Set[Track]): Set[TrackReference]
}
