package com.hydrangea.music.library

import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.{AndroidDirectory, AndroidFile, VirtualFile}
import com.hydrangea.music.library.index.IndexService
import com.hydrangea.music.tagger.Mp3agicAndroidTagger

object LibraryService {
  def updateIndex(device: Device, source: AndroidDirectory, forceOverwrite: Boolean = false): Seq[TrackRecord] =
    device.withCommandLine() { commandLine =>
      val records: Seq[TrackRecord] = Mp3agicAndroidTagger.tagDirectory(commandLine, source).map(_._2)
      IndexService.putAll(device, records, forceOverwrite)
    }
}
