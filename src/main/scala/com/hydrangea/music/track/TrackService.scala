package com.hydrangea.music.track

import com.hydrangea.file.{AndroidRegularFileData, LocalRegularFileData, RegularFileData}
import com.hydrangea.music.tagger.TikaTagger
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1

object TrackService {
  def readTrack(file: RegularFileData): Track =
    file match {
      case local: LocalRegularFileData     => getLocalTrack(local)
      case android: AndroidRegularFileData => readAndroidTrack(android)
    }

  def getLocalTrack(file: LocalRegularFileData): Track = {
    val sha1 = new DigestUtils(SHA_1)
    val hash: String = sha1.digestAsHex(file.location.toJavaPath.toFile)

    Track(hash, file.location.path, file.modifyTime, TikaTagger.tag(file.location))
  }

  def readAndroidTrack(file: AndroidRegularFileData): Track =
    file.location.device.withCommandLine() { commandLine =>
      val hash: String = commandLine.sha1sum(file.location.path)
      val trackTag: Tag = TikaTagger.tag(file.location)

      val path = file.location.path
      Track(hash, path, file.modifyTime, trackTag)
    }
}
