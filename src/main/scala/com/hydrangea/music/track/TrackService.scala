package com.hydrangea.music.track

import com.hydrangea.file.LocalFileData
import com.hydrangea.music.tagger.TikaTagger
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1

object TrackService {
  def getLocalTrack(file: LocalFileData): Track = {
    val sha1 = new DigestUtils(SHA_1)
    val hash: String = sha1.digestAsHex(file.location.toJavaPath.toFile)

    Track(hash, file.location.path, file.modifyTime, TikaTagger.tag(file.location))
  }
}
