package com.hydrangea.music.track

import com.hydrangea.android.adb.ADBService
import com.hydrangea.file.{AndroidRegularFileData, LocalRegularFileData, RegularFileData}
import com.hydrangea.music.tagger.TikaTagger
import com.hydrangea.process.{CLIProcessFactory, DefaultCLIProcessFactory}
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1

class TrackService(adbService: ADBService, tagger: TikaTagger) {
  def readTrack(file: RegularFileData): Track =
    file match {
      case local: LocalRegularFileData     => getLocalTrack(local)
      case android: AndroidRegularFileData => readAndroidTrack(android)
    }

  def getLocalTrack(file: LocalRegularFileData): Track = {
    val sha1 = new DigestUtils(SHA_1)
    val hash: String = sha1.digestAsHex(file.location.toJavaPath.toFile)

    Track(hash, file.location.path, file.modifyTime, tagger.tag(file.location))
  }

  def readAndroidTrack(file: AndroidRegularFileData): Track =
    adbService.withCommandLine(file.location.device) { commandLine =>
      val hash: String = commandLine.sha1sum(file.location.path)
      val trackTag: Tag = tagger.tag(file.location)

      val path = file.location.path
      Track(hash, path, file.modifyTime, trackTag)
    }
}

object TrackService {
  def apply(adbService: ADBService, tikaTagger: TikaTagger): TrackService =
    new TrackService(adbService, tikaTagger)

  def default(cliProcessFactory: CLIProcessFactory = DefaultCLIProcessFactory.instance): TrackService =
    apply(ADBService.default(cliProcessFactory), TikaTagger.default(cliProcessFactory))
}
