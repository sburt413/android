package com.hydrangea.music.track

import com.hydrangea.android.adb.ADBService
import com.hydrangea.file.{AndroidRegularFileData, FileSystemService, LocalRegularFileData, RegularFileData}
import com.hydrangea.music.tagger.TikaTagger
import javax.inject.Inject
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.digest.MessageDigestAlgorithms.SHA_1
import scalaz.Disjunction

class TrackService @Inject()(adbService: ADBService, tagger: TikaTagger, fileSystemService: FileSystemService) {
  def readTrack(file: RegularFileData): Track =
    file match {
      case local: LocalRegularFileData     => getLocalTrack(local)
      case android: AndroidRegularFileData => readAndroidTrack(android)
    }

  private def getLocalTrack(file: LocalRegularFileData): Track = {
    val sha1 = new DigestUtils(SHA_1)
    val hashOutput: Disjunction[String, String] =
      fileSystemService.read(file.location) { inputStream =>
        Hex.encodeHexString(DigestUtils.digest(sha1.getMessageDigest, inputStream))
      }

    val hash: String =
      hashOutput.fold(failure => throw new IllegalArgumentException(s"Could not has file (${file.location}): $failure"),
                      hexHash => hexHash)

    Track(hash, file.location.path, file.modifyTime, tagger.tag(file.location))
  }

  private def readAndroidTrack(file: AndroidRegularFileData): Track =
    adbService.withCommandLine(file.location.device) { commandLine =>
      val hash: String = commandLine.sha1sum(file.location.path)
      val trackTag: Tag = tagger.tag(file.location)

      val path = file.location.path
      Track(hash, path, file.modifyTime, trackTag)
    }
}
