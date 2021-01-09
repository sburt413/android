package com.hydrangea.repository

import java.io.StringWriter
import java.nio.charset.Charset
import java.nio.file.Path

import argonaut.Argonaut._
import argonaut._
import com.google.inject.Inject
import com.hydrangea.android.adb.ADBCommandLine
import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, FileLocation, FileSystemService, LocalFileLocation}
import org.apache.commons.io.IOUtils
import scalaz.Disjunction

class MusicRepositoryService @Inject()(fileSystemService: FileSystemService) {
  def persist[L <: FileLocation: EncodeJson: DecodeJson](repository: MusicRepository[L], recordFilePath: Path): Unit = {
    val repositoryRecordPath: AbsolutePath =
      recordFilePath.toAbsolutePath.toString.toAbsolutePath
        .getOrElse(throw new IllegalArgumentException(recordFilePath + " is not a parsable path"))

    val data: Array[Byte] = repository.asJson.toString().getBytes(MusicRepositoryService.encoding)
    fileSystemService.write(LocalFileLocation(repositoryRecordPath), data);
  }

  def load[L <: FileLocation: EncodeJson: DecodeJson](recordFilePath: Path): Option[MusicRepository[L]] = {
    val repositoryRecordPath: AbsolutePath =
      recordFilePath.toAbsolutePath.toString.toAbsolutePath
        .getOrElse(throw new IllegalArgumentException(recordFilePath + " is not a parsable path"))

    val location: LocalFileLocation = LocalFileLocation(repositoryRecordPath)
    val readResult: Disjunction[String, Option[MusicRepository[L]]] =
      fileSystemService.read(location) { inputStream =>
        val stringWriter: StringWriter = new StringWriter();
        IOUtils.copy(inputStream, stringWriter, MusicRepositoryService.encoding)
        val jsonStr: JsonField = stringWriter.toString
        jsonStr.decodeOption[MusicRepository[L]]
      }

    readResult.toOption.flatten
  }
}

object MusicRepositoryService {
  private val encoding: Charset = ADBCommandLine.UTF_8
}
