package com.hydrangea

import java.io.StringWriter
import java.nio.charset.Charset
import java.nio.file.Path

import argonaut.Argonaut.{JsonField, _}
import argonaut.{DecodeJson, EncodeJson}
import com.hydrangea.android.adb.ADBCommandLine
import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, FileSystemService, LocalFileLocation}
import org.apache.commons.io.IOUtils
import scalaz.Disjunction

abstract class AbstractDAO(fileSystemService: FileSystemService) {
  protected def persistObj[T](obj: T, recordFilePath: Path)(implicit encodeJson: EncodeJson[T],
                                                            decodeJson: DecodeJson[T]): Unit = {
    val recordPath: AbsolutePath =
      recordFilePath.toAbsolutePath.toString.toAbsolutePath
        .getOrElse(throw new IllegalArgumentException(recordFilePath + " is not a parsable path"))

    val data: Array[Byte] = obj.asJson.toString().getBytes(AbstractDAO.encoding)
    fileSystemService.write(LocalFileLocation(recordPath), data);
  }

  protected def loadObj[T](recordFilePath: Path)(implicit encodeJson: EncodeJson[T],
                                                 decodeJson: DecodeJson[T]): Option[T] = {
    val recordPath: AbsolutePath =
      recordFilePath.toAbsolutePath.toString.toAbsolutePath
        .getOrElse(throw new IllegalArgumentException(recordFilePath + " is not a parsable path"))

    val location: LocalFileLocation = LocalFileLocation(recordPath)
    val readResult: Disjunction[String, Option[T]] =
      fileSystemService.read(location) { inputStream =>
        val stringWriter: StringWriter = new StringWriter();
        IOUtils.copy(inputStream, stringWriter, AbstractDAO.encoding)
        val jsonStr: JsonField = stringWriter.toString
        jsonStr.decodeOption[T]
      }

    readResult.toOption.flatten
  }
}

object AbstractDAO {
  private val encoding: Charset = ADBCommandLine.UTF_8
}
