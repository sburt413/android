package com.hydrangea.file

import java.nio.file.Path

import argonaut.Argonaut._
import argonaut._
import com.hydrangea.android.adb.Device

/**
  * A data structure containing enough information to retrieve a file.
  */
sealed trait FileLocation {
  type thisType >: this.type <: FileLocation

  def path: AbsolutePath

  def ++(relativePath: RelativePath): thisType
}

object FileLocation {
  final val localKey: String = "l"
  final val androidKey: String = "a"

  implicit def encode: EncodeJson[FileLocation] = {
    case local: LocalFileLocation => LocalFileLocation.encode(local)
    case android: AndroidLocation => AndroidLocation.encode(android)
  }

  implicit def decode: DecodeJson[FileLocation] = { cursor =>
    {
      def decode(key: String): DecodeResult[FileLocation] =
        key match {
          case `localKey`   => LocalFileLocation.decode(cursor).map(_.asInstanceOf[FileLocation])
          case `androidKey` => AndroidLocation.decode(cursor).map(_.asInstanceOf[FileLocation])
          case typ          => DecodeResult.fail[FileLocation](s"Unknown location type: $typ", cursor.history)
        }

      for {
        typ <- (cursor --\ "_type").as[String]
        path <- decode(typ)
      } yield path
    }
  }
}

case class LocalFileLocation(path: AbsolutePath) extends FileLocation {
  override type thisType = LocalFileLocation

  override def ++(relativePath: RelativePath): LocalFileLocation =
    LocalFileLocation(path ++ relativePath)

  def toJavaPath: Path = Path.of(path.raw)
}

object LocalFileLocation {
  def apply(javaPath: Path): Option[LocalFileLocation] = AbsolutePath(javaPath).map(LocalFileLocation(_))

  implicit def encode: EncodeJson[LocalFileLocation] =
    location => Json("_type" := FileLocation.localKey, "path" := location.path)
  implicit def decode: DecodeJson[LocalFileLocation] =
    cursor => {
      def validateType(key: String, history: CursorHistory): DecodeResult[String] = key match {
        case FileLocation.`localKey` => DecodeResult(Right(key))
        case _                       => DecodeResult.fail(s"Unknown location type: ${key}", history)
      }

      for {
        typ <- (cursor --\ "_type").as[String]
        _ <- validateType(typ, cursor.history)
        path <- (cursor --\ "path").as[AbsolutePath]
      } yield LocalFileLocation(path)
    }
}

case class AndroidLocation(device: Device, path: AbsolutePath) extends FileLocation {
  override type thisType = AndroidLocation

  override def ++(relativePath: RelativePath): AndroidLocation =
    AndroidLocation(device, path ++ relativePath)
}

object AndroidLocation {
  implicit def encode: EncodeJson[AndroidLocation] =
    location => Json("_type" := FileLocation.androidKey, "device" := location.device, "path" := location.path)
  implicit def decode: DecodeJson[AndroidLocation] =
    cursor => {
      def validateType(key: String, history: CursorHistory): DecodeResult[String] =
        key match {
          case FileLocation.`androidKey` => DecodeResult(Right(key))
          case _                         => DecodeResult.fail(s"Unknown location type: ${key}", history)
        }

      for {
        typ <- (cursor --\ "_type").as[String]
        _ <- validateType(typ, cursor.history)
        device <- (cursor --\ "device").as[Device]
        path <- (cursor --\ "path").as[AbsolutePath]
      } yield AndroidLocation(device, path)
    }
}
