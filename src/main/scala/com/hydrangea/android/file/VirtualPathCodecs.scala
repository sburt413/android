package com.hydrangea.android.file

import argonaut._
import Argonaut._
import com.hydrangea.codec.Codecs._

trait VirtualPathCodecs {

  private final val android = "a"
  private final val windows = "w"

  implicit def encodeAndroidPath: EncodeJson[AndroidPath] =
    path => Json("_type" := android, "raw" := path.raw)

  implicit def encodeWindowsPath: EncodeJson[WindowsPath] =
    path => Json("_type" := windows, "raw" := path.raw)

  implicit def encode: EncodeJson[VirtualPath] = {
    case androidPath: AndroidPath => encodeAndroidPath(androidPath)
    case windowsPath: WindowsPath => encodeWindowsPath(windowsPath)
  }

  implicit def decodeAndroidPath: DecodeJson[AndroidPath] =
    cursor => {
      def validateType(typ: String, history: CursorHistory): DecodeResult[String] = typ match {
        case `android` => DecodeResult(Right(typ))
        case _         => DecodeResult.fail(s"Unknown path type: ${typ}", history)
      }

      for {
        typ <- (cursor --\ "_type").as[String]
        _ <- validateType(typ, cursor.history)
        raw <- (cursor --\ "raw").as[String]
      } yield AndroidPath(raw)
    }

  implicit def decodeWindowsPath: DecodeJson[WindowsPath] =
    cursor => {
      def validateType(typ: String, history: CursorHistory): DecodeResult[String] = typ match {
        case `windows` => DecodeResult(Right(typ))
        case _         => DecodeResult.fail(s"Unknown path type: ${typ}", history)
      }

      for {
        typ <- (cursor --\ "_type").as[String]
        _ <- validateType(typ, cursor.history)
        raw <- (cursor --\ "raw").as[String]
      } yield WindowsPath(raw)
    }

  implicit def decode: DecodeJson[VirtualPath] = { cursor =>
    {
      def decode(typ: String): DecodeResult[VirtualPath] =
        typ match {
          case `android` => decodeAndroidPath.decode(cursor).map(_.asInstanceOf[VirtualPath])
          case `windows` => decodeWindowsPath.decode(cursor).map(_.asInstanceOf[VirtualPath])
          case typ       => DecodeResult.fail[VirtualPath](s"Unknown path type: $typ", cursor.history)
        }

      for {
        typ <- (cursor --\ "_type").as[String]
        path <- decode(typ)
      } yield path
    }
  }

  implicit def androidCodex: CodecJson[AndroidPath] = CodecJson(encodeAndroidPath.encode, decodeAndroidPath.decode)
  implicit def windowsCodex: CodecJson[WindowsPath] = CodecJson(encodeWindowsPath.encode, decodeWindowsPath.decode)
  implicit def codex: CodecJson[VirtualPath] = CodecJson(encode.encode, decode.decode)
}
