package com.hydrangea.android.file

import java.nio.file.{Path, Paths}
import java.time.Instant

import argonaut._
import Argonaut._
import com.hydrangea.codec.Codecs._
import com.hydrangea.android.file.VirtualFileType.{
  AndroidDirectoryType,
  AndroidRegularFileType,
  WindowsDirectoryType,
  WindowsRegularFileType
}
import enumeratum._
import enumeratum.values._

import scala.reflect.ClassTag

sealed abstract class VirtualFileType(override val value: String, val typ: Class[_ <: VirtualFile])
    extends StringEnumEntry

object VirtualFileType extends StringEnum[VirtualFileType] with StringArgonautEnum[VirtualFileType] {
  val values = findValues

  case object WindowsDirectoryType extends VirtualFileType("wd", classOf[WindowsDirectory])
  case object WindowsRegularFileType extends VirtualFileType("wf", classOf[WindowsRegularFile])
  case object AndroidDirectoryType extends VirtualFileType("ad", classOf[AndroidDirectory])
  case object AndroidRegularFileType extends VirtualFileType("af", classOf[AndroidRegularFile])
}

private object VirtualFileCodecHelper {
  def toJson[A <: VirtualFile]: A => Json = { virtualFile: A =>
    val vft: VirtualFileType =
      VirtualFileType.values
        .find(_.typ.equals(virtualFile.getClass))
        .getOrElse(
          throw new IllegalArgumentException(s"No type value defined for Virtual File: ${virtualFile.getClass}"))

    virtualFile match {
      case windowsFile: WindowsFile => Json("_type" := vft, "path" := windowsFile.path)
      case androidFile: AndroidFile =>
        Json("_type" := vft, "path" := androidFile.path, "modifyTime" := androidFile.modifyTime)
    }
  }

  def validateType[A](cursor: HCursor)(implicit classTag: ClassTag[A]): DecodeResult[VirtualFileType] = {
    def checkType(vft: VirtualFileType): DecodeResult[VirtualFileType] = {
      // Is the type we are looking for (classTag) either the same class or a superclass of the type in the JSON
      if (classTag.runtimeClass.isAssignableFrom(vft.typ)) {
        DecodeResult.ok(vft)
      } else {
        DecodeResult.fail(s"Invalid virtual file type: $vft", cursor.history)
      }
    }

    for {
      vft <- (cursor --\ "_type").as[VirtualFileType]
      validatedVft <- checkType(vft)
    } yield validatedVft
  }
}

trait VirtualFileCodecs {

  def decodeFile[VirtualFile]: HCursor => DecodeResult[VirtualFile] =
    cursor => {
      def decode(vft: VirtualFileType, cursor: HCursor): DecodeResult[VirtualFile] =
        vft match {
          case WindowsDirectoryType =>
            cursor.jdecode[WindowsDirectory](WindowsDirectoryCodecs.decodeJson).map(_.asInstanceOf[VirtualFile])
          case WindowsRegularFileType =>
            cursor.jdecode[WindowsRegularFile](WindowsRegularFileCodecs.decodeJson).map(_.asInstanceOf[VirtualFile])
          case AndroidDirectoryType =>
            cursor.jdecode[AndroidDirectory](AndroidDirectoryCodecs.decodeJson).map(_.asInstanceOf[VirtualFile])
          case AndroidRegularFileType =>
            cursor.jdecode[AndroidRegularFile](AndroidRegularFileCodecs.decodeJson).map(_.asInstanceOf[VirtualFile])
        }

      for {
        vft <- (cursor --\ "_type").as[VirtualFileType]
        file <- decode(vft, cursor)
      } yield file
    }
}

// Android Files

trait AndroidFileCodecs {
  def decodeFn[A <: AndroidFile](build: (AndroidPath, Instant) => A)(
      implicit classTag: ClassTag[A]): HCursor => DecodeResult[A] =
    cursor =>
      for {
        _ <- VirtualFileCodecHelper.validateType[A](cursor)
        path <- (cursor --\ "path").as[AndroidPath]
        modifyTime <- (cursor --\ "modifyTime").as[Instant]
      } yield build(path, modifyTime)

  implicit val encodeJson: EncodeJson[AndroidFile] =
    EncodeJson(VirtualFileCodecHelper.toJson[AndroidFile])

  implicit val decodeJson: DecodeJson[AndroidFile] = {
    def decode(vft: VirtualFileType, cursor: HCursor): DecodeResult[AndroidFile] = {
      vft match {
        // TODO: This needs the method from the subclass trait to decode
        case AndroidDirectoryType =>
          cursor.jdecode[AndroidDirectory](AndroidDirectoryCodecs.decodeJson).map(_.asInstanceOf[AndroidFile])
        case AndroidRegularFileType =>
          cursor.jdecode[AndroidRegularFile](AndroidRegularFileCodecs.decodeJson).map(_.asInstanceOf[AndroidFile])
        case _ => throw new IllegalArgumentException(s"Unexpected file type: $vft")
      }
    }

    DecodeJson(
      cursor =>
        for {
          vft <- VirtualFileCodecHelper.validateType[AndroidFile](cursor)
          file <- decode(vft, cursor)
        } yield file
    )
  }

  implicit val codec: CodecJson[AndroidFile] = CodecJson(encodeJson.encode, decodeJson.decode)
}

object AndroidFileCodecs extends AndroidFileCodecs

trait AndroidDirectoryCodecs {
  implicit val encodeJson: EncodeJson[AndroidDirectory] =
    EncodeJson(VirtualFileCodecHelper.toJson[AndroidDirectory])

  implicit val decodeJson: DecodeJson[AndroidDirectory] = DecodeJson(AndroidFileCodecs.decodeFn(AndroidDirectory.apply))

  implicit val codec: CodecJson[AndroidDirectory] = CodecJson.derive
}

object AndroidDirectoryCodecs extends AndroidDirectoryCodecs

trait AndroidRegularFileCodecs {
  implicit val encodeJson: EncodeJson[AndroidRegularFile] =
    EncodeJson(VirtualFileCodecHelper.toJson[AndroidRegularFile])

  implicit val decodeJson: DecodeJson[AndroidRegularFile] = DecodeJson(
    AndroidFileCodecs.decodeFn(AndroidRegularFile.apply))

  implicit val codec: CodecJson[AndroidRegularFile] = CodecJson.derive
}

object AndroidRegularFileCodecs extends AndroidRegularFileCodecs

// Windows files

trait WindowsFileCodecs {
  def decodeFn[A <: WindowsFile](build: (Path) => A)(implicit classTag: ClassTag[A]): HCursor => DecodeResult[A] =
    cursor =>
      for {
        _ <- VirtualFileCodecHelper.validateType[A](cursor)
        windowsPath <- (cursor --\ "path").as[WindowsPath]
      } yield build(windowsPath.toJavaPath)

  implicit val encodeJson: EncodeJson[WindowsFile] =
    EncodeJson(VirtualFileCodecHelper.toJson[WindowsFile])

  implicit val decodeJson: DecodeJson[WindowsFile] = {
    def decode(vft: VirtualFileType, cursor: HCursor): DecodeResult[WindowsFile] = {
      vft match {
        case WindowsDirectoryType =>
          cursor.jdecode[WindowsDirectory](WindowsDirectoryCodecs.decodeJson).map(_.asInstanceOf[WindowsFile])
        case WindowsRegularFileType =>
          cursor.jdecode[WindowsRegularFile](WindowsRegularFileCodecs.decodeJson).map(_.asInstanceOf[WindowsFile])
        case _ => throw new IllegalArgumentException(s"Unexpected file type: $vft")
      }
    }

    DecodeJson(
      cursor =>
        for {
          vft <- VirtualFileCodecHelper.validateType[WindowsFile](cursor)
          file <- decode(vft, cursor)
        } yield file
    )
  }

  implicit val codec: CodecJson[WindowsFile] = CodecJson(encodeJson.encode, decodeJson.decode)
}

object WindowsFileCodecs extends WindowsFileCodecs

trait WindowsDirectoryCodecs {
  implicit val encodeJson: EncodeJson[WindowsDirectory] =
    EncodeJson(VirtualFileCodecHelper.toJson[WindowsDirectory])

  implicit val decodeJson: DecodeJson[WindowsDirectory] = DecodeJson(WindowsFileCodecs.decodeFn(WindowsDirectory.apply))

  implicit val codec: CodecJson[WindowsDirectory] = CodecJson(encodeJson.encode, decodeJson.decode)
}

object WindowsDirectoryCodecs extends WindowsDirectoryCodecs

trait WindowsRegularFileCodecs {
  implicit val encodeJson: EncodeJson[WindowsRegularFile] =
    EncodeJson(VirtualFileCodecHelper.toJson[WindowsRegularFile])

  implicit val decodeJson: DecodeJson[WindowsRegularFile] = DecodeJson(
    WindowsFileCodecs.decodeFn(WindowsRegularFile.apply))

  implicit val codec: CodecJson[WindowsRegularFile] = CodecJson(encodeJson.encode, decodeJson.decode)
}

object WindowsRegularFileCodecs extends WindowsRegularFileCodecs
