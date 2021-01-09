package com.hydrangea.file

import argonaut.Argonaut._
import argonaut.JsonObject
import com.hydrangea.android.adb.Device
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class FileLocationTest extends AnyFlatSpec {
  val path = AbsolutePath(UnixPathBase, Seq("usr", "bin"))
  val device = Device("123456789")

  "FileLocation" should "be serialized LocalFileLocation to JSON" in {
    val local = LocalFileLocation(path)
    val jsonObject: JsonObject = local.asJson.objectOr(fail("Not a JSON object."))
    println(s"JSON => $jsonObject")
    jsonObject.fieldSet should contain only ("_type", "path")
    val typ: Option[String] = jsonObject("_type").map(_.as[String].getOr(fail("_type is not a String")))
    typ should equal(Some(FileLocation.localKey))
    val foundPath: Option[AbsolutePath] =
      jsonObject("path").map(_.as[AbsolutePath].getOr(fail("path is not an AbsolutePath")))
    foundPath should equal(Some(path))
  }

  it should "be serialized AndroidFileLocation to JSON" in {
    val android = AndroidLocation(device, path)
    val jsonObject: JsonObject = android.asJson.objectOr(fail("Not a JSON object."))
    jsonObject.fieldSet should contain only ("_type", "device", "path")
    val typ: Option[String] = jsonObject("_type").map(_.as[String].getOr(fail("_type is not a String")))
    typ should equal(Some(FileLocation.androidKey))
    val foundDevice: Option[Device] = jsonObject("device").map(_.as[Device].getOr(fail("_type is not a Device")))
    foundDevice should equal(Some(device))
    val foundPath: Option[AbsolutePath] =
      jsonObject("path").map(_.as[AbsolutePath].getOr(fail("path is not an AbsolutePath")))
    foundPath should equal(Some(path))
  }

  it should "be serialized LocalFileLocation as FileLocation to JSON" in {
    val local: FileLocation = LocalFileLocation(path)
    val jsonObject: JsonObject = local.asJson.objectOr(fail("Not a JSON object."))
    println(s"JSON => $jsonObject")
    jsonObject.fieldSet should contain only ("_type", "path")
    val typ: Option[String] = jsonObject("_type").map(_.as[String].getOr(fail("_type is not a String")))
    typ should equal(Some(FileLocation.localKey))
    val foundPath: Option[AbsolutePath] =
      jsonObject("path").map(_.as[AbsolutePath].getOr(fail("path is not an AbsolutePath")))
    foundPath should equal(Some(path))
  }

  it should "be serialized AndroidFileLocation as FileLocation to JSON" in {
    val android = AndroidLocation(device, path)
    val jsonObject: JsonObject = android.asJson.objectOr(fail("Not a JSON object."))
    jsonObject.fieldSet should contain only ("_type", "device", "path")
    val typ: Option[String] = jsonObject("_type").map(_.as[String].getOr(fail("_type is not a String")))
    typ should equal(Some(FileLocation.androidKey))
    val foundDevice: Option[Device] = jsonObject("device").map(_.as[Device].getOr(fail("_type is not a Device")))
    foundDevice should equal(Some(device))
    val foundPath: Option[AbsolutePath] =
      jsonObject("path").map(_.as[AbsolutePath].getOr(fail("path is not an AbsolutePath")))
    foundPath should equal(Some(path))
  }

  it should "deserialize LocalFileLocation from JSON" in {
    val jsonString =
      s"""{
         |  "_type": "${FileLocation.localKey}",
         |  "path": "/usr/bin"
         |}
        """.stripMargin

    val decodedLocation: LocalFileLocation = jsonString.decode[LocalFileLocation].getOrElse(fail("Not JSON"))
    decodedLocation should equal(LocalFileLocation(path))
  }

  it should "deserialize AndroidFileLocation from JSON" in {
    val jsonString =
      s"""{
         |  "_type": "${FileLocation.androidKey}",
         |  "device": { "serial": "123456789" },
         |  "path": "/usr/bin"
         |}
        """.stripMargin

    val decodedLocation: AndroidLocation = jsonString.decode[AndroidLocation].getOrElse(fail("Not JSON"))
    decodedLocation should equal(AndroidLocation(device, path))
  }

  it should "deserialize LocalFileLocation from JSON as FileLocation" in {
    val jsonString =
      s"""{
         |  "_type": "${FileLocation.localKey}",
         |  "path": "/usr/bin"
         |}
        """.stripMargin

    val decodedLocation: FileLocation = jsonString.decode[FileLocation].getOrElse(fail("Not JSON"))
    decodedLocation should equal(LocalFileLocation(path))
  }

  it should "deserialize AndroidFileLocation from JSON as FileLocation" in {
    val jsonString =
      s"""{
         |  "_type": "${FileLocation.androidKey}",
         |  "device": { "serial": "123456789" },
         |  "path": "/usr/bin"
         |}
        """.stripMargin

    val decodedLocation: FileLocation = jsonString.decode[FileLocation].getOrElse(fail("Not JSON"))
    decodedLocation should equal(AndroidLocation(device, path))
  }
}
