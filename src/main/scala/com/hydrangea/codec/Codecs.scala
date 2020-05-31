package com.hydrangea.codec

import java.time.Instant

import argonaut._
import Argonaut._

object Codecs {
  implicit val encodeInstant: EncodeJson[Instant] =
    instant => Json.jNumber(instant.toEpochMilli)

  implicit val decodeInstant: DecodeJson[Instant] =
    cursor => cursor.as[Long].map(Instant.ofEpochMilli)

  implicit val codecInstant: CodecJson[Instant] = CodecJson(encodeInstant.encode, decodeInstant.decode)
}
