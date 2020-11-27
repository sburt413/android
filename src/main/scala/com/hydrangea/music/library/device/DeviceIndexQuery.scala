package com.hydrangea.music.library.device

import java.time.Instant

import com.hydrangea.file.AbsolutePath

sealed trait DeviceIndexQuery {
  def &&(rhs: DeviceIndexQuery): AllOf =
    (this, rhs) match {
      case (allOf: AllOf, rhsAllOf: AllOf) => AllOf(allOf.elements ++ rhsAllOf.elements)
      case (allOf: AllOf, _)               => AllOf(allOf.elements ++ Seq(rhs))
      case (_, rhsAllOf: AllOf)            => AllOf(Seq(this) ++ rhsAllOf.elements)
      case _                               => AllOf(Seq(this, rhs))
    }

  def ||(rhs: DeviceIndexQuery): AnyOf =
    (this, rhs) match {
      case (anyOf: AnyOf, rhsAnyOf: AnyOf) => AnyOf(anyOf.elements ++ rhsAnyOf.elements)
      case (anyOf: AnyOf, _)               => AnyOf(anyOf.elements ++ Seq(rhs))
      case (_, rhsAnyOf: AnyOf)            => AnyOf(Seq(this) ++ rhsAnyOf.elements)
      case _                               => AnyOf(Seq(this, rhs))
    }
}

object DeviceIndexQuery {
  def apply(element: DeviceIndexQueryOperand): DeviceIndexQuery = AnyOf(Seq(element))
}

sealed trait DeviceIndexQueryOperator extends DeviceIndexQuery

/**
  * A logical conjunction of one or more elements.  Queries will only return records that match all criteria.
  */
case class AllOf(elements: Seq[DeviceIndexQuery]) extends DeviceIndexQueryOperator

object AllOf {
  def elements(elements: DeviceIndexQuery*): AllOf = AllOf(elements)
}

/**
  * A logical disjunction of one or more elements.  Queries will return all records that match any of the given criteria.
  */
case class AnyOf(elements: Seq[DeviceIndexQuery]) extends DeviceIndexQueryOperator

object AnyOf {
  def elements(elements: DeviceIndexQuery*): AnyOf = AnyOf(elements)
}

sealed trait DeviceIndexQueryOperand extends DeviceIndexQuery

case class SubPathQuery(path: AbsolutePath, raw: Boolean = true) extends DeviceIndexQueryOperand

case class PathQuery(path: AbsolutePath, raw: Boolean = true) extends DeviceIndexQueryOperand

case class HashQuery(hash: String) extends DeviceIndexQueryOperand

case class LastUpdatedBefore(when: Instant) extends DeviceIndexQueryOperand

case class LastUpdatedAfter(when: Instant) extends DeviceIndexQueryOperand

case class TitleQuery(title: String, raw: Boolean = true) extends DeviceIndexQueryOperand

case class AlbumQuery(album: String, raw: Boolean = true) extends DeviceIndexQueryOperand

case class ArtistQuery(artist: String, raw: Boolean = true) extends DeviceIndexQueryOperand

case class YearQuery(artist: String, raw: Boolean = true) extends DeviceIndexQueryOperand
