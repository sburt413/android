package com.hydrangea.music.library.index

import java.time.Instant

import com.hydrangea.android.file.VirtualPath

sealed trait RecordQuery {
  def &&(rhs: RecordQuery): AllOf =
    (this, rhs) match {
      case (allOf: AllOf, rhsAllOf: AllOf) => AllOf(allOf.elements ++ rhsAllOf.elements)
      case (allOf: AllOf, _)               => AllOf(allOf.elements ++ Seq(rhs))
      case (_, rhsAllOf: AllOf)            => AllOf(Seq(this) ++ rhsAllOf.elements)
      case _                               => AllOf(Seq(this, rhs))
    }

  def ||(rhs: RecordQuery): AnyOf =
    (this, rhs) match {
      case (anyOf: AnyOf, rhsAnyOf: AnyOf) => AnyOf(anyOf.elements ++ rhsAnyOf.elements)
      case (anyOf: AnyOf, _)               => AnyOf(anyOf.elements ++ Seq(rhs))
      case (_, rhsAnyOf: AnyOf)            => AnyOf(Seq(this) ++ rhsAnyOf.elements)
      case _                               => AnyOf(Seq(this, rhs))
    }
}

object RecordQuery {
  def apply(element: RecordQueryOperand): RecordQuery = AnyOf(Seq(element))
}

sealed trait RecordQueryOperator extends RecordQuery

/**
  * A logical conjunction of one or more elements.  Queries will only return records that match all criteria.
  */
case class AllOf(elements: Seq[RecordQuery]) extends RecordQueryOperator

object AllOf {
  def elements(elements: RecordQuery*): AllOf = AllOf(elements)
}

/**
  * A logical disjunction of one or more elements.  Queries will return all records that match any of the given criteria.
  */
case class AnyOf(elements: Seq[RecordQuery]) extends RecordQueryOperator

object AnyOf {
  def elements(elements: RecordQuery*): AnyOf = AnyOf(elements)
}

sealed trait RecordQueryOperand extends RecordQuery

case class SubPathQuery(path: VirtualPath, raw: Boolean = true) extends RecordQueryOperand

case class PathQuery(path: VirtualPath, raw: Boolean = true) extends RecordQueryOperand

case class HashQuery(hash: String) extends RecordQueryOperand

case class LastUpdatedBefore(when: Instant) extends RecordQueryOperand

case class LastUpdatedAfter(when: Instant) extends RecordQueryOperand

case class TitleQuery(title: String, raw: Boolean = true) extends RecordQueryOperand

case class AlbumQuery(album: String, raw: Boolean = true) extends RecordQueryOperand

case class ArtistQuery(artist: String, raw: Boolean = true) extends RecordQueryOperand

case class YearQuery(artist: String, raw: Boolean = true) extends RecordQueryOperand
