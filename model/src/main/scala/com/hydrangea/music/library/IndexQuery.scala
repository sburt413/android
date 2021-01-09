package com.hydrangea.music.library

import java.time.Instant

import com.hydrangea.file.AbsolutePath

sealed trait IndexQuery {
  def &&(rhs: IndexQuery): AllOf =
    (this, rhs) match {
      case (allOf: AllOf, rhsAllOf: AllOf) => AllOf(allOf.elements ++ rhsAllOf.elements)
      case (allOf: AllOf, _)               => AllOf(allOf.elements ++ Seq(rhs))
      case (_, rhsAllOf: AllOf)            => AllOf(Seq(this) ++ rhsAllOf.elements)
      case _                               => AllOf(Seq(this, rhs))
    }

  def ||(rhs: IndexQuery): AnyOf =
    (this, rhs) match {
      case (anyOf: AnyOf, rhsAnyOf: AnyOf) => AnyOf(anyOf.elements ++ rhsAnyOf.elements)
      case (anyOf: AnyOf, _)               => AnyOf(anyOf.elements ++ Seq(rhs))
      case (_, rhsAnyOf: AnyOf)            => AnyOf(Seq(this) ++ rhsAnyOf.elements)
      case _                               => AnyOf(Seq(this, rhs))
    }
}

object IndexQuery {
  def apply(element: IndexQueryOperand): IndexQuery = AnyOf(Seq(element))
}

sealed trait IndexQueryOperator extends IndexQuery

/**
  * A logical conjunction of one or more elements.  Queries will only return records that match all criteria.
  */
case class AllOf(elements: Seq[IndexQuery]) extends IndexQueryOperator

object AllOf {
  def elements(elements: IndexQuery*): AllOf = AllOf(elements)
}

/**
  * A logical disjunction of one or more elements.  Queries will return all records that match any of the given criteria.
  */
case class AnyOf(elements: Seq[IndexQuery]) extends IndexQueryOperator

object AnyOf {
  def elements(elements: IndexQuery*): AnyOf = AnyOf(elements)
}

sealed trait IndexQueryOperand extends IndexQuery

case class SubPathQuery(path: AbsolutePath, raw: Boolean = true) extends IndexQueryOperand

case class PathQuery(path: AbsolutePath, raw: Boolean = true) extends IndexQueryOperand

case class HashQuery(hash: String) extends IndexQueryOperand

case class LastUpdatedBefore(when: Instant) extends IndexQueryOperand

case class LastUpdatedAfter(when: Instant) extends IndexQueryOperand

case class TitleQuery(title: String, raw: Boolean = true) extends IndexQueryOperand

case class AlbumQuery(album: String, raw: Boolean = true) extends IndexQueryOperand

case class ArtistQuery(artist: String, raw: Boolean = true) extends IndexQueryOperand

case class YearQuery(artist: String, raw: Boolean = true) extends IndexQueryOperand
