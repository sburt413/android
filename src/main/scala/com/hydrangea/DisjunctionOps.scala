package com.hydrangea

import scalaz.{-\/, Disjunction, \/-}

object DisjunctionOps {
  implicit class AnyDisjunctionOps[A](a: A) {
    def toRightDisjunction[B]: Disjunction[B, A] = \/-(a)
    def toLeftDisjunction[B]: Disjunction[A, B] = -\/(a)
  }
}
