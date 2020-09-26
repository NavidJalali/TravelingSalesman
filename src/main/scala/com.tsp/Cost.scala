package com.tsp

case object GetInfiniteCostException extends Exception("No such route exists.")

sealed trait Cost {
  def contains(cost: Int): Boolean

  def +(that: Cost): Cost

  def toOption: Option[Finite]

  def <(that: Cost): Boolean

  def get: Int

  def getOrElse(default: Int): Int

  def isDefined: Boolean
}

case object Infinite extends Cost {
  override def contains(cost: Int): Boolean = false

  override def +(that: Cost): Cost = Infinite

  override def toOption: Option[Finite] = None

  override def <(that: Cost): Boolean = false

  override def get: Int = throw GetInfiniteCostException

  override def getOrElse(default: Int): Int = default

  override def isDefined: Boolean = false
 }

case class Finite(value: Int) extends Cost {
  override def contains(cost: Int): Boolean = cost == value

  override def +(that: Cost): Cost = that match {
    case Infinite => Infinite
    case Finite(thatCost) => Finite(value + thatCost)
  }

  override def toOption: Option[Finite] = Some(this)

  override def <(that: Cost): Boolean = that match {
    case Finite(v) => value < v
    case Infinite => true
  }

  override def get: Int = value

  override def getOrElse(default: Int): Int = value

  override def isDefined: Boolean = true
}

object Cost {
  def sum(costs: Vector[Cost]): Cost =
    costs
      .foldLeft(Finite(0): Cost) {
        case (acc, next) => (acc, next) match {
          case (Finite(a), Finite(b)) => Finite(a+b)
          case _ => Infinite
        }
      }

  def fromOption(cost: Option[Int]): Cost = cost match {
    case Some(value) => Finite(value)
    case None => Infinite
  }
}
