package com.tsp

import scala.util.Random.nextDouble

case class CostMatrix(value: Vector[Vector[Option[Int]]]) {
  def printMatrix(): Unit = {
    value.map(_.map {
      case Some(cost) => cost.toString
      case None => "∞"
    }).map(_.fold("")((a, b) => a + " " * (Config.printing.spacing - b.length) + b))
      .foreach(println)
  }

  def size: Int = value.length

  def outgoingVerticesAndCosts(from: Int): Set[(Int, Int)] =
    if (from >= 0 & from < size)
      ((0 until size) zip value(from)).collect {
        case (vertex, Some(cost)) => (vertex, cost)
      }.toSet diff Set((from, 0))
    else
      throw new IllegalArgumentException("outwardEdges source is out of bounds.")

  def getExistingRoutes(startFrom: Int, length: Int): Set[(Vector[Int], Int)] = {
    val init = outgoingVerticesAndCosts(startFrom).map {
      case (vertex, cost) => (Vector(vertex), cost)
    }
    val targetSet = (0 until length).toSet diff Set(startFrom)

    @scala.annotation.tailrec
    def iterate(pathCostSet: Set[(Vector[Int], Int)]): Set[(Vector[Int], Int)] = {
      if (pathCostSet.forall(_._1.toSet == targetSet)) {
        pathCostSet.collect {
          case (path, totalCost) if outgoingVerticesAndCosts(path.last)
            .exists(t => t._1 == startFrom) =>
            val t = outgoingVerticesAndCosts(path.last).filter(t => t._1 == startFrom).head
            (path :+ startFrom, totalCost + t._2)
        }
      } else {
        iterate(pathCostSet.collect {
          case (path, totalCost) if outgoingVerticesAndCosts(path.last)
            .exists(t => t._1 != startFrom && !(path contains t._1)) =>
            outgoingVerticesAndCosts(path.last)
              .filter(t => t._1 != startFrom && !(path contains t._1)).map {
              case (vertex, cost) => (path :+ vertex, totalCost + cost)
            }
        }.flatten)
      }
    }

    iterate(init)
  }

  def costOf(source: Int, dest: Int): Option[Int] =
    if (source >= 0 & source < size & dest >= 0 & dest < size)
      value(source)(dest)
    else
      throw new IllegalArgumentException("costOf argument is out of cost matrix bounds.")
}

object CostMatrix {
  def apply(value: Vector[Vector[Option[Int]]]): Option[CostMatrix] =
    if (isValidCostMatrix(value)) Some(new CostMatrix(value)) else None

  def isValidCostMatrix(c: Vector[Vector[Option[Int]]]): Boolean =
    hasZeroesAlongDiagonal(c) && isSquareMatrix(c)

  private def hasZeroesAlongDiagonal(c: Vector[Vector[Option[Int]]]): Boolean =
    c.indices.map(i => c(i)(i).contains(0)).reduce(_ & _)

  private def isSquareMatrix(c: Vector[Vector[Option[Int]]]): Boolean =
    c.map(_.length == c.length).reduce(_ & _)

  private def randomEntry: Option[Int] =
    if (nextDouble <= Config.chance.edgeExists) Some((nextDouble * Config.graph.maxWeight).ceil.toInt) else None

  def randomGraph(size: Int): Option[CostMatrix] =
    if (size < 0) {
      None
    } else {
      CostMatrix(
        (0 until size).map(row => {
          (0 until size).map(column => {
            if (row == column)
              Some(0)
            else
              randomEntry
          }
          ).toVector
        }
        ).toVector
      )
    }
}
