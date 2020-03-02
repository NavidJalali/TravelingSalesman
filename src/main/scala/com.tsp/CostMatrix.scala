package com.tsp

import scala.util.Random.nextDouble

class CostMatrix(value: List[List[Option[Int]]]) {
  def printMatrix(): Unit = {
    value.map(_.map {
      case Some(cost) => cost.toString
      case None => "∞"
    }).map(_.fold("")((a, b) => a + " " * (Config.printing.spacing - b.length) + b))
      .foreach(println)
  }

  def size: Int = value.length

  def outwardEdges(from: Int): Set[Int] =
    if (from >= 0 & from < size)
      ((0 until size) zip value(from)).collect {
        case (vertex, Some(_)) => vertex
      }.toSet
    else
      throw new IllegalArgumentException("outwardEdges source is out of bounds.")

  def costOf(source: Int, dest: Int): Option[Int] =
    if (source >= 0 & source < size & dest >= 0 & dest < size)
      value(source)(dest)
    else
      throw new IllegalArgumentException("costOf argument is out of cost matrix bounds.")

  def pathCost(path: List[Int]): Option[Int] = {
    val costs = (path zip path.tail).map {
      case (source, dest) => costOf(source, dest)
    }
    if (costs.forall(_.isDefined))
      Some(costs.map(_.get).sum)
    else
      None
  }
}

object CostMatrix {
  def apply(value: List[List[Option[Int]]]): Option[CostMatrix] =
    if (isValidCostMatrix(value)) {
      Some(new CostMatrix(value))
    } else {
      None
    }

  def isValidCostMatrix(c: List[List[Option[Int]]]): Boolean =
    hasZeroesAlongDiagonal(c) & isSquareMatrix(c)

  private def hasZeroesAlongDiagonal(c: List[List[Option[Int]]]): Boolean =
    c.indices.map(i => c(i)(i).contains(0)).reduce(_ & _)

  private def isSquareMatrix(c: List[List[Option[Int]]]): Boolean =
    c.map(_.length == c.length).reduce(_ & _)

  private def randomEntry: Option[Int] =
    if (nextDouble <= Config.chance.edgeExists) {
      Some((nextDouble * Config.graph.maxWeight).ceil.toInt)
    } else {
      None
    }

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
          ).toList
        }
        ).toList
      )
    }
}
