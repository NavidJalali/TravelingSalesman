package com.tsp

import scala.util.Random.nextDouble

case class CostMatrix(matrix: Vector[Vector[Cost]]) extends AnyVal {
  def printMatrix(): Unit = {
    matrix.map(_.map {
      case Finite(cost) => cost.toString
      case Infinite => "∞"
    }).map(_.fold("")((a, b) => a + " " * (Config.printing.spacing - b.length) + b))
      .foreach(println)
  }

  def size: Int = matrix.length

  def allNodes: Vector[Node] = (0 to size).map(Node).toVector

  def isValidNode(node: Node): Boolean = node.nodeIndex >= 0 && node.nodeIndex < size

  def outgoingVerticesAndCosts(from: Node): Set[(Node, Cost)] =
    if (from.nodeIndex >= 0 & from.nodeIndex < size)
      (allNodes zip matrix(from.nodeIndex)).collect {
        case t@(f, Finite(_)) if f != from => t
      }.toSet
    else
      throw new IllegalArgumentException("outwardEdges source is out of bounds.")

  def getExistingRoutes(startFrom: Node, length: Int): Set[(Path, Cost)] = {
    val init: Set[(Path, Cost)] = outgoingVerticesAndCosts(startFrom).map {
      case (vertex, cost) => (Path(Vector(vertex)), cost)
    }
    val targetSet = (0 until length).map(Node).toSet - startFrom

    @scala.annotation.tailrec
    def iterate(pathCostSet: Set[(Path, Cost)]): Set[(Path, Cost)] = {
      if (pathCostSet.forall {
        case (path, _) => path.nodes.toSet == targetSet
      }) {
        pathCostSet
          .map{ case (path, totalCost) => (path, totalCost, outgoingVerticesAndCosts(path.nodes.last))}
          .collect {
          case (path, totalCost, lastOutNodesAndCosts) if lastOutNodesAndCosts
            .exists { case (node, _) => node == startFrom } =>
            val returnCost = lastOutNodesAndCosts
              .collect{ case (outgoing, returnCost) if outgoing == startFrom => returnCost }.head
            (path :+ startFrom, totalCost + returnCost)
        }
      } else {
        iterate(
          pathCostSet
            .map{ case (path, totalCost) => (path, totalCost, outgoingVerticesAndCosts(path.nodes.last))}
            .collect {
          case (path, totalCost, lastOutNodesAndCosts) if lastOutNodesAndCosts
            .exists{case (node, _) => node != startFrom && !(path.nodes contains node)} =>
            lastOutNodesAndCosts
              .collect {
                case (node, cost) if node != startFrom && !path.nodes.contains(node) =>
                  (path :+ node, totalCost + cost)
              }
        }.flatten)
      }
    }

    iterate(init)
  }

  def costOf(source: Node, dest: Node): Cost =
    if (isValidNode(source) && isValidNode(dest))
      matrix(source.nodeIndex)(dest.nodeIndex)
    else
      throw new IllegalArgumentException("costOf argument is out of cost matrix bounds.")
}

object CostMatrix {
  def apply(value: Vector[Vector[Option[Int]]]): Option[CostMatrix] = {
    val typed: Vector[Vector[Cost]] = value.map(_.map {
      case Some(cost) => Finite(cost)
      case None => Infinite
    })
    if (isValidCostMatrix(typed)) Some(new CostMatrix(typed)) else None
  }

  def isValidCostMatrix(c: Vector[Vector[Cost]]): Boolean =
    hasZeroesAlongDiagonal(c) && isSquareMatrix(c)

  private def hasZeroesAlongDiagonal(c: Vector[Vector[Cost]]): Boolean =
    c.indices.map(i => c(i)(i).contains(0)).reduce(_ & _)

  private def isSquareMatrix(c: Vector[Vector[Cost]]): Boolean =
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
