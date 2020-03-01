package com.tsp

object Main {
  def main(args: Array[String]): Unit = {
//    val cost = CostMatrix(List(
//      List(Some(0), Some(4), None, Some(2)),
//      List(None, Some(0), None, Some(3)),
//      List(Some(1), None, Some(0), Some(9)),
//      List(Some(7), Some(13), Some(5), Some(0))
//    ))

    val cost = CostMatrix.randomGraph(5)
    cost match {
      case Some(c) => {
        c.printMatrix()
        println(c.outwardEdges(0))
      }
      case None => println("invalid cost")
    }
  }
}
