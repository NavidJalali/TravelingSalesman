package com.tsp

object Main {
  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(12)
    cost match {
      case Some(c) => {
        c.printMatrix()
        BruteForce.bruteForce(c, 0)
      }
      case None => println("invalid cost")
    }
  }
}
