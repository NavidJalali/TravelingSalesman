package com.tsp

object Main {
  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(10)
    cost match {
      case Some(c) => {
        c.printMatrix()
        BruteForce.bruteForceWithActorSystem(c, 0)
      }
      case None => println("Invalid cost matrix.")
    }
  }
}
