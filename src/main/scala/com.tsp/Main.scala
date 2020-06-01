package com.tsp

import com.Timer.Timer

object Main {
  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(10)
    // val cost = CostMatrix(Constants.exampleMatrix)
    cost match {
      case Some(c) =>
        c.printMatrix()
        println("Running GA For 1024 Generations")
        Timer.synchronousTimer{GeneticAlgorithm.geneticAlgorithm(c, 0, 1024)}
        println("solving tsp by dynamic brute force")
        Timer.synchronousTimer{Dynamic.dynamic(c, 0)}
        //println("solving tsp by brute force")
        //Timer.synchronousTimer{BruteForce.bruteForce(c, 0)}
        // println("solving tsp by bruteforce in parallel")
        // BruteForce.bruteForceWithStreams(c, 0)
      case None => println("Invalid cost matrix.")
    }
  }
}
