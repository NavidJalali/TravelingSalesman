package com.tsp
import Timer.Timer

object Main {
  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(11)
    // val cost = CostMatrix(Constants.exampleMatrix)
    cost match {
      case Some(c) =>
        c.printMatrix()
        //println("solving tsp by dynamic brute force")
        //Timer.synchronousTimer{BruteForce.dynamicBruteForce(c, 0)}
        println("solving tsp by brute force")
        Timer.synchronousTimer{BruteForce.bruteForce(c, 0)}
        println("solving tsp by bruteforce in parallel")
        BruteForce.bruteForceWithActorSystem(c, 0)
      case None => println("Invalid cost matrix.")
    }
  }
}
