package com.tsp
import Timer.Timer

object Main {
  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(11)
    cost match {
      case Some(c) =>
        c.printMatrix()
        // println("dynamic bruteforce")
        // Timer.synchronousTimer{BruteForce.dynamicBruteForce(c, 0)}
        println("solving tsp by bruteforce")
        Timer.synchronousTimer{BruteForce.bruteForce(c, 0)}
        println("solving tsp by bruteforce in parallel")
        BruteForce.bruteForceWithActorSystem(c, 0)
      case None => println("Invalid cost matrix.")
    }
  }
}
