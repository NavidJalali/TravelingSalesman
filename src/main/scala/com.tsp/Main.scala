package com.tsp
import Timer.Timer

object Main {
  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(12)
    cost match {
      case Some(c) =>
        c.printMatrix()
        // println("dynamic bruteforce")
        // Timer.synchronousTimer{BruteForce.dynamicBruteForce(c, 0)}
        println("bruteforce")
        Timer.synchronousTimer{BruteForce.bruteForce(c, 0)}
        println("actor bruteforce")
        BruteForce.bruteForceWithActorSystem(c, 0)
      case None => println("Invalid cost matrix.")
    }
  }
}
