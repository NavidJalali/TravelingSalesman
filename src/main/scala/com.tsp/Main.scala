package com.tsp

object Main {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"Elapsed time: ${(t1 - t0)/1000000f} ms")
    result
  }

  def main(args: Array[String]): Unit = {
    val cost = CostMatrix.randomGraph(11)
    cost match {
      case Some(c) => {
        c.printMatrix()
        time{BruteForce.dynamicBruteForce(c, 0)}
        time{BruteForce.bruteForce(c, 0)}
        time{BruteForce.bruteForceWithActorSystem(c, 0)}
      }
      case None => println("Invalid cost matrix.")
    }
  }
}
