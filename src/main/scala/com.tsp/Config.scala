package com.tsp

object Config {
  object chance {
    val edgeExists: Double = 1
  }

  object graph {
    val maxWeight: Int = 100
  }

  object printing {
    val spacing: Int = 8
    val pathDelimiter: String = "->"
  }

  object concurrency {
    val parallelism: Int = 16
    val batching: Int = 8192
  }

  object GA {
    val populationSize: Int = 100
    val defaultGenerations: Int = 64
    object Probability {
      val mutation: Double = 0.1
      val crossover: Double = 0.85
      val random: Double = 0.05
    }
  }
}
