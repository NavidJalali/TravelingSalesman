package com.tsp

object Config {
  object chance {
    val edgeExists: Double = 0.9
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
    val batching: Int = 512
  }

  object GA {
    val populationSize: Int = 64
  }
}
