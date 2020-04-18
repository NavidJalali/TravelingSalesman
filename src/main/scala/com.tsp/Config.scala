package com.tsp

object Config {
  object chance {
    def edgeExists: Double = 0.7
  }

  object graph {
    def maxWeight: Int = 100
  }

  object printing {
    def spacing: Int = 8
  }

  object concurrency {
    def parallelism: Int = 8
    def batching: Int = 256
  }
}
