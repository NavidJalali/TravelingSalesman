package com.tsp

object BruteForce {
  def bruteForce(costMatrix: CostMatrix, source: Int): Unit = {
    val allTours = ((0 until costMatrix.size).toSet diff Set(source)).toList
      .permutations.map(p => (source :: p) ++ List(source))
  }
}
