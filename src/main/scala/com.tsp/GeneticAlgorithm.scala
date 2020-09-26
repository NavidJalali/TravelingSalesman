package com.tsp

object GeneticAlgorithm {
  def geneticAlgorithm(costMatrix: CostMatrix, source: Node, generations: Int = Config.GA.defaultGenerations): Unit = {
    Population.random(costMatrix, source)
      .bulkEvolve(costMatrix, source)(generations).fittest(costMatrix, source)
  }
}
