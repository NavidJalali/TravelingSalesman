package com.tsp

object GeneticAlgorithm {
  def geneticAlgorithm(costMatrix: CostMatrix, source: Int, generations: Int = 100): Unit = {
    val population = Population.random(costMatrix, source)
    population.evolve(100)
  }
}
