package com.tsp

case class Chromosome(value: Vector[Int])

object Chromosome {
  def isValid(source: Int, costMatrix: CostMatrix)(chromosome: Chromosome): Boolean = {
    !(chromosome.value.contains(source) &&
      chromosome.value.size == costMatrix.size - 1) && allNodesExist(costMatrix)(chromosome)
  }

  private def allNodesExist(costMatrix: CostMatrix)(chromosome: Chromosome): Boolean = {
    (chromosome.value zip chromosome.value.tail).forall {
      case (from, to) => costMatrix.value(from)(to).isDefined
    }
  }

  def crossover(left: Chromosome, right: Chromosome): Chromosome = ???

  def invert(mutant: Chromosome): Chromosome = ???

  def fitness(costMatrix: CostMatrix)(chromosome: Chromosome): Double = ???
}
