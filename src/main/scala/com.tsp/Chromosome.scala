package com.tsp

import scala.util.Random

case class Chromosome(value: Vector[Int]) {
  def fatness(costMatrix: CostMatrix, source: Int): Int = {
    Path((source +: value) :+ source).cost(costMatrix).getOrElse(Int.MaxValue)
  }
}

object Chromosome {
  def isValid(source: Int, costMatrix: CostMatrix)(chromosome: Chromosome): Boolean = {
    !(chromosome.value.contains(source) &&
      chromosome.value.size == costMatrix.size - 1) && allNodesExist(costMatrix, source)(chromosome)
  }

  private def allNodesExist(costMatrix: CostMatrix, source: Int)(chromosome: Chromosome): Boolean = {
    val path = (source +: chromosome.value) :+ source
    (path zip path.tail).forall {
      case (from, to) => costMatrix.value(from)(to).isDefined
    }
  }

  def random(costMatrix: CostMatrix, source: Int): Chromosome = {
    val candidate = ((0 until costMatrix.size).toSet - source)
      .map(city => (city, Random.nextDouble))
      .toVector.sortBy(_._2)
      .map{case (city, _) => city}
    if(Chromosome.isValid(source, costMatrix)(Chromosome(candidate)))
      Chromosome(candidate)
    else random(costMatrix, source)
  }

  def crossover(left: Chromosome, right: Chromosome): Chromosome = ???

  def invert(mutant: Chromosome): Chromosome = ???
}
