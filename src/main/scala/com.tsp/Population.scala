package com.tsp

import scala.util.Random
import Config.GA._
import math.ceil

case class Population(value: Vector[Chromosome]) {
  def fittest(costMatrix: CostMatrix, source: Int): Unit = {
    Population.sortedWithCost(value)(costMatrix, source).head match {
      case (chromosome, cost) => {
        val path = Path((source +: chromosome.value) :+ source)
        println(s"best tour so far: ${path.prettyString}, $cost")
      }
    }
  }

  def evolve(costMatrix: CostMatrix, source: Int): Population = {
    val size = value.size
    val mutants = for (_ <- 0 until size * ceil(Probability.mutation).toInt) yield {
      Chromosome.invert(costMatrix, source)(pseudoRandomSelect(costMatrix, source))
    }
    val offspring = for (_ <- 0 until size * ceil(Probability.crossover).toInt) yield {
      pseudoRandomSelectTwoUnique(costMatrix, source) match {
        case (dad, mom) => Chromosome.crossover(costMatrix, source)(dad, mom)
      }
    }
    val abominations = for (_ <- 0 until size * ceil(Probability.random).toInt) yield {
      Chromosome.random(costMatrix, source)
    }
    Population.fittestOf(value ++ mutants ++ offspring ++ abominations)(costMatrix, source)
  }

  @scala.annotation.tailrec
  final def bulkEvolve(costMatrix: CostMatrix, source: Int)(generations: Int): Population =
    if (0 < generations)
      evolve(costMatrix, source).bulkEvolve(costMatrix, source)(generations - 1)
    else
      evolve(costMatrix, source)

  private def pseudoRandomSelect(costMatrix: CostMatrix, source: Int): Chromosome = {
    val roll = Random.nextDouble
    byFitnessCumulative(costMatrix, source).collect {
      case (chromosome, left, right) if left < roll & roll <= right => chromosome
    }.head
  }

  private def pseudoRandomSelectTwoUnique(costMatrix: CostMatrix, source: Int): (Chromosome, Chromosome) = {
    (pseudoRandomSelect(costMatrix, source), pseudoRandomSelect(costMatrix, source)) match {
      case (u, v) => if (u != v) (u, v) else pseudoRandomSelectTwoUnique(costMatrix, source)
    }
  }

  private def pseudoRandomSelectTwoNonUnique(costMatrix: CostMatrix, source: Int): (Chromosome, Chromosome) =
    (pseudoRandomSelect(costMatrix, source), pseudoRandomSelect(costMatrix, source))

  private def byFitnessCumulative(costMatrix: CostMatrix, source: Int): Vector[(Chromosome, Double, Double)] = {
    val normalized = byFitnessNormalized(costMatrix, source)
    var indicator: Double = 0
    normalized.map {
      case (chromosome, fitness) => {
        val out = (chromosome, indicator, indicator + fitness)
        indicator += fitness
        out
      }
    }
  }

  private def byFitnessNormalized(costMatrix: CostMatrix, source: Int): Vector[(Chromosome, Double)] = {
    val sorted = byFitness(costMatrix, source: Int)
    val total = sorted.map(_._2).sum
    sorted.map {
      case (chromosome, fitness) => (chromosome, fitness / total)
    }
  }

  private def byFitness(costMatrix: CostMatrix, source: Int): Vector[(Chromosome, Double)] = value.map(chromosome =>
    (chromosome, 1.0 / Path((source +: chromosome.value) :+ source).cost(costMatrix).get)
  ).sortBy {
    case (_, cost) => cost
  }(Ordering[Double].reverse)

}

object Population {
  def random(costMatrix: CostMatrix, source: Int): Population = Population((0 until Config.GA.populationSize).toVector
    .map(_ => Chromosome.random(costMatrix, source)))

  private def fittestOf(population: Vector[Chromosome])(costMatrix: CostMatrix, source: Int): Population = {
    Population(sortedWithCost(population)(costMatrix, source).take(populationSize).map {
      case (chromosome, _) => chromosome
    })
  }

  private def sortedWithCost(population: Vector[Chromosome])(costMatrix: CostMatrix, source: Int) = {
    population.distinct.map(
      chromosome => (chromosome, Path((source +: chromosome.value) :+ source).cost(costMatrix).get)
    ).sortBy {
      case (_, cost) => cost
    }
  }
}
