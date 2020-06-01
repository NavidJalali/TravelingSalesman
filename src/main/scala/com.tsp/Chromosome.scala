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
    val candidate = Chromosome(((0 until costMatrix.size).toSet - source)
      .map(city => (city, Random.nextDouble))
      .toVector.sortBy(_._2)
      .map{case (city, _) => city})
    if(Chromosome.isValid(source, costMatrix)(candidate))
      candidate
    else random(costMatrix, source)
  }

  def crossover(costMatrix: CostMatrix, source: Int)(left: Chromosome, right: Chromosome): Chromosome = {
    val length = left.value.length.ensuring(_ == right.value.length)
    val (begin, end) = TwoRandomInts(length)
    val injectable = left.value.slice(begin, end)
    val dick = right.value.splitAt(end) match {
      case (u, v) => v ++ u
    }
    val candidate = Chromosome(dick.filterNot(injectable.contains(_)).patch(begin, injectable, 0))
    if(Chromosome.isValid(source, costMatrix)(candidate))
      candidate
    else crossover(costMatrix, source)(left, right)
  }

  def invert(costMatrix: CostMatrix, source: Int)(mutant: Chromosome): Chromosome = {
    val length = mutant.value.length
    val (left, right) = TwoRandomInts(length)
    val candidate = Chromosome((0 until length).map(i =>
      if(i == left) mutant.value(right)
      else if(i == right) mutant.value(left)
      else mutant.value(i)
    ).toVector)
    if(Chromosome.isValid(source, costMatrix)(candidate))
      candidate
    else invert(costMatrix, source)(mutant)
  }

  private def TwoRandomInts(lessThan: Int): (Int, Int) = {
    (Random.nextInt(lessThan), Random.nextInt(lessThan)) match {
      case (u, v) if u != v => if (u <= v) (u, v) else (v, u)
      case _ => TwoRandomInts(lessThan)
    }
  }
}
