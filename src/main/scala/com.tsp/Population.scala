package com.tsp

case class Population(value: Vector[Chromosome]) {
  def evolve: Population = ???
  @scala.annotation.tailrec
  final def bulkEvolve(generations: Int): Population =
    if(0 < generations) evolve.bulkEvolve(generations - 1) else evolve
  def byFitness(costMatrix: CostMatrix, source: Int): Vector[(Chromosome, Int)] = value.map(chromosome =>
    (chromosome, Path((source +: chromosome.value) :+ source).cost(costMatrix).get)
  ).sortBy(_._2)
}

object Population {
  def random(costMatrix: CostMatrix, source: Int): Population = Population((0 until Config.GA.populationSize).toVector
    .map(_ => Chromosome.random(costMatrix, source)))
}
