package com.tsp

object BruteForce {
  def bruteForce(costMatrix: CostMatrix, source: Int): Unit = {
    val allTours = ((0 until costMatrix.size).toSet diff Set(source)).toList
      .permutations.map(p => (source :: p) ++ List(source))
    val possibleTours = allTours.map(tour => (tour, costMatrix.pathCost(tour))).collect {
      case (tour, Some(cost)) => {
        println(tour, cost)
        (tour, cost)
      }
    }.toList
    val bestTour = possibleTours.sortBy(_._2).headOption
    println(s"best tour: $bestTour")
  }
}
