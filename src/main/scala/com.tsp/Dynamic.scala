package com.tsp

object Dynamic {
  type CostWithParent = Option[(Int, Int)]

  def dynamic(costMatrix: CostMatrix, source: Int) = {
    val allCitiesExceptSource = (0 until costMatrix.size).toSet - source
    val powerSet = allCitiesExceptSource.subsets()
    val memo = collection.mutable.Map.empty[(Int, Set[Int]), CostWithParent]

    def minimumToSourceWithParent(destination: Int, via: Set[Int]): CostWithParent = {
      memo.get((destination, via)) match {
        case Some(value) => value
        case None => via.map(city =>
          minimumToSourceWithParent(city, via - city).flatMap {
            case (cost, _) => costMatrix.costOf(city, destination).map(c => (cost + c, city))
          }
        ).reduce((u: CostWithParent, v: CostWithParent) =>
          (u, v) match {
            case (None, None) => None
            case (t@Some(_), None) => t
            case (None, t@Some(_)) => t
            case (left@Some(l), right@Some(r)) => if (l._1 < r._1) left else right
          })
      }
    }

    powerSet.foreach {
      case through if through.isEmpty => {
        allCitiesExceptSource
          .foreach(city => memo.put((city, Set.empty[Int]), costMatrix.costOf(source, city).map((_, source))))
      }
      case through if through.nonEmpty => (allCitiesExceptSource -- through).foreach(
        city => memo.put((city, through), minimumToSourceWithParent(city, through))
      )
    }

    def getPath(costWithParent: CostWithParent, unvisited: Set[Int])(accumulator: Vector[Int] = Vector.empty): Option[Vector[Int]] = {
      costWithParent match {
        case Some((_, parent)) =>
          if (parent == source)
            Some(parent +: accumulator)
          else memo.get((parent, unvisited - parent)).flatMap(getPath(_, unvisited - parent)(parent +: accumulator))
        case None => None
      }
    }

    minimumToSourceWithParent(source, allCitiesExceptSource) match {
      case t@Some((cost, _)) =>
        println(s"best tour: ${getPath(t, allCitiesExceptSource)(Vector.empty).map(Path(_).prettyString)}, $cost")
      case None => println("No such route exists.")
    }
  }
}
