package com.tsp

object Dynamic {
  type CostWithParent = Option[(Cost, Node)]

  def dynamic(costMatrix: CostMatrix, source: Node): Unit = {
    val allCitiesExceptSource = (0 until costMatrix.size).map(Node).toSet - source
    val powerSet = allCitiesExceptSource.subsets()
    val memo = collection.mutable.Map.empty[(Node, Set[Node]), CostWithParent]

    def minimumToSourceWithParent(destination: Node, via: Set[Node]): CostWithParent = {
      memo.get((destination, via)) match {
        case Some(value) => value
        case None => via.map(city =>
          minimumToSourceWithParent(city, via - city).flatMap {
            case (cost, parent) => (costMatrix.costOf(city, destination) + cost).toOption.map((_, parent))
          }
        ).reduce((u: CostWithParent, v: CostWithParent) =>
          (u, v) match {
            case (None, None) => None
            case (t@Some(_), None) => t
            case (None, t@Some(_)) => t
            case (left@Some((leftCost, _)), right@Some((rightCost,_))) => if (leftCost < rightCost) left else right
          })
      }
    }

    powerSet.foreach {
      through =>
        if (through.isEmpty) {
          allCitiesExceptSource
            .foreach(city => memo.put((city, Set.empty[Node]), costMatrix.costOf(source, city).toOption.map((_, source))))
        } else {
          (allCitiesExceptSource -- through).foreach(
            city => memo.put((city, through), minimumToSourceWithParent(city, through))
          )
        }
    }

    def getPath(costWithParent: CostWithParent, unvisited: Set[Node])(accumulator: Vector[Node] = Vector.empty): Option[Vector[Node]] = {
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
