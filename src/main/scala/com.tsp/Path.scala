package com.tsp

case class Path(value: List[Int])

object Path {

  implicit class PathEnhancer(path: Path) {

    def cost(costMatrix: CostMatrix): Option[Int] = {
      val costs = (path.value zip path.value.tail).map {
        case (source, dest) => costMatrix.costOf(source, dest)
      }.collect { case Some(cost) => cost }

      if (costs.length == path.value.length - 1) Some(costs.sum) else None
    }

    def prettyString: String = path.value.mkString("Path(", Config.printing.pathDelimiter, ")")
  }

}
