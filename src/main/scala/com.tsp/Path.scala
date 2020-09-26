package com.tsp

case class Path(nodes: Vector[Node]) extends AnyVal {

  def :+(node: Node): Path = Path(nodes :+ node)

  def cost(costMatrix: CostMatrix): Cost =
    Cost
      .sum((nodes zip nodes.tail).map {
      case (source, dest) => costMatrix.costOf(source, dest)
    })

  def prettyString: String = nodes.map(_.nodeIndex).mkString("Path(", Config.printing.pathDelimiter, ")")
}
