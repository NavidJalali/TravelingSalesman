package com.tsp

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object BruteForce {
  def allTours(costMatrix: CostMatrix, source: Int): Iterator[Path] = ((0 until costMatrix.size)
    .toSet diff Set(source)).toList
    .permutations.map(p => Path((source :: p) ++ List(source)))

  def bruteForce(costMatrix: CostMatrix, source: Int): Unit = {
    val possibleTours = allTours(costMatrix, source).map(tour => (tour, tour.cost(costMatrix))).collect {
      case (tour, Some(cost)) => {
        (tour, cost)
      }
    }.toList
    val bestTour = possibleTours.sortBy(_._2).headOption
    println(s"best tour: $bestTour")
  }

  def bruteForceWithActorSystem(costMatrix: CostMatrix, source: Int): Unit = {
    implicit val system: ActorSystem = ActorSystem("BruteForce")
    implicit val ctx: ExecutionContext = system.dispatcher
    val tourPermutations = allTours(costMatrix, source)
    val s = Source.fromIterator(() => tourPermutations).mapAsync(Config.concurrency.parallelism)(
      tour => (tour, tour.cost(costMatrix)) match {
        case (tour, maybeCost) => {
          Future.successful((tour, maybeCost))
        }
      }
    ).collect {
      case (tour, Some(cost)) => (tour, cost)
    }.runWith(Sink.reduce((l: (Path, Int), r: (Path, Int)) => if (l._2 <= r._2) l else r))

    s onComplete {
      case Success(value) => {
        println(s"best tour: ${value._1.prettyString}, ${value._2}")
        system.terminate().onComplete {
          case Success(_) => println(s"actor system terminated: ${system.name}")
          case Failure(exception) => {
            println(s"failed to terminate actor system ${system.name}: $exception")
          }
            exception
        }
      }
      case Failure(exception) => println(s"brute force failed: $exception")
    }
  }

  def dynamicBruteForce(costMatrix: CostMatrix, source: Int): Unit = {
    def getExistingRoutes(startFrom: Int, length: Int) = {

      val init = costMatrix.outgoingVerticesAndCosts(startFrom).map {
        case (vertex, cost) => (List(vertex), cost)
      }
      val targetSet = (0 until length).toSet diff Set(startFrom)

      def iterate(pathCostSet: Set[(List[Int], Int)]): Set[(List[Int], Int)] = {
        if (pathCostSet.forall(_._1.toSet == targetSet)) {
          pathCostSet.collect {
            case (path, totalCost) if costMatrix.outgoingVerticesAndCosts(path.last)
              .exists(t => t._1 == startFrom) => {
              val t = costMatrix.outgoingVerticesAndCosts(path.last).filter(t => t._1 == startFrom).head
              (path :+ startFrom, totalCost + t._2)
            }
          }
        } else {
          iterate(pathCostSet.collect {
            case (path, totalCost) if costMatrix.outgoingVerticesAndCosts(path.last)
              .exists(t => t._1 != startFrom && !(path contains t._1)) =>
              costMatrix.outgoingVerticesAndCosts(path.last)
                .filter(t => t._1 != startFrom && !(path contains t._1)).map {
                case (vertex, cost) => (path :+ vertex, totalCost + cost)
              }
          }.flatten)
        }
      }
      iterate(init)
    }
    val bestPath = getExistingRoutes(0, costMatrix.size).toList
      .map { case (path, cost) => (Path(path), cost) }
      .sortBy(_._2)
      .headOption
    println(bestPath)
  }

}
