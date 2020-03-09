package com.tsp

import akka.actor.{ActorSystem, PoisonPill}
import akka.stream.Supervision.Stop
import akka.stream.scaladsl.{Sink, Source}

import scala.concurrent.Future
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
    implicit val system = ActorSystem("BruteForce")
    implicit val ctx = system.dispatcher
    val tourPermutations = allTours(costMatrix, source)
    val s = Source.fromIterator(() => tourPermutations).mapAsync(Config.concurrency.parallelism)(
      tour => (tour, tour.cost(costMatrix)) match {
        case (tour, maybeCost) => {
          // println(s"$tour costs $maybeCost")
          Future.successful((tour, maybeCost))
        }
      }
    ).collect {
      case (tour, Some(cost)) => (tour, cost)
    }.runWith(Sink.reduce((l: (Path, Int), r: (Path, Int)) => if(l._2 <= r._2) l else r))

    s onComplete{
      case Success(value) => {
        println(s"best tour: ${value._1.prettyString}, ${value._2}")
        system.terminate().onComplete{
          case Success(_) => println(s"actor system terminated: ${system.name}")
          case Failure(exception) => {
            println(s"failed to terminate actor system ${system.name}: $exception")}
            exception
        }
      }
      case Failure(exception) => println(s"brute force failed: $exception")
    }

  }

  def dynamicBruteForce(costMatrix: CostMatrix, source: Int): Unit = {
    ???
  }
}
