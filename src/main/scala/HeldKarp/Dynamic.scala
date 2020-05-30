package HeldKarp

import HeldKarp.Memo.{BulkPut, GetState, MemoMessage, MemoStateResponse}
import akka.actor.ActorSystem
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import com.tsp.{CostMatrix, Path}
import akka.actor.typed.scaladsl.adapter._

import concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Dynamic {
  type CostWithParent = Option[(Int, Int)]

  def dynamic(costMatrix: CostMatrix, source: Int) = {
    implicit val system: ActorSystem = ActorSystem("Held-Karp")
    implicit val ctx: ExecutionContext = system.dispatcher
    val Memo: ActorRef[MemoMessage] = system.spawnAnonymous(new Memo().start)

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
        Memo ! BulkPut(allCitiesExceptSource.map(
          city => ((city, Set.empty[Int]), costMatrix.costOf(source, city).map((_, source)))
        ).toMap)
        // allCitiesExceptSource
        //   .foreach(city => memo.put((city, Set.empty[Int]), costMatrix.costOf(source, city).map((_, source))))
      }
      case through if through.nonEmpty => {

        Memo ! BulkPut((allCitiesExceptSource -- through).map(
          city => ((city, through), minimumToSourceWithParent(city, through))
        ).toMap)

//        (allCitiesExceptSource -- through).foreach(
//          city => memo.put((city, through), minimumToSourceWithParent(city, through))
//        )
      }
    }

    def getPath(costWithParent: CostWithParent, unvisited: Set[Int])(accumulator: Vector[Int] = Vector.empty): Option[Vector[Int]] = {
      costWithParent match {
        case Some((_, parent)) =>
          if (parent == source)
            Some(parent +: accumulator)
          else {
            memo.get((parent, unvisited - parent)).flatMap(getPath(_, unvisited - parent)(parent +: accumulator))
          }
        case None => None
      }
    }

    minimumToSourceWithParent(source, allCitiesExceptSource) match {
      case t@Some((cost, _)) =>
        println(s"best tour: ${getPath(t, allCitiesExceptSource)(Vector.empty).map(Path(_).prettyString)}, $cost")
      case None => println("No such route exists.")
    }
    system.terminate().onComplete {
      case Success(_) => println(s"actor system terminated: ${system.name}")
      case Failure(exception) =>
        println(s"failed to terminate actor system ${system.name}: $exception")
    }
  }
}
