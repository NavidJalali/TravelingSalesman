package HeldKarp

import HeldKarp.Dynamic.CostWithParent
import HeldKarp.Memo.MemoStateResponse
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout

import scala.util.{Failure, Success}

class Memo {

  import Memo._

  def start: Behavior[MemoMessage] = running(State.initial)

  def running(state: State): Behavior[MemoMessage] =
    Behaviors.receive((ctx, msg) => msg match {
      case Get(key, replyTo) =>
        replyTo ! MemoValueResponse(state.map.get(key))
        Behaviors.same
      case GetState(replyTo) =>
        replyTo ! MemoStateResponse(state.map)
        Behaviors.same
      case Put(key, value) =>
        running(State(state.map + (key -> value)))
      case BulkPut(map) =>
        running(State(state.map ++ map))
      case SelfDestruct => Behaviors.stopped
    })
}

object Memo {

  case class State(map: Map[(Int, Set[Int]), CostWithParent])

  object State {
    val initial: State = State(Map.empty)
  }

  sealed trait MemoMessage

  case class Get(key: (Int, Set[Int]), replyTo: ActorRef[MemoValueResponse]) extends MemoMessage

  case class GetState(replyTo: ActorRef[MemoStateResponse])

  case class Put(key: (Int, Set[Int]), value: CostWithParent) extends MemoMessage

  case class BulkPut(map: Map[(Int, Set[Int]), CostWithParent]) extends MemoMessage

  case object SelfDestruct extends MemoMessage

  case class MemoValueResponse(response: Option[CostWithParent])

  case class MemoStateResponse(response: Map[(Int, Set[Int]), CostWithParent])
}

class MemoGetter {
  import MemoGetter._

  implicit val timeout: Timeout = 3.seconds


  def apply(memo: ActorRef[Memo.MemoMessage]): Behavior[MemoGetterMessage] = Behaviors.setup[MemoGetterMessage] {
    ctx => ctx.ask(memo, Memo.GetState){
      case Success(value) => ReceiveMemo(value)
      case Failure(_) => ReceiveMemo(MemoStateResponse(Map.empty))
    }
  })
}

object MemoGetter {
  sealed trait MemoGetterMessage
  case object SelfDestruct extends MemoGetterMessage
  case class ReceiveMemo(memoStateResponse: Memo.MemoStateResponse) extends MemoGetterMessage
}
