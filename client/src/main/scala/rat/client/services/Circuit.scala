package rat.client.services

import autowire._
import diode._
import diode.data._
import diode.util._
import diode.react.ReactConnector
import rat.shared._
import boopickle.Default._
import com.github.omidb.nlp.toolsInterface.TripsLF
import rat.client.components.{EdgeViz, GraphUtil, NodeViz, TripsLFViz}
import com.softwaremill.quicklens._
import dgraph.DEdge

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// Actions
case class SignIn(username: UserName) extends Action

case class UpdateUser(user: User) extends Action

case object CalculateAgreement extends Action

case object GetStatistics extends Action

case class UpdateStatistics(stats: AnnotationStatistics) extends Action
// Graph Actions
case class EdgeEndAction(isEnter:Boolean, isStart:Boolean, id:(Int, Int)) extends  Action
case class EdgeDragEnable(isStart:Boolean, id:(Int, Int), location:RPoint) extends  Action
case object EdgeDragDisable extends Action
case class MouseMove(location:RPoint) extends Action
case class AddNode(value:NodeViz) extends Action
case class AddEdge(id:(Int, Int), edge: EdgeViz) extends Action
case class RemoveNode(id:Int) extends Action
case class RemoveEdge(id:(Int, Int)) extends Action
case object LayoutLF extends Action
case class SelectAction(node:Option[Int], edge:Option[(Int, Int)]) extends Action

case class ModifyNode(id:Int, newVals:Map[String, String]) extends Action
case class ModifyEdge(id:(Int,Int), newVals:String) extends Action
case object GetUserTasks extends Action
case class UpdateUserTasks(tasks:Option[List[GraphInfo]]) extends Action
case class GetUserGraph(id:Int) extends Action
case class UpdateEditorGraph(lf:Option[TripsLF]) extends Action

case class SearchAlterNode(value: String) extends Action
case class SearchAlterEdge(value: String) extends Action

case class UpdateNodeAlters(nodes: Option[List[NodeAlternative]]) extends Action
case class UpdateEdgeAlters(edges: Option[List[EdgeAlternative]]) extends Action

object UndoAction extends Action
case class PushAction(action:Action) extends Action
case object EnableUndo extends Action
case object DisableUndo extends Action



// The base model of our application
case class RootModel(user: Pot[User], statistics: Pot[AnnotationStatistics], editorHelper: EditorHelper)

class UserHandler[M](modelRW: ModelRW[M, Pot[User]]) extends ActionHandler(modelRW) {
  override def handle = {
    case SignIn(username) => effectOnly(Effect(AjaxClient[Api2].signIn(username).call().map(user => UpdateUser(user))))
    case UpdateUser(user) => updated(Ready(user))
  }
}

class StatisticHandler[M](modelRW: ModelRW[M, Pot[AnnotationStatistics]], user:User) extends ActionHandler(modelRW) {
  override def handle = {
    case GetStatistics=>
      updated(modelRW().pending(),
        Effect(AjaxClient[Api2].getAnnotationStats(user).call().map(stats =>
          UpdateStatistics(stats)
        ))
      )

    case CalculateAgreement => effectOnly(Effect(AjaxClient[Api2].calculateAgreement().call().map(x => GetStatistics)))

    case UpdateStatistics(stats) => updated(Ready(stats))
  }
}

case class DragState(id:(Int,Int), isStart:Boolean)
case class SelectState(isEdge:Option[(Int, Int)], isNode:Option[Int])
case class Alternatives(edgeAlters: Pot[List[EdgeAlternative]], nodeAlters:Pot[List[NodeAlternative]])

case class UndoManager(actionStack:List[Action], undoEnable:Boolean = true)
case class EditorHelper(lf:Pot[TripsLFViz], graphInfos:Pot[List[GraphInfo]],
                        dragState:Option[DragState], selectState:SelectState,
                        alternatives: Alternatives, undoManager: UndoManager)


class GraphHandler[M](modelRW: ModelRW[M, EditorHelper], user:User) extends ActionHandler(modelRW) {
  override def handle = {
    case EdgeEndAction(isEnter, isStart, id) =>
      val value = isEnter
      if (isStart)
        updated(
          modelRW().copy(lf = modelRW().lf.map(lfz => lfz.modify(_.graphViz.edges.at(id).value.startOn).setTo(value)))
        )
      else
        updated(
          modelRW().copy(lf = modelRW().lf.map(lfz => lfz.modify(_.graphViz.edges.at(id).value.endOn).setTo(value)))
        )

    case EdgeDragEnable(isStart, id, location) =>
      updated(
        modelRW().copy(dragState = Some(DragState(id, isStart)))
      )

    case EdgeDragDisable => updated(
      modelRW().copy(dragState = None)
    )


    case MouseMove(location) =>
      if (modelRW().dragState.isDefined) {
        val isStart = modelRW().dragState.get.isStart
        val id = modelRW().dragState.get.id
        updated(
          modelRW().copy(lf = modelRW().lf.map(lfz => {
            val p = if (isStart) lfz.graphViz.edges(id).value.points.last else lfz.graphViz.edges(id).value.points.head
            val points = if (isStart)
              List(location, RPoint((location.x + p.x) / 2, (location.y + p.y) / 2), p)
            else
              List(p, RPoint((location.x + p.x) / 2, (location.y + p.y) / 2), location)
            lfz.modify(_.graphViz.edges.at(id).value.points).setTo(points)
          }))
        )
      } else ActionResult.NoChange

    case AddNode(value) =>
      val newStuffs = modelRW().lf.map{lfz =>
        val (n,g) = lfz.graphViz.addNode(value)
        (n, lfz.modify(_.graphViz).setTo(g))
      }
      val newNodeID = newStuffs.map(_._1).head.id
      val newModel = modelRW().copy(lf = newStuffs.map(_._2))
      if(modelRW().undoManager.undoEnable)
        updated(
          newModel,
          Effect(Future(PushAction(ActionBatch(RemoveNode(newNodeID), LayoutLF))))
        )
      else
        updated(newModel)

    case AddEdge(id, value) =>
      val newStuffs = modelRW().lf.map{lfz =>
        val (e,g) = lfz.graphViz.addEdge(DEdge(value, id._1, id._2)).get

        (e, lfz.modify(_.graphViz).setTo(g))
      }
      val newEdgeID = newStuffs.map(_._1).head
      val newModel = modelRW().copy(lf = newStuffs.map(_._2))
      if(modelRW().undoManager.undoEnable)
        updated(
          newModel,
          Effect(Future(PushAction(ActionBatch(RemoveEdge((newEdgeID.from, newEdgeID.to)), LayoutLF))))
        )
      else
        updated(newModel)


    case RemoveEdge(id) =>
      val edge = modelRW().lf.map(x => x.graphViz.edges(id)).head
      val newModel = modelRW().copy(lf = modelRW().lf.map(lfz =>
        lfz.modify(_.graphViz).setTo(lfz.graphViz.removeEdge(id._1, id._2))))
      if(modelRW().undoManager.undoEnable)
        updated(
          newModel,
          Effect(Future(PushAction(ActionBatch(AddEdge(id, edge.value), LayoutLF))))
        )
      else
        updated(newModel)


    case RemoveNode(id) =>
      val node = modelRW().lf.map(x => x.graphViz.nodes(id)).head
      val newModel = modelRW().copy(lf = modelRW().lf.map(lfz =>
        lfz.modify(_.graphViz).setTo(lfz.graphViz.removeNode(id))
      ))
      if(modelRW().undoManager.undoEnable)
        updated(
          newModel,
          Effect(Future(PushAction(ActionBatch(AddNode(node.value), LayoutLF))))
        )
      else
        updated(newModel)

    case LayoutLF =>
      updated(
        modelRW().copy(lf = modelRW().lf.map(lfz =>
          GraphUtil.layoutGraph(lfz)
        ))
      )

    case SelectAction(nd, ed) =>
      updated(
        modelRW().copy(lf = modelRW().lf.map(lfz =>
          lfz.modify(_.graphViz).setTo(
            lfz.graphViz.copy(
              nodes =
                lfz.graphViz.nodes.map {
                  case (i, n) =>
                    val value = if (nd.isDefined && i == nd.get) true else false
                    (i, n.copy(value = n.value.copy(isSelected = value)))
                },
              edges =
                lfz.graphViz.edges.map {
                  case (i, e) =>
                    val value = if (ed.isDefined && i == ed.get) true else false
                    (i, e.copy(value = e.value.copy(isSelected = value)))
                }
            )
          )
        ),
          selectState = SelectState(ed, nd)
        )
      )

    case ModifyNode(id, value) =>
      val oldValue = modelRW().lf.head.graphViz.nodes(id).value.value
      val newModel = modelRW().copy(lf = modelRW().lf.map(lfz =>
        lfz.modify(_.graphViz.nodes.at(id).value.value).setTo(value)))
      if(modelRW().undoManager.undoEnable)
        updated(
          newModel,
          Effect(Future(PushAction(ActionBatch(ModifyNode(id, oldValue), LayoutLF))))
        )
      else updated(newModel)

    case ModifyEdge(id, value) =>
      val oldValue = modelRW().lf.head.graphViz.edges(id).value.value
      updated(
        modelRW().copy(lf = modelRW().lf.map(lfz =>
          lfz.modify(_.graphViz.edges.at(id).value.value).setTo(value)
        )),
        Effect(Future(PushAction(ActionBatch(ModifyEdge(id, oldValue), LayoutLF))))
      )

    case GetUserTasks =>
      updated(modelRW().copy(graphInfos = modelRW().graphInfos.pending()),
        Effect(
          AjaxClient[Api2].getUserTasks(user).call().map(tasks =>
            UpdateUserTasks(tasks)
          )
        )
      )

    case UpdateUserTasks(tasks) =>
      if (tasks.isDefined)
        updated(modelRW().copy(graphInfos = Ready(tasks.get)))
      else
        updated(modelRW().copy(graphInfos = modelRW().graphInfos.unavailable()))

    case GetUserGraph(id) =>
      updated(modelRW().copy(lf = modelRW().lf.pending()),
        Effect(
          AjaxClient[Api2].getUserGraph(user, id).call().map(lf =>
            UpdateEditorGraph(lf)
          )
        )
      )

    case UpdateEditorGraph(lfo) =>
      if (lfo.isDefined)
        updated(modelRW().copy(lf = Ready(GraphUtil.layoutGraph(lfo.get))))
      else
        updated(modelRW().copy(lf = modelRW().lf.unavailable()))

    //
    case SearchAlterNode(value) =>
      println(s"Searching $value")

      updated(
        modelRW().copy(alternatives =
          modelRW().alternatives.copy(nodeAlters = modelRW().alternatives.nodeAlters.pending())),
        Effect(
          AjaxClient[Api2].getNodeAlters(value).call().map(alters => UpdateNodeAlters(alters))
        )
      )

    case SearchAlterEdge(value) =>
      updated(
        modelRW().copy(alternatives =
          modelRW().alternatives.copy(edgeAlters = modelRW().alternatives.edgeAlters.pending())),
        Effect(
          AjaxClient[Api2].getEdgeAlters(value).call().map(alters =>
            UpdateEdgeAlters(alters)
          )
        )
      )

    case UpdateNodeAlters(alters) =>
      if (alters.isDefined)
        updated(modelRW().copy(alternatives = modelRW().alternatives.copy(nodeAlters = Ready(alters.get))))
      else
        updated(modelRW().copy(alternatives =
          modelRW().alternatives.copy(nodeAlters = modelRW().alternatives.nodeAlters.unavailable())))


    case UpdateEdgeAlters(alters) =>
      if (alters.isDefined)
        updated(modelRW().copy(alternatives = modelRW().alternatives.copy(edgeAlters = Ready(alters.get))))
      else
        updated(modelRW().copy(alternatives =
          modelRW().alternatives.copy(edgeAlters = modelRW().alternatives.edgeAlters.unavailable())))


    case UndoAction =>
      val h = modelRW().undoManager.actionStack.head
      val rest = modelRW().undoManager.actionStack.tail
      updated(
        modelRW().copy(undoManager = UndoManager(rest)), Effect(Future(ActionBatch(DisableUndo,h, EnableUndo)))
      )

    case PushAction(action) =>
      updated(
        modelRW().copy(undoManager = UndoManager(action :: modelRW().undoManager.actionStack))
      )

    case EnableUndo =>
      updated(modelRW().copy(undoManager = modelRW().undoManager.copy(undoEnable = true)))

    case DisableUndo =>
      updated(modelRW().copy(undoManager = modelRW().undoManager.copy(undoEnable = false)))

  }
}


// Application circuit
object MainCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // initial application model
  override protected def initialModel = RootModel(Empty, Empty,
    EditorHelper(
      Empty, Empty, None, SelectState(None, None),Alternatives(Empty, Empty), UndoManager(List.empty)
    )
  )

  // combine all handlers into one
  override protected val actionHandler = composeHandlers(
    new UserHandler(zoomRW(_.user)((m, u) => m.copy(user = u))),
    new StatisticHandler(
      zoomRW(_.statistics)((m, u) => m.copy(statistics = u)), zoom(_.user.headOption.getOrElse(User.invalidUser)).value),

    new GraphHandler(zoomRW(_.editorHelper)((m, u) => m.copy(editorHelper = u)),
      user = zoom(_.user.headOption.getOrElse(User.invalidUser)).value)
  )
}