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
case class UpdateUserG(user: User) extends Action
case class UpdateUserT(user: User) extends Action

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

case class AddComment(taskID:Int, value:String) extends Action
case class AddCommentAndSave(taskID:Int, value:String) extends Action

case class ModifyNode(id:Int, newVals:Map[String, String]) extends Action
case class ModifyEdge(id:(Int,Int), newVals:String) extends Action
case object GetUserTasks extends Action
case class UpdateUserTasks(tasks:Option[List[TaskInfo]]) extends Action
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

//Tasks Actions
case object GetAllTasks extends Action
case class UpdateAllTasks(tasks:Option[List[TaskInfo]]) extends Action
case class DeleteTask(taskID:Int) extends Action
case class GoldTask(taskID:Int) extends Action
case class DeleteResult(res:ResultStatus) extends Action
case class GoldResult(res:ResultStatus) extends Action
case class GetTaskGraphs(id:Int) extends Action
case class UpdateTask(lfs: Map[User, TripsLFViz]) extends Action

//Golds Actions
case object GetAllGolds extends Action
case class UpdateAllGolds(tasks:Option[List[GoldInfo]]) extends Action
case class GetGoldGraphs(id:Int) extends Action
case class UpdateGoldGraph(lfs: TripsLFViz) extends Action
case class SearchGold(str:String) extends Action


case class TaskViewerHelper(tasks:Pot[List[TaskInfo]], deleteRes:Pot[ResultStatus],
                            goldRes:Pot[ResultStatus], graphs:Pot[Map[User,TripsLFViz]])

case class GoldViewerHelper(golds:Pot[List[GoldInfo]], rollbackRes:Pot[ResultStatus],
                            parseRes:Pot[ResultStatus], graph:Pot[TripsLFViz])

// The base model of our application
case class RootModel(user: Pot[User], statistics: Pot[AnnotationStatistics], editorHelper: EditorHelper,
                     taskHelper:TaskViewerHelper, goldHelper:GoldViewerHelper)


class GoldHandler[M](modelRW: ModelRW[M, GoldViewerHelper], user:User) extends ActionHandler(modelRW){
  override def handle = {
    case GetAllGolds =>
      updated(modelRW().modify(_.golds).setTo(modelRW().golds.pending()),
        Effect(
          AjaxClient[Api2].getAllGolds(user).call().map(golds =>
            UpdateAllGolds(golds.map(t => t.sortBy(_.id)))
          )
        )
      )

    case UpdateAllGolds(golds) =>
      if(golds.isDefined)
        updated(modelRW().modify(_.golds).setTo(Ready(golds.get)))
      else
        updated(modelRW().modify(_.golds).setTo(modelRW().golds.unavailable()))


    case GetGoldGraphs(taskID) =>
      updated(modelRW().modify(_.graph).setTo(modelRW().graph.pending()),
        Effect(
          AjaxClient[Api2].getGold(taskID).call().map(gld =>  {
            UpdateGoldGraph(GraphUtil.layoutGraph(gld))
          })
        )
      )

    case UpdateGoldGraph(lfs) => updated(modelRW().modify(_.graph).setTo(Ready(lfs)))

    case SearchGold(str) =>
      updated(modelRW().modify(_.golds).setTo(modelRW().golds.pending()),
        Effect(
          AjaxClient[Api2].searchGolds(str).call().map(golds =>
            UpdateAllGolds(golds.map(t => t.sortBy(_.id)))
          )
        )
      )

  }
}


class TasksHandler[M](modelRW: ModelRW[M, TaskViewerHelper], user:User) extends ActionHandler(modelRW){
  override def handle = {
    case GetAllTasks =>
      updated(modelRW().modify(_.tasks).setTo(modelRW().tasks.pending()),
        Effect(
          AjaxClient[Api2].getAllTasks(user).call().map(tasks =>
            UpdateAllTasks(tasks.map(t => t.sortBy(_.id)))
          )
        )
      )

    case UpdateAllTasks(tasks) =>
      if(tasks.isDefined)
        updated(modelRW().modify(_.tasks).setTo(Ready(tasks.get)))
      else
        updated(modelRW().modify(_.tasks).setTo(modelRW().tasks.unavailable()))

    case DeleteTask(taskID) =>
      updated(modelRW().modify(_.deleteRes).setTo(modelRW().deleteRes.pending()),
        Effect(
          AjaxClient[Api2].deleteTask(user, taskID).call().map(res => DeleteResult(res))
        )
      )

    case GoldTask(taskID) =>
      updated(modelRW().modify(_.goldRes).setTo(modelRW().goldRes.pending()),
        Effect(
          AjaxClient[Api2].goldTask(user, taskID).call().map(res => GoldResult(res))
        )
      )

    case DeleteResult(res) => updated(modelRW().modify(_.deleteRes).setTo(Ready(res)))

    case GoldResult(res) => updated(modelRW().modify(_.goldRes).setTo(Ready(res)))

    case GetTaskGraphs(taskID) =>
      updated(modelRW().modify(_.graphs).setTo(modelRW().graphs.pending()),
        Effect(
          AjaxClient[Api2].getTask(user, taskID).call().map(tsks =>  {
            val ts = tsks.map {case(u,t) => u -> GraphUtil.layoutGraph(t)}
            UpdateTask(ts)
          })
        )
      )

    case UpdateTask(lfs) => updated(modelRW().modify(_.graphs).setTo(Ready(lfs)))

    case AddCommentAndSave(taskID, value) =>
      updated(
        modelRW().modify(_.tasks).setTo(modelRW().tasks.map(ti =>{
          val x = ti.partition(_.id == taskID)
          val x1 = x._1.map(ti2 => ti2.copy(comments = Comment(user, value) :: ti2.comments)) ::: x._2
          x1.sortBy(_.id)
        }))
      )

  }
}

class UserHandler[M](modelRW: ModelRW[M, Pot[User]]) extends ActionHandler(modelRW) {
  override def handle = {
    case SignIn(username) => effectOnly(Effect(AjaxClient[Api2].signIn(username).call().map(user => UpdateUser(user))))
    case UpdateUser(user) => updated(Ready(user), Effect(Future(UpdateUserG(user))))
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
case class EditorHelper(user: User, lf:Pot[TripsLFViz], taskInfos:Pot[List[TaskInfo]],
                        dragState:Option[DragState], selectState:SelectState,
                        alternatives: Alternatives, undoManager: UndoManager)


class GraphHandler[M](modelRW: ModelRW[M, EditorHelper]) extends ActionHandler(modelRW) {
  override def handle = {
    //val user = userP.headOption.getOrElse(User.invalidUser)
    case UpdateUserG(user) => updated(modelRW().modify(_.user).setTo(user))

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
      println(modelRW().user)
      updated(modelRW().copy(taskInfos = modelRW().taskInfos.pending()),
        Effect(
          AjaxClient[Api2].getUserTasks(modelRW().user).call().map(tasks =>
            UpdateUserTasks(tasks)
          )
        )
      )

    case UpdateUserTasks(tasks) =>
      if (tasks.isDefined)
        updated(modelRW().copy(taskInfos = Ready(tasks.get)))
      else
        updated(modelRW().copy(taskInfos = modelRW().taskInfos.unavailable()))

    case GetUserGraph(id) =>
      updated(modelRW().copy(lf = modelRW().lf.pending()),
        Effect(
          AjaxClient[Api2].getUserGraph(modelRW().user, id).call().map(lf =>
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


    case AddComment(taskID, value) =>
      updated(
        modelRW().modify(_.taskInfos).setTo(modelRW().taskInfos.map(ti =>{
          val x = ti.partition(_.id == taskID)
          val x1 = x._1.map(ti2 =>
            ti2.copy(comments = Comment(modelRW().user, value) :: ti2.comments)) ::: x._2
          x1.sortBy(_.id)
        }))
      )
  }
}


// Application circuit
object MainCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // initial application model
  override protected def initialModel = RootModel(Empty, Empty,
    EditorHelper(User.invalidUser,
      Empty, Empty, None, SelectState(None, None),Alternatives(Empty, Empty), UndoManager(List.empty)
    ), TaskViewerHelper(Empty,Empty,Empty, Empty), GoldViewerHelper(Empty, Empty,Empty,Empty)
  )

  // combine all handlers into one
  override protected val actionHandler = composeHandlers(
    new UserHandler(zoomRW(_.user)((m, u) => m.copy(user = u))),
    new StatisticHandler(
      zoomRW(_.statistics)((m, u) => m.copy(statistics = u)), zoom(_.user.headOption.getOrElse(User.invalidUser)).value),

    new GraphHandler(zoomRW(_.editorHelper)((m, u) => m.copy(editorHelper = u))/*,
      userP = zoomRW(_.user)((x1,x2) => x2)*/),

    new TasksHandler(zoomRW(_.taskHelper)((m, u) => m.copy(taskHelper = u)),
      user = zoom(_.user.headOption.getOrElse(User.invalidUser)).value),

    new GoldHandler(zoomRW(_.goldHelper)((m, u) => m.copy(goldHelper = u)),
      user = zoom(_.user.headOption.getOrElse(User.invalidUser)).value)
  )
}