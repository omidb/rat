package rat.client.modules

import rat.client.components._
import com.github.omidb.nlp.toolsInterface.TripsLF
import dgraph.DGraph
import diode.ActionBatch
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.svg.all.{g, height, svg, transform, width}
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo, ReactComponentB, ReactDOM, ReactEventI, ReactMouseEventI, ReactNode, Ref, TopNode}
import rat.client.services._
import diode.react.ReactPot._
import diode.react._
import japgolly.scalajs.react.extra.{EventListener, OnUnmount}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.KeyboardEvent
import rat.client.components.Bootstrap.Card.Header
import rat.client.components.Bootstrap.{BTag, Button, ButtonList, Card, CommonStyle, Label, Modal, OButton, Panel}
import rat.shared._

import scala.language.implicitConversions
import scala.scalajs.js
import scalacss.ScalaCssReact._
import scalacss.Defaults._

object Editor {

  @inline private def bss = GlobalStyles.bootstrapStyles

  val shift = RVector(50, 5)

  case class Props(proxy: ModelProxy[EditorHelper])

  case class State(mouseLocation: RPoint = RPoint(), key: Boolean = false,
                   edgeAlterSelected: Option[EdgeAlternative] = None, nodeAlterSelected: Option[NodeAlternative] = None,
                   infoSearch: String = "",
                   nodeEdgeSearch: String = "", showGold:Boolean = false,
                   switchTasks:Boolean=false, switchTab:Boolean=false)

  class Backend($: BackendScope[Props, State]) extends OnUnmount {

    val svgRef = Ref("svg")

    def onMove(e: ReactMouseEventI) = {
      val location = RPoint(getPos(e.clientX, e.clientY)._1 - shift.x, getPos(e.clientX, e.clientY)._2 - shift.y)
      $.props.flatMap(p =>
        if (p.proxy().dragState.isDefined)
          p.proxy.dispatch(MouseMove(location))
        else
          Callback.empty
      )
    }

    def onKeyDown(e: KeyboardEvent): Callback = $.modState(_.copy(key = e.ctrlKey || e.shiftKey))

    def onKeyUp(e: KeyboardEvent) = $.modState(_.copy(key = e.ctrlKey || e.shiftKey))


    def onNodeMouseUp(id: Int): (ReactMouseEventI) => Callback = {
      def x(e: ReactMouseEventI) = {
        e.stopPropagation()
        val key = $.state.runNow().key
        $.props.flatMap(p => {
          val drgState = p.proxy().dragState
          if (drgState.isDefined) {
            val newEdgeId = if (drgState.get.isStart) (id, drgState.get.id._2) else (drgState.get.id._1, id)
            val newEd = p.proxy().lf.map(tz => tz.graphViz.edges(drgState.get.id).value)
            p.proxy.dispatch(
              ActionBatch(
                EdgeDragDisable,
                RemoveEdge(drgState.get.id),
                AddEdge(newEdgeId, newEd.head),
                LayoutLF))
          }
          else if (p.proxy().selectState.isNode.isDefined && key) {
            val newEdgeID = (p.proxy().selectState.isNode.get, id)
            p.proxy.dispatch(ActionBatch(AddEdge(newEdgeID, EdgeViz.empty), SelectAction(None, Some(newEdgeID)), LayoutLF))
          }
          else p.proxy.dispatch(
            ActionBatch(
              SelectAction(Some(id), None),
              SearchAlterNode(
                if(p.proxy().lf.map(_.graphViz.nodes(id).value.value.getOrElse("type", "")).head.startsWith("sa_"))
                  "sa_"
                else p.proxy().lf.map(_.graphViz.nodes(id).value.value.getOrElse("word", "")).head))) >>
            $.modState(_.copy(nodeEdgeSearch = ""))
        })
      }
      x
    }

    def getPos(x: Double, y: Double) = {
      val r = ReactDOM.findDOMNode(svgRef($).get).asInstanceOf[TopNode].getBoundingClientRect()
      (x - r.left, y - r.top)
    }


    def onChange(e: ReactEventI) = {
      val tv = e.currentTarget.value
      $.modState(_.copy(infoSearch = tv))
    }

    def onChangeNodeEdge(e: ReactEventI) = {
      val tv = e.currentTarget.value
      $.modState(_.copy(nodeEdgeSearch = tv))
    }


    def createToolBar(s: State, p: Props, lf: TripsLFViz): ReactNode = {
      if (p.proxy().selectState.isNode.isDefined) {
        val selectedNode = p.proxy().lf.head.graphViz.nodes(p.proxy().selectState.isNode.get)
        val isSplitable = selectedNode.value.value.contains("word") &&
          selectedNode.value.value("word").contains("_")
        val delete = OButton(OButton.Props(
          p.proxy.dispatch(
            ActionBatch(
              RemoveNode(selectedNode.id),
              SelectAction(None, None),
              LayoutLF)
          ),
          addStyles = Seq(bss.pullRight),
          style = CommonStyle.danger), Icon.close)
        val add = OButton(OButton.Props(p.proxy.dispatch(
          ActionBatch(AddNode(selectedNode.value), LayoutLF)),
          CommonStyle.success), "Duplicate")
        <.div(add, delete)
      }
      else if (p.proxy().selectState.isEdge.isDefined) {
        val selectedEdge = p.proxy().lf.head.graphViz.edges(p.proxy().selectState.isEdge.get)
        val delete = OButton(
          OButton.Props(
            p.proxy.dispatch(
              ActionBatch(RemoveEdge((selectedEdge.from, selectedEdge.to)),
                SelectAction(None, None), LayoutLF)), CommonStyle.danger, addStyles = Seq(bss.pullRight)), Icon.close)
        delete
      }
      else {
        val newNode = NodeViz(Map("-" -> "-"))
        val add = OButton(OButton.Props(p.proxy.dispatch(
          ActionBatch(AddNode(newNode), LayoutLF)), CommonStyle.success), Icon.plus)
        add
      }
    }

    def getTasksOrGolds(s:State, p:Props):Callback = {
      println(s"show golds ${s.showGold}")
      val tsksOrGolds =
        if(!s.showGold)
           p.proxy.dispatch(ActionBatch(GetGoldsForEdit, UpdateSelectedTask(None))) >>
             $.modState(_.copy(showGold = true, edgeAlterSelected = None, nodeAlterSelected = None))
         else p.proxy.dispatch(ActionBatch(GetUserTasks, UpdateSelectedTask(None))) >> $.modState(_.copy(
          showGold = false, edgeAlterSelected = None, nodeAlterSelected = None))

      tsksOrGolds >> p.proxy.dispatch(UpdateEditorGraph(None))
    }

    def saveSwitchTask(s:State, p:Props, save:Boolean = true, switch:Boolean = true):Callback = {
//      println(s"____________________$save ___ $switch ____________________")
//      val saveCallback = if(save && s.lastSelectedTask.isDefined) saveLastTask(s,p) else Callback.empty
//      val switchCallback = if(switch) {
//        if (s.showGold)
//          p.proxy.dispatch(GetGoldGraphForEdit(s.selectedTask.get))
//        else
//          p.proxy.dispatch(GetUserGraph(s.selectedTask.get))
//      } else Callback.empty

      p.proxy.dispatch(ClearUndo) >> p.proxy.dispatch(SelectAction(None, None)) >>
        {
          if(save && p.proxy.zoom(_.lastSelectedTask).apply().isDefined) saveLastTask(s,p) else Callback.empty
        } >> {
        if(switch) {
          if (s.showGold)
            p.proxy.dispatch(GetGoldGraphForEdit(p.proxy.zoom(_.selectedTask.get).apply()))
          else
            p.proxy.dispatch(GetUserGraph(p.proxy.zoom(_.selectedTask.get).apply()))
        } else Callback.empty
      }
    }

    def saveLastTask(s:State, p:Props):Callback = {
//      println(s"Saving ${s.lastSelectedTask}")
      if (s.showGold)
        p.proxy.dispatch(SaveEditedGold(p.proxy.zoom(_.lastSelectedTask.get).apply()))
      else
        p.proxy.dispatch(SaveEditedGraph(p.proxy.zoom(_.lastSelectedTask.get).apply()))
    }

    def render(s: State, p: Props) = {
      val pr = p.proxy
      val lfPot = pr.zoom(_.lf)
      val alters = pr.zoom(_.alternatives)

      val graphInfos = pr.zoom(_.taskInfos)

      val maxX = if (p.proxy().lf.exists(_.graphViz.edges.nonEmpty))
        p.proxy().lf.head.graphViz.edges.maxBy(_._2.value.labelLoc.topLeft.x)._2.value.labelLoc.topLeft.x + 400
      else 500

      val maxY = if (p.proxy().lf.exists(_.graphViz.edges.nonEmpty))
        p.proxy().lf.head.graphViz.edges.maxBy(_._2.value.labelLoc.topLeft.y)._2.value.labelLoc.topLeft.y + 400
      else 500


      <.div(^.className := "row",
        if (s.switchTasks || s.switchTab) modal(s, p) else <.div(),
        <.div(^.className := "col-sm-2", ^.overflowY := "auto", ^.maxHeight := 650,
          Card(
            Card.Props(
              header = Some(
                Header(
                  if(p.proxy.zoom(_.user.access).apply() == "all")
                    <.form(
                      <.div(^.className := "form-check",
                        <.label(^.className := "form-check-label",
                          <.input(^.`type` := "checkbox", ^.className := "form-check-input", ^.checked := s.showGold,
                            ^.onChange --> {
                              if(pr().selectedTask.isDefined && !pr.zoom(_.undoManager.actionStack.isEmpty).value) {
                                println("I am here")
                                $.modState(_.copy(switchTab = true, showGold = !s.showGold))
                              }
                              else {
                                println("I am here 2")
                                getTasksOrGolds(s,p)
                              }
                            }
                          ), "Show Golds"
                        )
                      )
                    )
                  else <.div()
                )),
              footer = None
            ),
            graphInfos().renderEmpty(<.h5(^.textAlign := "center", "Please Refresh")),
            graphInfos().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
            graphInfos().renderFailed(ex => <.div(<.p("Failed to load"))),
            graphInfos().renderReady(grInfos =>
              <.div(
                <.div(^.className := "input-group",
                  <.input(^.tpe := "text", ^.className := "form-control", ^.placeholder := "Search for id ...",
                    ^.onChange ==> onChange),
                  <.span(^.className := "input-group-btn",
                    Button(Button.Props(onClick = Callback.empty), Icon.search)
                  )
                ),
                ButtonList(
                  grInfos.filter(_.id.toString.contains(s.infoSearch)).map { gi => {
                    //println(gi.userStat)
                    val stl = if(s.showGold) gi.userStat.head._2 else  gi.userStat(p.proxy.zoom(_.user).apply().id)
                    val styles = stl match {
                      case UnEdited => CommonStyle.default
                      case Submitted => CommonStyle.success
                      case Impossible => CommonStyle.warning
                    }
                    ButtonList.ButtonItem(
                      <.div(BTag(gi.id.toString, style = styles), " " + gi.sentence),
                      active = pr().selectedTask.contains(gi.id),
                      style = styles,
                      onClick =
                        if (lfPot().isReady && !pr.zoom(_.undoManager.actionStack.isEmpty).value) {
                          pr.dispatch(ActionBatch(UpdateSelectedLastTask(p.proxy.zoom(_.selectedTask).apply()),
                            UpdateSelectedTask(Some(gi.id)))) >> $.modState(_.copy(switchTasks = true))

                        }
                        else {
//                          println("Doing Normal")
                          p.proxy.dispatch(ClearUndo) >> p.proxy.dispatch(SelectAction(None, None)) >> {
                            if (!s.showGold)
                              pr.dispatch(ActionBatch(UpdateSelectedLastTask(pr().selectedTask),
                                UpdateSelectedTask(Some(gi.id)), GetUserGraph(gi.id)))
                            else
                              pr.dispatch(ActionBatch(UpdateSelectedLastTask(pr().selectedTask),
                                UpdateSelectedTask(Some(gi.id)), GetGoldGraphForEdit(gi.id)))

                          }
                        }
                    )
                  }
                  }
                )
              )
            )
          )
        ),
        <.div(^.className := "col-sm-7",

          lfPot().renderEmpty(<.h5(^.textAlign := "center", "Please select a graph")),
          lfPot().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
          lfPot().renderFailed(ex => <.div(<.p("Failed to load"))),
          lfPot().renderReady(lf =>

            Card(
              Card.Props(
                header = Some(Card.Header(createToolBar(s, p, lf)))),
              <.div(^.overflowY := "scroll", ^.overflowX := "scroll", ^.maxHeight := 650,
                if (pr().selectedTask.isDefined) {
                  val content = graphInfos().head.find(_.id == pr().selectedTask.get).map(x =>
                    <.div(BTag(x.id.toString, style = CommonStyle.info), x.sentence))
                  <.div(^.className := "alert alert-info", ^.role := "alert", content.getOrElse(<.div()))
                }
                else <.div(),
                svg(^.ref := svgRef, width := maxX, height := maxY,
                  ^.onMouseMove ==> onMove,
                  ^.onMouseUp --> pr.dispatch(SelectAction(None, None)),
                  g(transform := GraphUtil.move(Array(shift.x, shift.y)),
                    GraphComponent(
                      GraphComponent.Props(
                        lf,
                        nodeMouseUp = onNodeMouseUp,

                        edgeStartOver = (id: (Int, Int)) => pr.dispatch(EdgeEndAction(isEnter = true, isStart = true, id)),
                        edgeEndOver = (id: (Int, Int)) => pr.dispatch(EdgeEndAction(isEnter = true, isStart = false, id)),
                        edgeStartOut = (id: (Int, Int)) => pr.dispatch(EdgeEndAction(isEnter = false, isStart = true, id)),
                        edgeEndOut = (id: (Int, Int)) => pr.dispatch(EdgeEndAction(isEnter = false, isStart = false, id)),

                        edgeStartDown = (id: (Int, Int)) =>
                          pr.dispatch(EdgeDragEnable(location = s.mouseLocation, isStart = true, id = id)),
                        edgeEndDown = (id: (Int, Int)) =>
                          pr.dispatch(EdgeDragEnable(location = s.mouseLocation, isStart = false, id = id)),
                        edgeClick = (id: (Int, Int)) => pr.dispatch(
                          ActionBatch(
                            SelectAction(None, Some(id)),
                            SearchAlterEdge(lf.graphViz.edges(id).value.value)
                          ))
                      )
                    )
                  )
                )
              )

            )
          )
        ),
        <.div(^.className := "col-sm-3", ^.overflowY := "auto", ^.maxHeight := 650,
          Card(
            Card.Props(
              header = Some(Card.Header(
                OButton(
                  OButton.Props(
                    p.proxy.dispatch(
                      ActionBatch(
                        UndoAction,
                        LayoutLF
                      )
                    ),
                    style = CommonStyle.danger,
                    addStyles = Seq(),
                    disabled = pr().undoManager.actionStack.isEmpty
                  ), Icon.undo
                ),
                OButton(
                  OButton.Props(
                    onClick =
                      if (pr().selectedTask.isDefined)
                        p.proxy.dispatch(
                          if (s.showGold)
                            SaveEditedGold(pr().selectedTask.get)
                          else
                            SaveEditedGraph(pr().selectedTask.get)
                        )
                      else Callback.empty,
                    style = CommonStyle.success,
                    addStyles = Seq(bss.pullRight)
                  ), "Save ", Icon.save
                )
              ))
            ),
            <.div(
              lfPot().renderReady(lf => {
                if (pr().selectState.isNode.isDefined) {
                  val mps = lf.graphViz.nodes(pr().selectState.isNode.get).value.value
                  <.div(
                    Card(Card.Props(),
                      mps.map { case (ky, vl) =>
                        <.div(^.className := "row",
                          <.div(^.className := "col-sm-5",
                            EditableText(
                              (s: String) => pr.dispatch(ModifyNode(pr().selectState.isNode.get,
                                mps.filterNot(_._1 == ky).updated(s, vl))), ky)),
                          <.div(^.className := "col-sm-5",
                            EditableTag(
                              (s: String) => pr.dispatch(ModifyNode(pr().selectState.isNode.get, mps.updated(ky, s))), vl,
                              CommonStyle.info)),
                          <.div(^.className := "col-sm-2",
                            Button(Button.Props(
                              addStyles = Seq(bss.pullRight, bss.buttonXS),
                              style = CommonStyle.danger,
                              onClick = pr.dispatch(
                                ModifyNode(pr().selectState.isNode.get, mps.filterNot(x => x._1 == ky)))
                            ), Icon.minus)
                          )
                        )
                      }.toList,
                      <.div(^.className := "row",
                        <.div(^.className := "col-sm-5"),
                        <.div(^.className := "col-sm-2",
                          <.button(^.className := "btn btn-default",
                            ^.tpe := "button",
                            ^.onClick --> pr.dispatch(ModifyNode(pr().selectState.isNode.get, mps.updated("-", "-"))),
                            Icon.plus)
                        ),
                        <.div(^.className := "col-sm-5")
                      )
                    ),

                    Card(Card.Props(),
                      <.div(
                        <.div(^.className := "input-group",
                          <.input(^.tpe := "text", ^.className := "form-control", ^.placeholder := "Search for...",
                            ^.onChange ==> onChangeNodeEdge),
                          <.span(^.className := "input-group-btn",
                            Button(Button.Props(onClick = pr.dispatch(SearchAlterNode(s.nodeEdgeSearch))), Icon.search)
                          )
                        ),
                        alters().nodeAlters.renderEmpty(<.h5(^.textAlign := "center", "Please select a graph")),
                        alters().nodeAlters.renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
                        alters().nodeAlters.renderFailed(ex => <.div(<.p("Failed to load"))),
                        alters().nodeAlters.renderReady(altrs =>
                          ButtonList(
                            altrs.map(al =>
                              ButtonList.ButtonItem(
                                <.div(BTag(al.typ, style = if (al.isWordNetMapping) CommonStyle.danger else CommonStyle.info),
                                  <.p(al.path2root.mkString(">"), ^.fontSize := "12px")),
                                active = al.typ == lf.graphViz.nodes(pr().selectState.isNode.get).value.value.getOrElse("type", "-"),
                                onClick = pr.dispatch(
                                  ActionBatch(
                                    ModifyNode(pr().selectState.isNode.get,
                                      lf.graphViz.nodes(pr().selectState.isNode.get).value.value.updated("type", al.typ)),
                                    LayoutLF
                                  )
                                )
                              )
                            ),
                            ellipsis = false
                          )
                        )
                      )
                    )
                  )
                }
                else if (pr().selectState.isEdge.isDefined) {
                  Card(Card.Props(),
                    <.div(
                      <.div(^.className := "input-group",
                        <.input(^.tpe := "text", ^.className := "form-control", ^.placeholder := "Search for...",
                          ^.onChange ==> onChangeNodeEdge),
                        <.span(^.className := "input-group-btn",
                          Button(Button.Props(onClick = pr.dispatch(SearchAlterEdge(s.nodeEdgeSearch))), Icon.search)
                        )
                      ),
                      ButtonList(
                        SharedUtil.allEdgeAlterMappings("all").filter(_.value.contains(s.nodeEdgeSearch))
                          .map(al =>
                          ButtonList.ButtonItem(
                            <.div(BTag(al.value, style = CommonStyle.info)),
                            active = al.value == lf.graphViz.edges(pr().selectState.isEdge.get).value.value,
                            onClick = pr.dispatch(
                              ActionBatch(
                                ModifyEdge(pr().selectState.isEdge.get, al.value),
                                LayoutLF
                              )
                            )
                          )
                        )
                      )

                    )
                  )
                }
                else {
                  if (pr().selectedTask.isDefined && graphInfos().isReady)
                    List(CommentComp(
                      graphInfos().head.find(_.id == pr().selectedTask.get).get.comments,
                      newComment = (st) => pr.dispatch(AddComment(pr().selectedTask.get, st))
                    ))
                  else List(<.h5("Select a Node or an Edge"))
                }
              }
              )
            )
          )

        )
      )
    }

    def modal(s:State, p:Props) = {
      Modal(Modal.Props(
        // header contains a cancel button (X)
        header = hide =>
          <.span(<.h4(
              if(s.switchTasks) "Are you sure that you want change task?"
              else if(s.switchTab) "Are you sure that you want to switch to golds?"
              else "Close me!")),
        // footer has the OK button that submits the form before hiding it
        footer = hide => {
//          println(s"Last Selected ${s.lastSelectedTask}")
//          println(s"Selected ${s.selectedTask}")
          <.span(
            Button(
              Button.Props(hide >> {
                //println(s"selected task: ${s.selectedTask}")
                if (s.switchTasks) saveSwitchTask(s, p, save = true, switch = true)
                else if(s.switchTab) saveSwitchTask(s, p, save = true, switch = false) >> getTasksOrGolds(s, p)
                else Callback.empty
              }),
              "Save And Leave"),
            Button(
              Button.Props(
                if (s.switchTasks) hide >> saveSwitchTask(s, p, save = false, switch = true)
                else if (s.switchTab) hide >> saveSwitchTask(s, p, save = false, switch = false) >>getTasksOrGolds(s, p)
                else hide
              ),
              "Leave")
          )
        },
        // this is called after the modal has been hidden (animation is completed)
        closed = $.modState(_.copy(switchTab = false, switchTasks = false)))//formClosed(s, p)),


      )
    }
  }

  val component = ReactComponentB[Props]("graphComp")
    .initialState({
      println("initilizing again")
      State(RPoint(), key = false)
    })
    .renderBackend[Backend]
    .componentWillMount(scope =>
      scope.props.proxy.dispatch(GetUserTasks)
    )
    .configure(
      EventListener[KeyboardEvent].install("keydown", _.backend.onKeyDown, _ => dom.window),
      EventListener[KeyboardEvent].install("keyup", _.backend.onKeyUp, _ => dom.window)
    )
    .build

  def apply(proxy: ModelProxy[EditorHelper]) = component(Props(proxy))
}
