package rat.client.modules

import rat.client.components._
import com.github.omidb.nlp.toolsInterface.TripsLF
import dgraph.DGraph
import diode.ActionBatch
import diode.data.Pot
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.svg.all.{g, height, svg, transform, viewBox, width}
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo, ReactComponentB, ReactDOM, ReactEventI, ReactMouseEventI, ReactNode, Ref, TopNode}
import rat.client.services._
import diode.react.ReactPot._
import diode.react._
import japgolly.scalajs.react.extra.{EventListener, OnUnmount}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.KeyboardEvent
import rat.client.components.Bootstrap.Card.Header
import rat.client.components.Bootstrap.{BTag, Button, ButtonList, Card, CommonStyle, Label, OButton, Panel, Popover}
import rat.shared._

import scala.language.implicitConversions
import scala.scalajs.js
import scalacss.ScalaCssReact._
import scalacss.Defaults._
import Table.{TableHeading, TableHeadingItem, TableItem}
object TaskViewer {

  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(proxy: ModelProxy[TaskViewerHelper])

  case class State(columnID: Int, isAscending: Boolean, deleteCheck: Boolean,
                   selectedUsers: (Option[User], Option[User]), zoomSize:Double, selectedTask:Option[Int])

  class Backend($: BackendScope[Props, State]) extends OnUnmount {

    def rev(tsk:List[TaskInfo], rev:Boolean) = if(rev) tsk.reverse else tsk

    def render(s: State, p: Props) = {
      println("We are in Task Viewer")
      val tasksPot = p.proxy.zoom(_.tasks)
      val grs = p.proxy.zoom(_.graphs)
      <.div(
        <.div(^.className := "row",
          <.div(^.className := "col-sm-12",
            Card(
              Card.Props(addStyles = Seq(^.height := "230px", ^.overflow := "auto")),
              tasksPot().renderEmpty(<.h5(^.textAlign := "center", "Please Refresh")),
              tasksPot().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
              tasksPot().renderFailed(ex => <.div(<.p("Failed to load, Please Refresh"))),
              tasksPot().renderReady(tasks1 => {

                val tasks = if (s.columnID == 0)
                  rev(tasks1.sortBy(_.id), s.isAscending)
                else if (s.columnID == 1)
                  rev(tasks1.sortBy(_.sentence), s.isAscending)
                else if (s.columnID == 3)
                  rev(tasks1.sortBy(_.agreement), s.isAscending)
                else tasks1
                Table(
                  tableHeading = TableHeading(
                    List(
                      TableHeadingItem("ID", s.columnID == 0 && s.isAscending, s.columnID == 0 && !s.isAscending,
                        $.modState(_.copy(isAscending = !s.isAscending, columnID = 0))),
                      TableHeadingItem("Sentence", s.columnID == 1 && s.isAscending, s.columnID == 1 && !s.isAscending,
                        $.modState(_.copy(isAscending = !s.isAscending, columnID = 1))),
                      TableHeadingItem("Users", s.columnID == 2 && s.isAscending, s.columnID == 2 && !s.isAscending, Callback.empty),
                      TableHeadingItem("Agreement", s.columnID == 3 && s.isAscending, s.columnID == 3 && !s.isAscending,
                        $.modState(_.copy(isAscending = !s.isAscending, columnID = 3))),
                      TableHeadingItem("Tools", s.columnID == 4 && s.isAscending, s.columnID == 4 && !s.isAscending,
                        Callback.empty)
                    )),
                  tableItems = tasks.map(t =>
                    TableItem(
                      List(
                        List(t.id),
                        List(t.sentence),
                        t.userStat.toList.map {
                          case (u, Impossible) => BTag(u, style = CommonStyle.warning)
                          case (u, Submitted) => BTag(u, style = CommonStyle.info)
                          case (u, UnEdited) => BTag(u, style = CommonStyle.danger)
                        },
                        List(f"${t.agreement}%1.2f"),
                        List(
                          if(p.proxy.zoom(_.user).apply().access == "all")
                            OButton(OButton.Props(
                              addStyles = Seq(bss.pullRight, bss.buttonXS),
                              style = CommonStyle.danger,
                              onClick =
                                if (s.deleteCheck)
                                  $.modState(_.copy(deleteCheck = false)) >> p.proxy.dispatch(DeleteTask(t.id))
                                else $.modState(_.copy(deleteCheck = true))
                            ), if (s.deleteCheck) "Sure?" else "Delete")
                          else <.div(),

                          OButton(OButton.Props(
                            addStyles = Seq(bss.pullRight, bss.buttonXS),
                            style = CommonStyle.primary,
                            onClick = p.proxy.dispatch(GetTaskGraphs(t.id)) >> $.modState(_.copy(selectedTask = Some(t.id)))), "Select"),

                          if (t.isFinished && p.proxy.zoom(_.user).apply().access == "all")
                            OButton(OButton.Props(
                              addStyles = Seq(bss.pullRight, bss.buttonXS),
                              style = CommonStyle.warning,
                              onClick = p.proxy.dispatch(GoldTask(t.id)) >> $.modState(_.copy(selectedTask = None))), "Gold")
                          else <.div()
                        )
                      )
                    )
                  )
                )
              }
              )
            )
          )
        ),
        <.div(^.className := "row",
          <.div(^.className := "col-sm-10",
            Card(
              Card.Props(),
              grs().renderEmpty(<.h5(^.textAlign := "center", "Please Select a Task")),
              grs().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
              grs().renderFailed(ex => <.div(<.p("Failed to load, Please Select Again"))),
              grs().renderReady(grphs =>
                <.div(
                  <.div(^.className := "row",
                    <.div(^.className := "col-sm-4", ^.textAlign:="left",
                      <.div(^.className:="btn-group btn-group-sm", ^.role:="group",
                        grphs.toList.sortBy(_._1.id).map { case(u, tlf) =>
                          Button(Button.Props(
                            style =
                              if(s.selectedUsers._1.isDefined && u.id == s.selectedUsers._1.get.id) CommonStyle.danger
                              else CommonStyle.secondary,
                            onClick = $.modState(_.copy(selectedUsers = s.selectedUsers.copy(_1 = Some(u))))), u.id)
                        }
                      )
                    ),
                    <.div(^.className := "col-sm-4",^.textAlign:="center",
                      <.div(^.className:="btn-group btn-group-sm", ^.role:="group",
                          OButton(OButton.Props(
                            style =
                              if(s.zoomSize == 2.0) CommonStyle.danger
                              else CommonStyle.secondary,
                            onClick = $.modState(_.copy(zoomSize = 2.0))
                          ),"x0.5"),
                          OButton(OButton.Props(
                            style =
                              if(s.zoomSize == 1.5) CommonStyle.danger
                              else CommonStyle.secondary,
                            onClick = $.modState(_.copy(zoomSize = 1.5))
                          ), "x0.75"),
                          OButton(OButton.Props(
                            style =
                              if(s.zoomSize == 1.0) CommonStyle.danger
                              else CommonStyle.secondary,
                            onClick = $.modState(_.copy(zoomSize = 1.0))
                          ), "x1.0")


                      )
                    ),
                    <.div(^.className := "col-sm-4",^.textAlign:="right",
                      <.div(^.className:="btn-group btn-group-sm", ^.role:="group",
                        grphs.toList.sortBy(_._1.id).map { case(u, tlf) =>
                          Button(Button.Props(
                            addStyles = Seq(bss.pullRight),
                            style =
                              if(s.selectedUsers._2.isDefined && u.id == s.selectedUsers._2.get.id) CommonStyle.danger
                              else CommonStyle.secondary,
                            onClick = $.modState(_.copy(selectedUsers = s.selectedUsers.copy(_2 = Some(u))))), u.id)
                        }
                      )
                    )
                  ),
                  <.div(^.className := "row",
                    <.div(^.className := "col-sm-6",
                      Card(
                        Card.Props(addStyles = Seq(^.height := "600px", ^.overflow := "auto")),
                        if(s.selectedUsers._1.isDefined)
                          svg(width := 2000, height := 2000, viewBox:= s"0,0,${2000*s.zoomSize},${2000*s.zoomSize}",
                            g(transform := GraphUtil.move(Array(5, 5)),
                              GraphComponent(
                                GraphComponent.Props(
                                  lf = grphs(s.selectedUsers._1.get)
                                )
                              )
                            )
                          )
                        else <.h6("Please select a user")
                      )
                    ),
                    <.div(^.className := "col-sm-6",
                      Card(
                        Card.Props(addStyles = Seq(^.height := "600px", ^.overflow := "auto")),
                        if(s.selectedUsers._2.isDefined)
                          svg(width := 2000, height := 2000, viewBox:= s"0,0,${2000*s.zoomSize},${2000*s.zoomSize}",
                            g(transform := GraphUtil.move(Array(5, 5)),
                              GraphComponent(
                                GraphComponent.Props(
                                  lf = grphs(s.selectedUsers._2.get)
                                )
                              )
                            )
                          )
                        else <.h6("Please select a user")
                      )
                    )
                  )
                )
              )
            )
          ),
          <.div(^.className := "col-sm-2",
            Card(
              Card.Props(),
              grs().renderEmpty(<.h5(^.textAlign := "center", "Please Select a Task")),
              grs().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
              grs().renderFailed(ex => <.div(<.p("Failed to load, Please Select Again"))),
              grs().renderReady(grphs =>
                if(tasksPot().isReady && s.selectedTask.isDefined)
                  CommentComp(
                    tasksPot().head.find(_.id == s.selectedTask.get).get.comments,
                    newComment = (st) => p.proxy.dispatch(AddCommentAndSave(s.selectedTask.get, st))
                  )
                else
                  <.h5(^.textAlign := "center", "Please Select a Task")
              )
            )
          )
        )
      )
    }


  }

  val component = ReactComponentB[Props]("tasksComp")
    .initialState(State(0, isAscending = false, deleteCheck = false, selectedUsers = (None, None), zoomSize = 1.0, selectedTask = None))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.props.proxy.dispatch(GetAllTasks))
      .componentWillReceiveProps(p => {
        if(p.nextProps.proxy().graphs.isReady)
          if(p.nextProps.proxy().graphs.head.size == 1)
            p.$.modState(_.copy(selectedUsers =
              (Some(p.nextProps.proxy().graphs.head.head._1), Some(p.nextProps.proxy().graphs.head.head._1))))
          else
            p.$.modState(_.copy(
              selectedUsers =
                (Some(p.nextProps.proxy().graphs.head.toList.head._1),
                  Some(p.nextProps.proxy().graphs.head.toList(1)._1))))
        else Callback.empty
      })
    .build

  def apply(proxy: ModelProxy[TaskViewerHelper]) = component(Props(proxy))
}

