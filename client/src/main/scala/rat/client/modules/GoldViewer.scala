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

import scala.scalajs.js.URIUtils

object GoldViewer {

  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(proxy: ModelProxy[GoldViewerHelper])

  case class State(columnID: Int, isAscending: Boolean, deleteCheck: Boolean,
                   zoomSize:Double, selectedTask:Option[Int], searchStr:String = "", lisp:Option[String] = None)

  class Backend($: BackendScope[Props, State]) extends OnUnmount {

    def rev(tsk:List[TaskInfo], rev:Boolean) = if(rev) tsk.reverse else tsk

    def onChange(e: ReactEventI) = {
      val tv = e.currentTarget.value
      $.modState(_.copy(searchStr= tv))
    }


    def render(s: State, p: Props) = {
      val tasksPot = p.proxy.zoom(_.golds)
      val grs = p.proxy.zoom(_.graph)
      val lisp = p.proxy.zoom(_.lisp)
      <.div(
        <.div(^.className := "row",
          <.div(^.className := "col-sm-6",
            <.form(
              <.div(^.className:="form-group",
                <.label(^.`for`:="exampleTextarea", "Search"),
                <.textarea(^.className:="form-control", ^.id:="exampleTextarea", ^.rows:="3", ^.onChange ==> onChange)
              ),
              <.div(^.className:="row",
                <.div(^.className:="col-sm-4",
                  <.button(^.`type`:="submit", ^.className:="btn btn-primary", "Search",
                    ^.onClick --> p.proxy.dispatch(SearchGold(s.searchStr)))
                ),
                <.div(^.className:="col-sm-4",
                    OButton(
                      OButton.Props(
                        if(tasksPot().isReady)
                          p.proxy.dispatch(GetLispForGolds(tasksPot().get.map(_.id)))
                        else Callback.empty,
                        style = CommonStyle.success
                      ), "To Lisp"
                    )
                ),
                <.div(^.className:="col-sm-4",
                  lisp().renderEmpty(""),
                  lisp().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimate)),
                  lisp().renderFailed(ex => "Faild"),
                  lisp().renderReady(lsp =>
                    <.a(^.download := "golds.lisp", ^.href := "data:application/octet-stream;charset=utf-8," +
                    URIUtils.encodeURIComponent(lsp), "download"))
                )
              )
            )
          )//,
//          if(p.proxy.zoom(_.user).apply().access == "all")
//            <.div(^.className := "col-sm-6",
//              <.form(
//                <.div(^.className:="form-group",
//                  <.label(^.`for`:="exampleTextarea", "Search"),
//                  <.textarea(^.className:="form-control", ^.id:="exampleTextarea", ^.rows:="3", ^.onChange ==> onChange)
//                ),
//                <.div(^.className:="row",
//                  <.div(^.className:="col-sm-4",
//                    <.button(^.`type`:="submit", ^.className:="btn btn-primary", "Search",
//                      ^.onClick --> p.proxy.dispatch(SearchGold(s.searchStr)))
//                  )
//                )
//              )
//            )
//          else <.div()
        ),
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
                else tasks1
                Table(
                  tableHeading = TableHeading(
                    List(
                      TableHeadingItem("ID", s.columnID == 0 && s.isAscending, s.columnID == 0 && !s.isAscending,
                        $.modState(_.copy(isAscending = !s.isAscending, columnID = 0))),
                      TableHeadingItem("Sentence", s.columnID == 1 && s.isAscending, s.columnID == 1 && !s.isAscending,
                        $.modState(_.copy(isAscending = !s.isAscending, columnID = 1))),
                      TableHeadingItem("Users", s.columnID == 2 && s.isAscending, s.columnID == 2 && !s.isAscending,
                        Callback.empty),
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
                        List(
                          OButton(OButton.Props(
                            addStyles = Seq(bss.pullRight, bss.buttonXS),
                            style = CommonStyle.primary,
                            onClick = p.proxy.dispatch(GetGoldGraphs(t.id)) >>
                              $.modState(_.copy(selectedTask = Some(t.id)))), "Select"),

                          if(p.proxy.zoom(_.user).apply().access == "all")
                            OButton(OButton.Props(
                              addStyles = Seq(bss.pullRight, bss.buttonXS),
                              style = CommonStyle.danger,
                              onClick = p.proxy.dispatch(RollbackGold(t.id))
                            ), "Rollback")
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
          <.div(^.className := "col-sm-12",
            Card(
              Card.Props(),
              grs().renderEmpty(<.h5(^.textAlign := "center", "Please Select a Task")),
              grs().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
              grs().renderFailed(ex => <.div(<.p("Failed to load, Please Select Again"))),
              grs().renderReady(grphs =>
                <.div(
                  <.div(^.className := "row",
                    <.div(^.className := "col-sm-4", ^.textAlign:="left", <.div()),
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
                      <.div(^.className:="btn-group btn-group-sm", ^.role:="group", <.div())
                    )
                  ),
                  <.div(^.className := "row",
                    <.div(^.className := "col-sm-12",
                      Card(
                        Card.Props(addStyles = Seq(^.height := "600px", ^.overflow := "auto")),
                        if(s.selectedTask.isDefined)
                          svg(width := 2000, height := 2000, viewBox:= s"0,0,${2000*s.zoomSize},${2000*s.zoomSize}",
                            g(transform := GraphUtil.move(Array(5, 5)),
                              GraphComponent(
                                GraphComponent.Props(
                                  lf = grphs
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
          )
        )
      )
    }


  }

  val component = ReactComponentB[Props]("goldsComp")
    .initialState(State(0, isAscending = false, deleteCheck = false, zoomSize = 1.0, selectedTask = None))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.props.proxy.dispatch(GetAllGolds))
    .build

  def apply(proxy: ModelProxy[GoldViewerHelper]) = component(Props(proxy))
}

