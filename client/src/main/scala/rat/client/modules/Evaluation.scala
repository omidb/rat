package rat.client.modules

import rat.client.components._
import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsServers}
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

object Evaluation {

  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(proxy: ModelProxy[EvaluationHelper])

  case class State(selectedDomains:Map[String, Boolean], parserType:String = "step-dev", useSkeleton:Boolean = false,
                   lastID:Int = 0, running:Boolean = false)

  class Backend($: BackendScope[Props, State]) extends OnUnmount {

    def render(s: State, p: Props) = {
      val tasksPot = p.proxy.zoom(_.tasks)
      val results = p.proxy.zoom(_.evalResult)
      println("-------render--------")
      println(results())
      <.div(^.className := "row",
        <.div(^.className := "col-sm-9",
          Card(
            Card.Props(addStyles = Seq(^.height := "600px", ^.overflow := "auto")),
            tasksPot().renderEmpty(<.h5(^.textAlign := "center", "Please Refresh")),
            tasksPot().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
            tasksPot().renderFailed(ex => <.div(<.p("Failed to load, Please Refresh"))),
            tasksPot().renderReady(tasks => {
              tasks.foreach(x => println(x._1))
              Table(
                tableHeading = TableHeading(
                  List(
                    TableHeadingItem("ID"),
                    TableHeadingItem("Sentence"),
                    TableHeadingItem("Domain"),
                    TableHeadingItem("Users"),
                    TableHeadingItem("Results")
                  )),
                tableItems = tasks.filter(t => s.selectedDomains.contains(t._1) && s.selectedDomains(t._1))
                  .values.toList.flatten.map { t =>
                  TableItem(
                    List(
                      List(t.id),
                      List(t.sentence),
                      List(BTag(t.domain, style = CommonStyle.info)),
                      t.userStat.toList.map {
                        case (u, Impossible) => BTag(u, style = CommonStyle.warning)
                        case (u, Submitted) => BTag(u, style = CommonStyle.info)
                        case (u, UnEdited) => BTag(u, style = CommonStyle.danger)
                      },
                      if(results().contains(t.id))
                        List(
                          results()(t.id).renderEmpty("-"),
                          results()(t.id).renderPending(_ => Icon.spinnerAnimate),
                          results()(t.id).renderFailed(ex => "Failed"),
                          results()(t.id).renderReady(res => res)
                        )
                      else List("-")
                    )
                  )
                }
              )
            }
            )
          )
        )
        ,
        <.div(^.className := "col-sm-3",
          Card(
            Card.Props(),
            tasksPot().renderEmpty(<.h5(^.textAlign := "center", "Please Refresh")),
            tasksPot().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
            tasksPot().renderFailed(ex => <.div(<.p("Failed to load, Please Refresh"))),
            tasksPot().renderReady(tasks => {
              <.div(
                Card(
                  Card.Props(),
                  tasks.keys.toList.map(t => {
                    <.div(^.className := "row",
                      <.div(^.className := "col-sm-6", BTag(t, style = CommonStyle.info)),
                      <.div(^.className := "col-sm-6", ^.textAlign := "right",
                        <.input(^.`type` := "checkbox", ^.className := "form-check-input",
                          ^.onChange --> {
                            if (s.selectedDomains.contains(t))
                              $.modState(_.copy(selectedDomains = s.selectedDomains.updated(t, !s.selectedDomains(t))))
                            else
                              $.modState(_.copy(selectedDomains = s.selectedDomains.updated(t, false)))
                          },
                          ^.checked := s.selectedDomains.getOrElse(t, false)
                        )
                      )
                    )
                  })
                ),
                Card(
                  Card.Props(),
                  <.div(^.className := "row",
                    <.div(^.className := "col-sm-12",
                      <.form(
                        <.p("Select the parser:"),
                        SharedUtil.availableParsers.map(prsName =>
                          <.div(^.className := "radio",
                            <.label(<.input(^.`type` := "radio", ^.value := prsName, ^.name := "parsopt",
                              ^.onClick --> $.modState(_.copy(parserType = prsName))), prsName)
                          )
                        ),
                        <.div(^.className := "row",
                          <.div(^.className := "col-md-6",
                            <.span(
                              <.input(^.tpe := "checkbox", ^.checked := s.useSkeleton,
                                ^.onChange --> $.modState(_.copy(useSkeleton = !s.useSkeleton))),
                              BTag("Use Skeleton", style = CommonStyle.danger)
                            )
                          ),
                          <.div(^.className := "col-md-6",^.textAlign:="right",
                            <.button(^.className := "btn btn-danger", ^.`type` := "button", "Start!",
                              ^.onClick -->
                                p.proxy.dispatch(
                                  RecursiveEvaluate(
                                    tasks.filter(t => s.selectedDomains.contains(t._1) && s.selectedDomains(t._1))
                                      .values.toList.flatten.map(_.id), 0,s.parserType
                                  )
                                )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            })
          )
        )
      )
    }


  }

  val component = ReactComponentB[Props]("Evaluation")
    .initialState(State(Map.empty))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.props.proxy.dispatch(GetAllGoldsForEvaluation))
      .componentWillReceiveProps(p => {
        val s = p.currentState
        if(p.currentProps.proxy.zoom(_.tasks).apply().isReady) {
          val newTs = p.currentProps.proxy.zoom(_.tasks).apply().get
            .filter(t => !s.selectedDomains.contains(t._1))
            .map(t => t._1 -> false)
          println(s"new TS : $newTs")
          p.$.modState(_.copy(selectedDomains = s.selectedDomains ++ newTs))
        }
        else Callback.empty
      })
    .build

  def apply(proxy: ModelProxy[EvaluationHelper]) = component(Props(proxy))
}

