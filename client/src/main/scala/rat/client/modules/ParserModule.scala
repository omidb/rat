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

object ParserModule {
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(proxy: ModelProxy[Pot[Map[String, Int]]])

  case class State(selectedDomains:Map[String, Boolean], domain:String = "ROC stories", parserType:String = "step-dev",
                   useSkeleton:Boolean = false, lines:String = "", running:Boolean = false)



  class Backend($: BackendScope[Props, State]) extends OnUnmount {


    def onChange(e: ReactEventI) = {
      val tv = e.currentTarget.value
      $.modState(_.copy(domain= tv))
    }

    def onChangeLines(e: ReactEventI) = {
      val tv = e.currentTarget.value
      $.modState(_.copy(lines= tv))
    }

    def render(s: State, p: Props) = {
      val stats = p.proxy()
      <.div(^.className := "row",
        <.div(^.className := "col-sm-9",
          Card(
            Card.Props(addStyles = Seq(^.height := "600px", ^.overflow := "auto")),
            <.textarea(^.className:="form-control", ^.id:="exampleTextarea", ^.rows:="15", ^.onChange ==> onChangeLines,
              s.lines)
          )
        ),
        <.div(^.className := "col-sm-3",
          Card(
            Card.Props(),
            <.div(
              Card(
                Card.Props(),
                stats.renderEmpty(<.h5(^.textAlign := "center", "Please Refresh")),
                stats.renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
                stats.renderFailed(ex => <.div(<.p("Failed to load, Please Refresh"))),
                stats.renderReady(statList => {
                  statList.map { case (user, st) =>
                    <.div(^.className := "row",
                      <.div(^.className := "col-sm-6", BTag(s"$user -> $st", style = CommonStyle.info)),
                      <.div(^.className := "col-sm-6", ^.textAlign := "right",
                        <.input(^.`type` := "checkbox", ^.className := "form-check-input",
                          ^.onChange --> {
                            if (s.selectedDomains.contains(user))
                              $.modState(_.copy(selectedDomains = s.selectedDomains.updated(user, !s.selectedDomains(user))))
                            else
                              $.modState(_.copy(selectedDomains = s.selectedDomains.updated(user, false)))
                          },
                          ^.checked := s.selectedDomains.getOrElse(user, false)
                        )
                      )
                    )
                  }
                }
              )),
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
                          <.input(^.`type`:="text", ^.className:="form-control", ^.onChange ==> onChange)
//                          <.span(
//                            <.input(^.tpe := "checkbox", ^.checked := s.useSkeleton,
//                              ^.onChange --> $.modState(_.copy(useSkeleton = !s.useSkeleton))),
//                            BTag("Use Skeleton", style = CommonStyle.danger)
//                          )
                        ),
                        <.div(^.className := "col-md-6",^.textAlign:="right",
                          <.button(^.className := "btn btn-danger", ^.`type` := "button", "Start!",
                            ^.onClick --> p.proxy.dispatch(
                              ParseLines(
                                s.parserType,
                                s.domain,
                                s.selectedDomains.filter(_._2).keys.toList,
                                s.lines.split("\n").toList))
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
      )
    }

  }

  val component = ReactComponentB[Props]("Evaluation")
    .initialState(State(Map.empty))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.props.proxy.dispatch(ParseLines("step", "ROc stories", List.empty, List.empty)))
    .componentWillReceiveProps(p => {
      val s = p.currentState
      if(p.currentProps.proxy().isReady) {
        val newTs = p.currentProps.proxy().get
          .filter(t => !s.selectedDomains.contains(t._1))
          .map(t => t._1 -> false)
        println(s"new TS : $newTs")
        p.$.modState(_.copy(selectedDomains = s.selectedDomains ++ newTs))
      }
      else Callback.empty
    })
    .build

  def apply(proxy: ModelProxy[Pot[Map[String, Int]]]) = component(Props(proxy))
}


