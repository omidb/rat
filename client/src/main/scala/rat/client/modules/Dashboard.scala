package rat.client.modules

import diode.data.Pot
import diode.react.ModelProxy
import diode.react.ReactPot._
import diode.react._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.SvgTags.{rect, svg}
import japgolly.scalajs.react.vdom.SvgAttrs.{height, style, width}
import rat.client.components.Bootstrap.{Button, Card, Panel}
import rat.client.components.piechart.PieChartValue
import rat.client.components._
import rat.client.services.{CalculateAgreement, GetStatistics}
import rat.shared.AnnotationStatistics



object Dashboard {

  @inline private def bss = GlobalStyles.bootstrapStyles


  class Backend($: BackendScope[ModelProxy[Pot[AnnotationStatistics]], Unit]) {

    def render(props: ModelProxy[Pot[AnnotationStatistics]]) = {
      val proxy = props
      <.div(
        Card(Card.Props(header = Some(Card.Header("Dashboard",
          Button(Button.Props(proxy.dispatch(CalculateAgreement),
            addStyles = Seq(bss.pullRight, bss.buttonXS)), "Recalculate Agreement")))),
          <.div(
            proxy().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
            proxy().renderFailed(ex => <.div(<.p("Failed to load"))),
            proxy().renderReady(annotationStats => {
              val dta = List(
                PieChartValue(s"MyTasks:${annotationStats.myRemaining}", annotationStats.myRemaining),
                PieChartValue(s"Bronze:${annotationStats.allSubmitted}", annotationStats.allSubmitted),
                PieChartValue(s"Silver:${annotationStats.ready2Gold}", annotationStats.ready2Gold),
                PieChartValue(s"Gold:${annotationStats.goldSize}", annotationStats.goldSize)
              )

              <.div(^.className := "row",
                <.div(^.className := "col-md-12", ^.textAlign := "center",
                  if (annotationStats.allSubmitted > 0) {
                    piechart.PieChart(dta.filter(_.piVal > 0))
                  }
                  else
                    <.h5("No submitted task found!")
                )
              )
            }
            )
          )
        )
      )
    }

  }

  val component = ReactComponentB[ModelProxy[Pot[AnnotationStatistics]]]("Dashboard")
    .stateless
    .renderBackend[Backend]
    .componentWillMount(scope =>
      scope.props.dispatch(GetStatistics)
    )
    .build


  def apply(proxy: ModelProxy[Pot[AnnotationStatistics]]) = component(proxy)

}
