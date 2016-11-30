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
import rat.client.components.Table.{TableHeading, TableHeadingItem, TableItem}
import rat.client.components.piechart.PieChartValue
import rat.client.components._
import rat.client.services.{CalculateAgreement, GetStatistics, StatisticHelper}
import rat.shared.AnnotationStatistics



object Dashboard {

  @inline private def bss = GlobalStyles.bootstrapStyles


  class Backend($: BackendScope[ModelProxy[StatisticHelper], Unit]) {

    def render(props: ModelProxy[StatisticHelper]) = {
      val proxy = props.zoom(_.annotationStatistics)
      <.div(
        Card(Card.Props(header = Some(Card.Header("Dashboard",
          Button(Button.Props(proxy.dispatch(CalculateAgreement),
            addStyles = Seq(bss.pullRight, bss.buttonXS)), "Recalculate Agreement")))),

            proxy().renderPending(_ => <.div(^.textAlign := "center", Icon.spinnerAnimateLarge)),
            proxy().renderFailed(ex => <.div(<.p("Failed to load"))),
            proxy().renderReady(annotationStats => {
              val dta = List(
                PieChartValue(s"MyTasks:${annotationStats.myRemaining}", annotationStats.myRemaining),
                PieChartValue(s"Bronze:${annotationStats.allSubmitted}", annotationStats.allSubmitted),
                PieChartValue(s"Silver:${annotationStats.ready2Gold}", annotationStats.ready2Gold),
                PieChartValue(s"Gold:${annotationStats.goldSize}", annotationStats.goldSize)
              )
              <.div(
                <.div(^.className := "row",
                  <.div(^.className := "col-md-12", ^.textAlign := "center",
                    if (annotationStats.allSubmitted > 0) {
                      piechart.PieChart(dta.filter(_.piVal > 0))
                    }
                    else
                      <.h5("No submitted task found!")
                  )
                ),
                if(annotationStats.ontologyStat.nonEmpty)
                  <.div(^.className := "row",
                    <.div(^.className := "col-md-12",
                      Table(
                        tableHeading = TableHeading(
                          List(
                            TableHeadingItem("ID", false, false, Callback.empty),
                            TableHeadingItem("Word", false, false, Callback.empty),
                            TableHeadingItem("New ONT Type", false, false, Callback.empty),
                            TableHeadingItem("Current Words", false, false, Callback.empty)
                          )),
                        tableItems = annotationStats.ontologyStat.map(t =>
                          TableItem(
                            List(
                              List(t.id),
                              List(t.word),
                              List(t.tpe),
                              List(t.currentWords.mkString(", "))
                            )
                          )
                        )
                      )
                    )
                  )
                else <.div()
              )
            })
        )
      )
    }

  }

  val component = ReactComponentB[ModelProxy[StatisticHelper]]("Dashboard")
    .stateless
    .renderBackend[Backend]
    .componentWillMount(scope =>
      scope.props.dispatch(GetStatistics)
    )
    .build


  def apply(proxy: ModelProxy[StatisticHelper]) = component(proxy)

}
