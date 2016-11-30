package rat.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import rat.client.components.Bootstrap.CommonStyle
import scalacss.ScalaCssReact._


object Table {

  @inline private def bss = GlobalStyles.bootstrapStyles

  // if ascending ^ otherwise downward

  case class TableHeadingItem(label:String, isAscending:Boolean = false, isDescending:Boolean= false,
                              onClick:Callback = Callback.empty)
  case class TableHeading(items: List[TableHeadingItem])

  case class TableItem(items: List[List[ReactNode]])

  case class Props(tableHeading: TableHeading, tableItems: List[TableItem])

  class Backend($: BackendScope[Props, Unit]) {


    def render(props: Props) = {

      <.table(^.className := "table table-striped table-sm",
        <.thead(
          <.tr(
            props.tableHeading.items.zipWithIndex.map { case (th, ind) =>
              val caret =
                if (th.isAscending)
                  Icon.caretUp
                else if(th.isDescending)
                  Icon.caretDown
                else Icon.empty
                <.th(^.textAlign:= "center", th.label + " ", caret, ^.onClick --> th.onClick)
            }
          )
        ),
        <.tbody(
          for (row <- props.tableItems) yield
            <.tr(
              row.items.map(item => <.td(^.textAlign:= "center", item))
            )
        )
      )
    }
  }

  val component = ReactComponentB[Props]("EditableText")
    .stateless
    .renderBackend[Backend]
    .build

  def apply(tableHeading: TableHeading, tableItems: List[TableItem]) =
    component(Props(tableHeading, tableItems))
}