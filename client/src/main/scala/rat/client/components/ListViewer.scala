package rat.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import rat.client.components.Bootstrap.CommonStyle
import scala.language.implicitConversions
import scalacss.ScalaCssReact._




case class ListItem(node:ReactNode, action:Callback, styles: CommonStyle.Value = CommonStyle.default)
object ListViewer {

  @inline private def bss = GlobalStyles.bootstrapStyles


  class Backend($: BackendScope[List[ListItem], Unit]) {

    def render(props: List[ListItem]) = {
      <.div(
        props.map(item =>
          <.button(bss.buttonOpt(item.styles), ^.`type` := "button",
            ^.width := "100%",
            ^.paddingTop := 6,
            ^.paddingBottom := 6,
            ^.maxHeight := 80,
            ^.onClick --> item.action,
            <.div(^.className := "text-left", item.node)
          )
        )
      )
    }
  }

  val component = ReactComponentB[List[ListItem]]("ListViewer")
    .stateless
    .renderBackend[Backend]
    .build


  def apply(items: List[ListItem]) = component(items)

}
