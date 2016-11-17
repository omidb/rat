package rat.client.modules

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import rat.client.SPAMain.{DashboardLoc, EditorLoc, Loc, TasksLoc}
import rat.client.components.Bootstrap.CommonStyle
import rat.client.components.Icon._
import rat.client.components._

import scalacss.ScalaCssReact._

object MainMenu {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[Loc], currentLoc: Loc, editorCount:Int)

  private case class MenuItem(idx: Int, label: (Props) => ReactNode, icon: Icon, location: Loc)

  private def buildGrEditMenu(props: Props) = {
    Seq(
      <.span(^.key := "seq1", "Editor"),
      if (props.editorCount > 0)
        <.span(^.key := "seq2", bss.labelOpt(CommonStyle.danger), bss.labelAsBadge, props.editorCount)
      else <.span(^.key := "seq2")
    )
  }

  private val menuItems = Seq(
    MenuItem(1, _ => "Dashboard", Icon.dashboard, DashboardLoc),
    MenuItem(2, p => buildGrEditMenu(p), Icon.scissors, EditorLoc),
    MenuItem(3, p => "Tasks", Icon.barChart, TasksLoc)
  )

  private class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      <.ul(bss.navbar)(
        // build a list of menu items
        for (item <- menuItems) yield {
          <.li(^.className := "nav-item", ^.key := item.idx, (props.currentLoc == item.location) ?= (^.className := "nav-item active"),
            props.router.link(item.location)(^.className:="nav-link", item.icon, " ", item.label(props))
          )
        }
      )
    }
  }

  private val component = ReactComponentB[Props]("MainMenu")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[Loc], currentLoc: Loc, editorCount:Int = 0): ReactElement =
    component(Props(ctl, currentLoc, editorCount))
}
