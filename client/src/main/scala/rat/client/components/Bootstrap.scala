package rat.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.language.implicitConversions
import scala.scalajs.js
import scalacss.ScalaCssReact._
import scalacss.Defaults._

/**
  * Common Bootstrap components for scalajs-react
  */
object Bootstrap {

  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  @js.native
  trait BootstrapJQuery extends JQuery {
    def modal(action: String): BootstrapJQuery = js.native
    def modal(options: js.Any): BootstrapJQuery = js.native
  }

  implicit def jq2bootstrap(jq: JQuery): BootstrapJQuery = jq.asInstanceOf[BootstrapJQuery]

  // Common Bootstrap contextual styles
  object CommonStyle extends Enumeration {
    val default, primary, secondary, success, info, warning, danger = Value
  }

  object Button {

    case class Props(onClick: Callback, style: CommonStyle.Value = CommonStyle.default, addStyles: Seq[StyleA] = Seq())

    val component = ReactComponentB[Props]("Button")
      .renderPC((_, p, c) =>
        <.button(bss.buttonOpt(p.style), p.addStyles,  ^.tpe := "button", ^.onClick --> p.onClick, c)
      ).build

    def apply(props: Props, children: ReactNode*) = component(props, children: _*)
    def apply() = component
  }

  object OButton {

    case class Props(onClick: Callback, style: CommonStyle.Value = CommonStyle.default, addStyles: Seq[StyleA] = Seq(),
                     disabled:Boolean = false)

    val component = ReactComponentB[Props]("Button")
      .renderPC((_, p, c) =>
        <.button(bss.buttonOutlineOpt(p.style), p.addStyles, ^.tpe := "button", p.disabled ?= (^.disabled:=true),
          ^.onClick --> p.onClick, c)
      ).build

    def apply(props: Props, children: ReactNode*) = component(props, children: _*)
    def apply() = component
  }

  object ButtonList {

    case class ButtonItem(content: ReactNode, onClick: Callback,
                          active: Boolean = false, addStyles: Seq[StyleA] = Seq())

    case class Props(items: List[ButtonItem], ellipsis:Boolean= true)

    val component = ReactComponentB[Props]("ButtonList")
      .render_P(p =>
        <.ul(bss.listGroup.listGroup)(
          p.items.map(item =>
            <.button(
//              if(item.style == CommonStyle.default)
//                ^.className:=bss.listGroup.item
//              else
//                bss.listGroup.itemOpt(item.style),
//              item.addStyles,
              if(item.active)
                ^.className := "list-group-item list-group-item-action active"
              else
                ^.className := s"list-group-item list-group-item-action",
              item.addStyles,
              ^.tpe := "button",
              p.ellipsis ?= (^.whiteSpace := "nowrap"),
              p.ellipsis ?= (^.overflow := "hidden"),
                p.ellipsis ?= (^.textOverflow := "ellipsis"),
              ^.onClick --> item.onClick, item.content)
          )
        )
      ).build

    def apply(items:List[ButtonItem], ellipsis:Boolean= true) = component(Props(items, ellipsis))

    def apply() = component
  }

  object Panel {

    case class Props(heading: String, style: CommonStyle.Value = CommonStyle.default)

    val component = ReactComponentB[Props]("Panel")
      .renderPC((_, p, c) =>
        <.div(bss.panelOpt(p.style),
          <.div(bss.panelHeading, p.heading),
          <.div(bss.panelBody, c)
        )
      ).build

    def apply(props: Props, children: ReactNode*) = component(props, children: _*)
    def apply() = component
  }

  object Card {

    case class Header(nodes: ReactNode*)

    case class Footer(nodes: ReactNode*)

    case class Props(header: Option[Header] = None,
                     footer: Option[Footer] = None,
                     style: CommonStyle.Value = CommonStyle.default,
                     addStyles: Seq[TagMod] = Seq()
                    )

    val component = ReactComponentB[Props]("Panel")
      .renderPC((_, p, c) =>
        <.div(bss.cardOpt(p.style), p.addStyles,
          p.header.map(he => <.div(bss.cardHeading, he.nodes)).getOrElse(EmptyTag),
          <.div(bss.cardBlock, c)
        )
      ).build

    def apply(props: Props, children: ReactNode*) = component(props, children: _*)

    def apply() = component
  }

  object Label {

    case class Props(text: String, style: CommonStyle.Value = CommonStyle.default, onClick: Callback)

    val component = ReactComponentB[Props]("Label")
      .stateless
      .render_P(p =>
        <.div(bss.labelOpt(p.style), ^.onClick --> p.onClick, p.text)
      ).build

    def apply(text: String, style: CommonStyle.Value = CommonStyle.default, onClick: Callback = Callback.empty) =
      component(Props(text, style, onClick))
  }

  object Popover {

    case class Props(header: List[ReactNode] = List(), inside: List[ReactNode] = List(), left:Boolean = true)

    val component = ReactComponentB[Props]("Label")
      .stateless
      .render_P(p => {
        val side = if (p.left) "left" else "right"
        <.div(^.className := s"popover popover-$side",
          <.h3(^.className := "popover-title", p.header),
          <.div(^.className := "popover-content",
            p.inside
          )
        )
      }
      ).build

    def apply(header: List[ReactNode] = List(), inside: List[ReactNode] = List(), left:Boolean = true) =
      component(Props(header, inside, left))
  }

  object BTag {

    case class Props(text: String, pill:Boolean = false, style: CommonStyle.Value = CommonStyle.default, onClick: Callback)

    val component = ReactComponentB[Props]("Tag")
      .stateless
      .render_P(p =>
        <.div(bss.tagOpt(p.style), ^.onClick --> p.onClick, p.text)
      ).build

    def apply(text: String, pill:Boolean = false,
              style: CommonStyle.Value = CommonStyle.default,
              onClick: Callback = Callback.empty) =
      component(Props(text, pill, style, onClick))
  }

  object Modal {

    // header and footer are functions, so that they can get access to the the hide() function for their buttons
    case class Props(header: Callback => ReactNode, footer: Callback => ReactNode, closed: Callback, backdrop: Boolean = true,
                     keyboard: Boolean = true)

    class Backend(t: BackendScope[Props, Unit]) {
      def hide = Callback {
        // instruct Bootstrap to hide the modal
        jQuery(t.getDOMNode()).modal("hide")
      }

      // jQuery event handler to be fired when the modal has been hidden
      def hidden(e: JQueryEventObject): js.Any = {
        // inform the owner of the component that the modal was closed/hidden
        t.props.flatMap(_.closed).runNow()
      }

      def render(p: Props, c: PropsChildren) = {
        val modalStyle = bss.modal
        <.div(modalStyle.modal, modalStyle.fade, ^.role := "dialog", ^.aria.hidden := true,
          <.div(modalStyle.dialog,
            <.div(modalStyle.content,
              <.div(modalStyle.header, p.header(hide)),
              <.div(modalStyle.body, c),
              <.div(modalStyle.footer, p.footer(hide))
            )
          )
        )
      }
    }

    val component = ReactComponentB[Props]("Modal")
      .renderBackend[Backend]
      .componentDidMount(scope => Callback {
        val p = scope.props
        // instruct Bootstrap to show the modal
        jQuery(scope.getDOMNode()).modal(js.Dynamic.literal("backdrop" -> p.backdrop, "keyboard" -> p.keyboard, "show" -> true))
        // register event listener to be notified when the modal is closed
        jQuery(scope.getDOMNode()).on("hidden.bs.modal", null, null, scope.backend.hidden _)
      })
      .build

    def apply(props: Props, children: ReactElement*) = component(props, children: _*)
    def apply() = component
  }

}
