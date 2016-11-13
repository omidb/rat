package rat.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import rat.client.components.Bootstrap.CommonStyle
import scalacss.ScalaCssReact._
import scalacss.Defaults._

import scala.language.implicitConversions

object EditableText {

  case class State(strValue: String, editMode: Boolean = false)
  case class Props(onSave: (String) => Callback, value: String)

  class Backend($: BackendScope[Props, State]) {

    def onChange(e: ReactEventI) = $.setState(State(e.currentTarget.value, editMode = true))
    def onSave() = {
      val dispatchAction = $.props.flatMap(x => x.onSave($.state.runNow().strValue))
      dispatchAction >> $.modState(_.copy(editMode = false))
    }


    def render(state: State, props: Props) = {
      if(state.editMode)
        <.div(^.className := "input-group",
          <.input(^.`type` := "text", ^.className := "form-control", ^.placeholder := "insert something",
            ^.defaultValue := props.value, ^.onChange ==> onChange),
          <.span(^.className := "input-group-btn",
            <.button(^.className := "btn btn-success", ^.`type` := "button",
              ^.onClick --> onSave, Icon.check))
        )
      else
        <.div(^.onClick --> $.modState(_.copy(editMode = true)), props.value)
    }
  }
  val component = ReactComponentB[Props]("EditableText")
    .initialState_P(p => State(p.value))
    .renderBackend[Backend]
    .build

  def apply(onSave: (String) => Callback, value: String) = component(Props(onSave, value))
}

//
object EditableTag {

  @inline private def bss = GlobalStyles.bootstrapStyles

  case class State(strValue: String, editMode: Boolean = false)
  case class Props(onSave: (String) => Callback, value: String, style: CommonStyle.Value)

  class Backend($: BackendScope[Props, State]) {

    def onChange(e: ReactEventI) = $.setState(State(e.currentTarget.value, editMode = true))
    def onSave() = {
      val dispatchAction = $.props.flatMap(x => x.onSave($.state.runNow().strValue))
      dispatchAction >> $.modState(_.copy(editMode = false))
    }


    def render(state: State, props: Props) = {
      if(state.editMode)
        <.div(^.className := "input-group",
          <.input(^.`type` := "text", ^.className := "form-control", ^.placeholder := "insert something",
            ^.defaultValue := props.value, ^.onChange ==> onChange),
          <.span(^.className := "input-group-btn",
            <.button(^.className := "btn btn-success", ^.`type` := "button",
              ^.onClick --> onSave, Icon.check))
        )
      else
        <.div(bss.labelOpt(props.style), ^.onClick --> $.modState(_.copy(editMode = true)), props.value)
    }
  }
  val component = ReactComponentB[Props]("EditableText")
    .initialState_P(p => State(p.value))
    .renderBackend[Backend]
    .build

  def apply(onSave: (String) => Callback, value: String, style: CommonStyle.Value = CommonStyle.default) =
    component(Props(onSave, value, style))
}
