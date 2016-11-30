package rat.client.components

import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactNode, ReactEventI}
import japgolly.scalajs.react.vdom.prefix_<^._
import rat.client.components.Bootstrap.{BTag, Button, ButtonList, Card, CommonStyle}
import rat.client.services.GetUserGraph
import rat.shared.Comment

object CommentComp {

  case class Props(comments: List[Comment] = List(), newComment: (String) => Callback)
  case class State(nc:String)

  class Backend($: BackendScope[Props, State]) {

    def onChange(e: ReactEventI) = {
      val tv = e.currentTarget.value
      $.modState(_.copy(nc = tv))
    }

    def render(s:State, p: Props) = {
      Card(
        Card.Props(),
        <.div(^.className := "input-group",
          <.input(^.tpe := "text", ^.className := "form-control", ^.placeholder := "Comment ...",
            ^.onChange ==> onChange),
          <.span(^.className := "input-group-btn",
            Button(Button.Props(onClick = p.newComment(s.nc)), Icon.send)
          )
        ),
//        Card(
//          Card.Props(),
        <.div(
          p.comments.map(c =>
            <.div(BTag(c.user, style = CommonStyle.info), c.value)
          )
        )
      )
    }

  }

  val component = ReactComponentB[Props]("Comment")
    .initialState(State(""))
    .renderBackend[Backend]
    .build

  def emptyFun(s:String) = Callback.empty
  def apply(comments: List[Comment] = List(), newComment: (String) => Callback = emptyFun) =
    component(Props(comments, newComment))
}
