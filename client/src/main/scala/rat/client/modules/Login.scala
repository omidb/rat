package rat.client.modules

import diode.data.Pot
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import rat.client.SPAMain.{DashboardLoc, Loc}
import rat.client.components.Bootstrap.{Button, CommonStyle}
import rat.client.components.{GlobalStyles, Icon}
import rat.client.services.SignIn
import rat.shared.{User, UserName}


object Login {

  @inline private def bss = GlobalStyles.bootstrapStyles

  case class State(userName: UserName)

  case class Props(proxy: ModelProxy[Pot[User]], router: RouterCtl[Loc])

  val redirector = ReactComponentB[RouterCtl[Loc]]("Simple")
    .stateless
    .render_P(x => <.h1("Helooo"))
    .componentWillMount(c => {
      println("trying to push it to dashboard")
      c.props.set(DashboardLoc)
    })
    .build


  class Backend($: BackendScope[Props, State]) {

    def userChange(e: ReactEventI) = {
      val text = e.target.value
      $.modState(s => s.copy(userName = s.userName.copy(user = text)))
    }


    def passChange(e: ReactEventI) = {
      val text = e.target.value
      $.modState(s => s.copy(userName = s.userName.copy(pass = text)))
    }

    def render(p:Props, s: State) = {
      val validModel = p.proxy.modelReader.zoom(_.exists(_.valid))
      println(s"In login module and user ${validModel()}")
      if (!validModel()) {
        <.div(
          <.form(^.className := "form-signin",
            <.h2(^.className := "form-signin-heading", "Please Sign In"),
            <.label(^.`for` := "inputEmail", ^.className := "sr-only", "Email Address"),
            <.input(^.`type` := "email", ^.id := "inputEmail", ^.className := "form-control",
              ^.placeholder := "User Name", ^.required := true, ^.autoFocus := true, ^.onChange ==> userChange),
            <.label(^.`for` := "inputPassword", ^.className := "sr-only", "Password"),
            <.input(^.`type` := "password", ^.id := "inputPassword", ^.className := "form-control",
              ^.placeholder := "Password", ^.required := true, ^.onChange ==> passChange),
            Button(Button.Props(p.proxy.dispatch(SignIn(s.userName)),
              CommonStyle.primary), Icon.refresh, " Sign in")
          )
        )
      } else <.div(redirector(p.router))
    }
  }

  val component = ReactComponentB[Props]("LogIn")
    .initialState(State(UserName("","")))
    .renderBackend[Backend]
//    .componentWillMount(scope => scope.backend.mounted(scope.props) )
    .build

  //def apply(router: RouterCtl[Loc]) = component(router)

  def apply(proxy: ModelProxy[Pot[User]], router: RouterCtl[Loc]) = component(Props(proxy, router))

}
