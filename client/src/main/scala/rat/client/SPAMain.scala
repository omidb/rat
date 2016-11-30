package rat.client

import japgolly.scalajs.react.{Callback, CallbackB, CallbackTo, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import rat.client.components.GlobalStyles
import rat.client.logger._
import rat.client.modules._
import rat.client.services.MainCircuit

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import rx._
import rx.async.Platform._

@JSExport("SPAMain")
object SPAMain extends js.JSApp {

  // Define the locations (pages) used in this application
  sealed trait Loc

  case object DashboardLoc extends Loc
  case object LoginLoc extends Loc
  case object EditorLoc extends Loc
  case object TasksLoc extends Loc
  case object GoldsLoc extends Loc
  case object EvaluationLoc extends Loc


  val loginWrapper = MainCircuit.connect(_.user)
  val dashboardWrapper = MainCircuit.connect(x => x.statistics)
  val editorWrapper = MainCircuit.connect(x => x.editorHelper)
  val tasksWrapper = MainCircuit.connect(x => x.taskHelper)
  val goldsWrapper = MainCircuit.connect(x => x.goldHelper)
  val evaluationWrapper = MainCircuit.connect(x => x.evaluationHelper)

  def getRouter(userValid:CallbackB) = {
    RouterConfigDsl[Loc].buildConfig { dsl =>
      import dsl._

      val privateRouts =
        (staticRoute(root, DashboardLoc) ~> renderR(ctl => dashboardWrapper(proxy => Dashboard(proxy)))
         | staticRoute("#editor", EditorLoc) ~> renderR(ctl => editorWrapper(proxy => Editor(proxy)))
         | staticRoute("#tasks", TasksLoc) ~> renderR(ctl => tasksWrapper(proxy => TaskViewer(proxy)))
          | staticRoute("#golds", GoldsLoc) ~> renderR(ctl => goldsWrapper(proxy => GoldViewer(proxy)))
          | staticRoute("#evaluation", EvaluationLoc) ~> renderR(ctl => evaluationWrapper(proxy => Evaluation(proxy)))
          ).addCondition(userValid)(_ => redirectToPage(LoginLoc)(Redirect.Push))

      (staticRoute("#login", LoginLoc) ~> renderR(ctl => loginWrapper(proxy => Login(proxy, ctl)))
        | privateRouts
        ).notFound(redirectToPage(LoginLoc)(Redirect.Replace))
    }.renderWith(layout)
  }



  // base layout for all pages
  def layout(c: RouterCtl[Loc], r: Resolution[Loc]) = {
    println(c.toString)
    <.div(
      // here we use plain Bootstrap class names as these a specific to the top level layout defined here
      //<.nav(^.className := "navbar navbar-inverse navbar-fixed-top",
//      <.header(<.p("aaaa")),
      <.nav(^.className := "navbar navbar-dark bg-inverse", ^.marginBottom:=10,
          <.a(^.className := "navbar-brand", ^.color:="#fff", "RAT"),
            // connect menu to model, because it needs to update when the number of open todos changes
            MainMenu(c, r.page)

      ),
      // currently active module is shown in this container
      //^.className := "container",
      <.div(r.render())
    )
  }

  @JSExport
  def main(): Unit = {
    log.warn("Application starting")
    // send log messages also to the server
    log.enableServerLogging("/rocnlp/logging")
    log.info("This message goes to server as well")

    // create stylesheet
    GlobalStyles.addToDocument()
    // create the router
    val userValid = MainCircuit.zoom(_.user.exists(_.valid))
    println(s"in the main: ${userValid()}")
    val routerConfigs = getRouter(CallbackTo {userValid()})

    val router = Router(BaseUrl.until_#, routerConfigs)
    // tell React to render the router in the document body
    ReactDOM.render(router(), dom.document.getElementById("root"))
  }
}
