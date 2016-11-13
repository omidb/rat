//package rat.client.services
//
//import autowire._
//import boopickle.Default._
//import rat.client.ukko.Actor
//import rat.shared.{User, UserName, Api2}
//import rx._
//import rat.client.ukko._
//import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
//
//case class UpdateUser(user: User)
//
//trait MainHub extends Actor{
//  private val theUser = Var(User("invalid", "invalid", valid = false))
//  override val name = "MainHub"
//
//  private def updateUser(newUser: User): Unit = {
//    println(s"let's update the user $newUser")
//    println(s"last user ${theUser.now}")
//    if (newUser != theUser.now) {
//      theUser() = newUser
//      println(s"last user ${theUser.now}")
//    }
//  }
//
//  override def receive = {
//    case UpdateUser(usr) => updateUser(usr)
//    case _ =>
//  }
//  def user:Rx[User] = theUser
//
//}
//
//
//
//object MainHub extends MainHub{
//  MainDispatcher.register(this)
//}
//
//object UserAction {
//  def signIn(userName: UserName) = {
//    println(userName)
//    AjaxClient[Api2].signIn(userName).call().foreach { users =>
//      MainDispatcher.dispatch(UpdateUser(users))
//    }
//  }
//}
