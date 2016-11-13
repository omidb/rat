package rat.shared

case class UserName(user:String, pass:String)

case class User(id:String, name:String, valid:Boolean)

object User {
  val invalidUser = User("invalid", "invalid", valid = false)
}



