package rat.shared

case class UserName(user:String, pass:String)

case class User(id:String, name:String, access:String, valid:Boolean)

object User {
  val invalidUser = User("invalid", "invalid", "none", valid = false)
}



