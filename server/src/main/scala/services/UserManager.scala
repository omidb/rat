package services

import rat.shared.{User, UserName}

object UserManager {
  val invalidUser = User("invalid","invalid", false)
  val users = List(
    User("omid", "Omid Ba",true),
    User("james", "James Allen",true),
    User("will", "William",true),
    User("ian", "Ian Perera",true),
    User("bonnie", "Bonnie",true),
    User("archna", "Archna",true),
    User("chohman", "Chohman",true),
    User("jansen", "Jansen",true),
    User("lucian", "Lucian",true),
    User("rik", "Ritwik",true),
    User("jena", "Jena",true))


  lazy val activeUsers = users.filterNot(u => u.id == "lucian" || u.id == "will" || u.id=="chohman"|| u.id=="ian"|| u.id=="james")

  def checkUser(userName: UserName):User = {
    val us = users.filter(_.id == userName.user && userName.pass == "rochester")
    if(us.nonEmpty) us.head else invalidUser
  }
}
