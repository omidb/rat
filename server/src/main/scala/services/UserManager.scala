package services

import rat.shared.{User, UserName}

object UserManager {
  val invalidUser = User("invalid","invalid", "none", false)
  val users = List(
    User("omid", "Omid", "all",true),
    User("james", "James Allen", "all",true),
    User("will", "William", "none",true),
    User("ian", "Ian Perera", "none",true),
    User("bonnie", "Bonnie", "none",true),
    User("archna", "Archna", "none",true),
    User("chohman", "Chohman", "none",true),
    User("jansen", "Jansen", "none",true),
    User("lucian", "Lucian", "none",true),
    User("rik", "Ritwik", "none",true),
    User("jena", "Jena", "none",true))


  lazy val activeUsers = users.filterNot(u => u.id == "lucian" || u.id == "will" || u.id=="chohman"|| u.id=="ian"|| u.id=="james")

  def checkUser(userName: UserName):User = {
    val us = users.filter(_.id == userName.user && userName.pass == "rochester")
    if(us.nonEmpty) us.head else invalidUser
  }
}
