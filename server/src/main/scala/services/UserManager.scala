package services

import rat.shared.{User, UserName}

object UserManager {
  val invalidUser = User("invalid","invalid", "none", false)
  val users = Map(
    "omid" -> User("omid", "Omid", "all",true),
    "james" -> User("james", "James Allen", "all",true),
    "will" -> User("will", "William", "none",true),
    "ian" -> User("ian", "Ian Perera", "none",true),
    "bonnie" -> User("bonnie", "Bonnie", "none",true),
    "archna" -> User("archna", "Archna", "none",true),
    "chohman" -> User("chohman", "Chohman", "none",true),
    "jansen" -> User("jansen", "Jansen", "none",true),
    "lucian" -> User("lucian", "Lucian", "none",true),
    "rik" -> User("rik", "Ritwik", "none",true),
    "jena" -> User("jena", "Jena", "none",true))


  lazy val activeUsers = users.filterNot{case(uid,u) => u.id == "lucian" || u.id == "will" || u.id=="chohman"|| u.id=="ian"|| u.id=="james"}

  def checkUser(userName: UserName):User = {
    val us = users.filter(_._2.id == userName.user && userName.pass == "rochester")
    if(us.nonEmpty) us.head._2 else invalidUser
  }
}
