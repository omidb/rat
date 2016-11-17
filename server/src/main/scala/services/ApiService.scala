package services

import rat.shared._
import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsOnline, TripsServers}
class ApiService  extends Api2{

  override def signIn(userName: UserName):User = {
    val user = UserManager.checkUser(userName)
    println(user.name + " logged in!")
    user
  }

  override def getAnnotationStats(user: User): AnnotationStatistics = {
    AnnotationStatistics(5, 100, 10, 100)
  }

  override def calculateAgreement(): Unit = {

  }

  override def getUserTasks(user: User): Option[List[TaskInfo]] = {
    println(user)
    Some(DB.db.tasks.filter(_.userStat.exists(_._1.id == user.id)))
  }

  override def getUserGraph(user: User, id: Int): Option[TripsLF] = {
    DB.db.graphs.find(x => x._1._1.id == user.id && x._1._2 == id).map(_._2)
  }

  override def getNodeAlters(value:String): Option[List[NodeAlternative]] = {
    Some(AlternativeManager.getAllSenses(value))
  }

  override def getEdgeAlters(value:String): Option[List[EdgeAlternative]] = {
    Some(List(EdgeAlternative("1", "EdgeAlter1"), EdgeAlternative("2", "EdgeAlter2")))
  }


  override def getAllTasks(user: User): Option[List[TaskInfo]] = {
    Some(DB.db.tasks)
  }

  override def deleteTask(user:User, taskID:Int): ResultStatus = {
    DB.db.copy(tasks = DB.db.tasks.filterNot(_.id == taskID))
    SuccessResult
  }

  override def goldTask(user:User, taskID:Int): ResultStatus = {
    DB.db.copy(
      golds = Gold(taskID, DB.db.tasks.find(_.id == taskID).get.sentence, DB.db.graphs.head._2) :: DB.db.golds,
      tasks = DB.db.tasks.filterNot(_.id == taskID),
      graphs = DB.db.graphs.filterNot(_._1._2 == taskID)
    )
    SuccessResult
  }

  override def submitTask(user:User, taskID:Int): ResultStatus = {
    SuccessResult
  }

  override def getTask(user:User, taskID:Int): Map[User, TripsLF] =  {
    DB.db.graphs.filter(_._1._2 == taskID).map(x => x._1._1 -> x._2)
  }

}
