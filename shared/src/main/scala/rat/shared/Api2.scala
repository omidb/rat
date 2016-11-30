package rat.shared

import com.github.omidb.nlp.toolsInterface.{TripsDoc, TripsLF}

trait Api2 {

  def signIn(userName: UserName): User

  def getAnnotationStats(user: User): AnnotationStatistics

  def calculateAgreement(user: User): Unit

  def getUserTasks(user: User): Option[List[TaskInfo]]

  def getAllTasks(user: User): Option[List[TaskInfo]]

  def deleteTask(user:User, taskID:Int): ResultStatus

  def goldTask(user:User, taskID:Int): ResultStatus

  def saveTask(user:User, taskID:Int, lf:TripsLF): ResultStatus

  def getTask(user:User, taskID:Int): Map[User, TripsLF]

  def getUserGraph(user: User, id:Int): Option[TripsLF]

  def getAllGolds(): Option[List[TaskInfo]]

  def getAllDomainGold(): Option[Map[String, List[TaskInfo]]]

  def getGold(taskID:Int): Option[TripsLF]

  def saveGold(user:User, taskID:Int, lf:TripsLF): ResultStatus

  def getNodeAlters(value:String): Option[List[NodeAlternative]]

  def getEdgeAlters(value:String): Option[List[EdgeAlternative]]

  def searchGolds(search:String): Option[List[TaskInfo]]

  def evalGoldID(id:Int):Double

  def getLispForGolds(ids:List[Int]):String

  def rollBack(goldID:Int): ResultStatus




}

object TripsHelper {
  //todo:change this to one graph
  def doc2lf(doc:TripsDoc) = {
    doc.lfs.head
  }
}

