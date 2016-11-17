package rat.shared

import com.github.omidb.nlp.toolsInterface.{TripsDoc, TripsLF}

trait Api2 {

  def signIn(userName: UserName): User

  def getAnnotationStats(user: User): AnnotationStatistics

  def calculateAgreement(): Unit

  def getUserTasks(user: User): Option[List[TaskInfo]]

  def getAllTasks(user: User): Option[List[TaskInfo]]

  def deleteTask(user:User, taskID:Int): ResultStatus

  def goldTask(user:User, taskID:Int): ResultStatus

  def submitTask(user:User, taskID:Int): ResultStatus

  def getTask(user:User, taskID:Int): Map[User, TripsLF]

  def getUserGraph(user: User, id:Int): Option[TripsLF]

  def getNodeAlters(value:String): Option[List[NodeAlternative]]

  def getEdgeAlters(value:String): Option[List[EdgeAlternative]]




}

object TripsHelper {
  //todo:change this to one graph
  def doc2lf(doc:TripsDoc) = {
    doc.lfs.head
  }
}

