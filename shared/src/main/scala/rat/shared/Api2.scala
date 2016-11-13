package rat.shared

import com.github.omidb.nlp.toolsInterface.{TripsDoc, TripsLF}

trait Api2 {

  def signIn(userName: UserName): User

  def getAnnotationStats(user: User): AnnotationStatistics

  def calculateAgreement(): Unit

  def getUserTasks(user: User): Option[List[GraphInfo]]

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

