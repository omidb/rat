package services

import dgraph.DGraph
import rat.shared._
import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsOnline, TripsServers}
class ApiService  extends Api2{

  override def signIn(userName: UserName):User = {
    val user = UserManager.checkUser(userName)
    println(user.name + " logged in!")
    user
  }

  override def getAnnotationStats(user: User): AnnotationStatistics = {
//    Thread.sleep(1500)
    AnnotationStatistics(5, 100, 10, 100)
  }

  override def calculateAgreement(): Unit = {

  }

  override def getUserTasks(user: User): Option[List[GraphInfo]] = {
    Some(
      List(
        GraphInfo(10, "I am going to go togo to schoool of my choice and after that may be I will eat", UnEdited),
        GraphInfo(235, "I am going to go togo to schoool of my choice and after that may be I will eat", Edited)
      )
    )
  }

  override def getUserGraph(user: User, id: Int): Option[TripsLF] = {
    val onlineParser = new TripsOnline()
    Some(TripsHelper.doc2lf(onlineParser.onlineParse(TripsServers.stepDev, "I have a good school")))
  }

  override def getNodeAlters(value:String): Option[List[NodeAlternative]] = {
    println("HEloo I am here!!")
    Some(
      List(
        NodeAlternative("1", "foo", "bar", List("a1", "a2", "a3", "a4"), isWordNetMapping = false, version = "0"),
        NodeAlternative("2", "foo1", "bar1", List("a11", "a12", "a13", "a14"), isWordNetMapping = false, version = "0"),
        NodeAlternative("3", "foo2", "bar2", List("a21", "a22", "a23", "a24"), isWordNetMapping = false, version = "0")

    ))
  }

  override def getEdgeAlters(value:String): Option[List[EdgeAlternative]] = {
    Some(List(EdgeAlternative("1", "EdgeAlter1"), EdgeAlternative("2", "EdgeAlter2")))
  }

}
