package rat.shared

import com.github.omidb.nlp.toolsInterface.{TripsDoc, TripsLF}
import dgraph.{DEdge, DGraph, Node}

trait Api2 {

  def signIn(userName: UserName): User

  def getAnnotationStats(user: User): AnnotationStatistics

  def calculateAgreement(user: User): Unit

  def getUserTasks(user: User): Option[List[TaskInfo]]

  def getAllTasks(): Option[List[TaskInfo]]

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

  def evalGoldID(id:Int, parser:String):Double

  def getLispForGolds(ids:List[Int]):String

  def rollBack(goldID:Int): ResultStatus

  def parseForUsers(parser:String, domain:String, users:List[String], lines:List[String]):Map[String, Int]

  def resetGraph(user:String, id:Int): Unit





}

object TripsHelper {
  //todo:change this to one graph
  def doc2lf(doc:TripsDoc) = {

    if(doc.lfs.size > 1){
      var g = DGraph.empty[Map[String,String], String]()
      g =  g.addNode(Node(Map("type" -> "DUMB_ROOT"),0))._2

      for(t <- doc.lfs) {
        val newNodeMapings = t.graph.nodes.values.toList.map(n => {
          val (newN, newG) = g.addNode(n.value)
          g = newG
          (n.id, newN.id)
        }).toMap
        t.graph.edges.values.toList.foreach(e => {
          g = g.addEdge(DEdge(e.value, newNodeMapings(e.from), newNodeMapings(e.to))).get._2
        })
//        println(s"--------${t.graph.nodes.size}---------")
//        println(t.rootNode)
//        println(newNodeMapings)
        if(t.graph.nodes.nonEmpty)
          g = g.addEdge(DEdge("DUMB_EDGE", 0, newNodeMapings(t.rootNode.get))).get._2
      }
      TripsLF(g, Some(0))
    }
    else doc.lfs.head
  }
}

