package services

import com.github.omidb.nlp.toolsInterface.TripsLF
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.getquill._
import dgraph._
import rat.shared.{Comment, GraphStatus, TaskInfo, User}

import scala.collection.immutable.TreeMap

object Impls {
  implicit val encodeKeyTupleString: KeyEncoder[(Int, Int)] = new KeyEncoder[(Int, Int)] {
    final def apply(key: (Int, Int)): String = key._1.toString + "," + key._2.toString
  }

  implicit val decodeKeyTupleString: KeyDecoder[(Int, Int)] = new KeyDecoder[(Int, Int)] {
    final def apply(key: String): Option[(Int, Int)] = {
      val pairs = key.split(",")
      Some((pairs(0).toInt, pairs(1).toInt))
    }
  }
}

case class GraphRow(id:Int, user:String, graph:String)
case class TaskRow(id:Int, sentence:String, domain:String, usersStats:String, score:Double, isFinished:Int, comments:String)
case class GoldRow(id:Int, sentence:String, domain:String, usersStats:String, graph:String, comments:String)
object TaskRow{
  import Impls._
  def toTaskInfo(tr:TaskRow):TaskInfo = {
    TaskInfo(
      tr.id,
      tr.sentence,
      decode[List[(String,GraphStatus)]](tr.usersStats).fold(er => List.empty, mp => mp).toMap,
      tr.isFinished == 1,
      tr.score,
      decode[List[Comment]](tr.comments).fold(er => List.empty, mp => mp),
      tr.domain
    )
  }

  def fromTaskInfo(ti:TaskInfo):TaskRow = {
    TaskRow(
      ti.id,
      ti.sentence,
      ti.domain,
      ti.userStat.toList.asJson.noSpaces,
      ti.agreement,
      if(ti.isFinished) 1 else 0,
      ti.comments.asJson.noSpaces
    )
  }
}


import db.DbContext

class Tasks(val db: DbContext) {
  import db._
  import Impls._


  def decodeComments(json:String) = decode[List[Comment]](json).fold(er => List.empty, cmnts => cmnts)

  def decodeLF(json:String) = {
    decode[TripsLF](json).fold(er =>  None, lf => Some(lf))
  }

  def decodeUserStats(json:String) = decode[List[(String, GraphStatus)]](json).fold(er => List.empty, fb => fb).toMap



  val tasks = quote(query[TaskRow].schema(_.entity("Tasks")))
  val graphs = quote(query[GraphRow].schema(_.entity("graphs")))
  val golds = quote(query[GoldRow].schema(_.entity("golds")))

  def insertTask(taskInfo: TaskInfo) = run(tasks.insert(lift(TaskRow.fromTaskInfo(taskInfo))))

  def insertGraph(id:Int, user:User, graph:TripsLF) = {
    val grStr = graph.asJson.noSpaces
    run(graphs.insert(lift(GraphRow(id, user.id, grStr))))
  }

  def insertGold(goldInfo:TaskInfo, lf:TripsLF) = {
    val grStr = lf.asJson.noSpaces
    val usrStatStr = goldInfo.userStat.toList.asJson.noSpaces
    val commentsStr = goldInfo.comments.asJson.noSpaces
    run(golds.insert(lift(GoldRow(goldInfo.id, goldInfo.sentence, goldInfo.domain, usrStatStr, grStr, commentsStr))))
  }

  case class DGraphOld[N,E] (nodes:Map[Int, dgraph.Node[N]], edges:TreeMap[(Int,Int), dgraph.DEdge[E]],
                          inMap:Map[Int,IndexedSeq[Int]], outMap:Map[Int,IndexedSeq[Int]])

  case class TripsLFOld(graph:DGraphOld[Map[String,String],String], rootNode:Option[Int])

  def change(str:String) = {
    val old = decode[TripsLFOld](str).fold(er => {
//      println(er)
      TripsLFOld(DGraphOld(Map.empty, TreeMap.empty, Map.empty, Map.empty), None)
    }, fb => fb)
//    println(old)
    val dg = TripsLF(DGraph(old.graph.nodes, old.graph.edges, old.graph.inMap, old.graph.outMap, DGraph.warshall(old.graph.nodes, old.graph.edges)), old.rootNode)
    dg.asJson.noSpaces
  }
  def repair() = {
    val news = run(graphs).toList.map(g => {
//      println(g.graph)
      g.copy(graph = change(g.graph))
    })
    val glds = run(golds).toList.map(g => {
//      println(g.graph)
      g.copy(graph = change(g.graph))
    })
    run(graphs.delete)
    run(golds.delete)
    for(n <- news) {
      run(graphs.insert(lift(n)))
    }

    for(n <- glds) {
      run(golds.insert(lift(n)))
    }

  }

}
