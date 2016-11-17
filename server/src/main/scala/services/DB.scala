package services

import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsOnline, TripsServers}
import dgraph._
import rat.shared._

import scala.collection.immutable.TreeMap

case class DB(tasks:List[TaskInfo], graphs:Map[(User, Int), TripsLF], goldsInfo:List[GoldInfo], golds:List[Gold])

object DB {

  def init() = {
    val tasks = List(
      TaskInfo(0, "I have a book",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty),
      TaskInfo(1, "He killed the man",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty),
      TaskInfo(2, "He ate the burger",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty),
      TaskInfo(3, "I am not happy",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty)
    )

    val onlineParser = new TripsOnline()
    val graphs = tasks.flatMap(t =>
      t.userStat.map(u =>
        (u._1, t.id) -> TripsHelper.doc2lf(onlineParser.onlineParse(TripsServers.stepDev, t.sentence)))
    ).toMap

    val goldInfos = List(
      GoldInfo(10, "I have a good dog",
        Map(
          UserManager.users.find(_.id == "james").get -> Submitted,
          UserManager.users.find(_.id == "omid").get -> Submitted
        )),
      GoldInfo(11, "I have a good cat",
        Map(
          UserManager.users.find(_.id == "james").get -> Submitted,
          UserManager.users.find(_.id == "omid").get -> Submitted
        )),
      GoldInfo(12, "He is eating an apple",
        Map(
          UserManager.users.find(_.id == "james").get -> Submitted,
          UserManager.users.find(_.id == "omid").get -> Submitted
        )),
      GoldInfo(13, "He wants to have a car",
        Map(
          UserManager.users.find(_.id == "james").get -> Submitted,
          UserManager.users.find(_.id == "omid").get -> Submitted
        )),
      GoldInfo(14, "I sold my computer",
        Map(
          UserManager.users.find(_.id == "james").get -> Submitted,
          UserManager.users.find(_.id == "omid").get -> Submitted
        )),
      GoldInfo(15, "She kicked the ball",
        Map(
          UserManager.users.find(_.id == "james").get -> Submitted,
          UserManager.users.find(_.id == "omid").get -> Submitted
        ))
    )

    val golds = goldInfos.map(g =>
      Gold(g.id, g.sentence, TripsHelper.doc2lf(onlineParser.onlineParse(TripsServers.stepDev, g.sentence))))
    db = DB(tasks, graphs, goldInfos, golds)
  }

  var db = DB(List.empty, Map.empty, List.empty, List.empty)

  import DGraph._
  import dgraph.DGraphDSL._

  case class QN(id:String, tpe:String, direction:String, props:List[(String,String)])
  def createQuery(s:DGraph[IndexedSeq[String], String]) = {
    val s1 = s.map(
      nodeMapper = (strs: IndexedSeq[String]) => strs.map(x => {
        val ys = x.split("::")
//        println(ys.toList)
        if (!x.contains("::")) x -> x
        else if (ys.length > 1) ys(0) -> ys(1)
        else ys(0) -> ys(0)
      }).toMap,
      edgeMapper = (str: String) => str)

    val s2 = s1.map(
      nodeMapper = (mp: Map[String, String]) => {
        var qn = QN("" ,"", direction = "=", List.empty)
        for ((ky, vl) <- mp) yield {
          if (ky == "type")
            qn = qn.copy(tpe = vl.substring(1), direction = vl(0).toString)
          else if (ky == "id")
            qn = qn.copy(id = vl)
          else
            qn = qn.copy(props = (ky, vl) :: qn.props)

          qn.copy()
        }
        qn
      },
      edgeMapper = (str: String) => str)

    val edges = s2.nodes.filter(sn => sn._2.value.props.exists(e => e._1 == "edge" && e._2.contains(","))).flatMap(sn => {
      sn._2.value.props.filter(e => e._1 == "edge" && e._2.contains(",")).map { case(ed,vl) =>
        val res = vl.split(",")
        (sn._2.value.id, res(0), res(1))
    }
    }).toList

//    edges.foreach(println(_))

    val nodes = s2.nodes.filterNot(_._2.value.id == "").map(x => x._2.value.copy(props = x._2.value.props.filterNot(_._1 == "edge")))
      .zipWithIndex.map{case(qn, id) => id -> Node(qn, id)}.toMap
    val fEdges =
      edges.filter(e => nodes.exists(_._2.value.id == e._3))
      .map { case(nid,label,id) =>
        DEdge(label, nodes.find(_._2.value.id == nid).get._1, nodes.find(_._2.value.id == id).get._1)
      }.map(e => (e.from, e.to) -> e).toList

    val q1 = DGraph.from(nodes, TreeMap(fEdges:_*))
//    println(q1)

    def typeExact(n:QN): Map[String,String] => Boolean = {
      def x(ndm:Map[String,String]):Boolean = {
//        println("**************")
//        println(n)
//        println(ndm)
        val typeCHeck =
          if(n.tpe != "" && ndm.contains("type"))
            if(n.direction == "=")
              ndm("type").toLowerCase == n.tpe.toLowerCase
            else {
//              println(ndm("type").toLowerCase :: AlternativeManager.ont.^^(ndm("type").toLowerCase))
//              println(n.tpe)
//              println("888888888")
              (ndm("type").toLowerCase :: AlternativeManager.ont.^^(ndm("type").toLowerCase)).contains(n.tpe)
            }

          else true
        val rest = n.props.forall{
          case (ky, vl) =>
            if(ndm.contains(ky)) ndm(ky).toLowerCase == vl.toLowerCase
            else true
        }
//        println(rest, typeCHeck)
        rest && typeCHeck
      }
      x
    }

    def edgeMatcher(e:String): String => Boolean = {
      def x(s:String):Boolean = {
        if(e == "*") true
        else e.toLowerCase == s.toLowerCase
      }
      x
    }

    val q2:DGraph[NodeMatchLike[Map[String, String]], EdgeMatchLike[String]] = q1.map(
      nodeMapper = (n:QN) => {
        NodeMatchAND(typeExact(n))
      },
      edgeMapper = (label:String) => EdgeMatch(edgeMatcher(label))
    )
    q2

  }

  def search(s:DGraph[IndexedSeq[String], String]) = {
    val q = createQuery(s)
    val resIds = db.golds.filter(gold => gold.graph.graph.filter(q).nonEmpty).map(_.id)
//    println(resIds)
    db.goldsInfo.filter(inf => resIds.contains(inf.id))
  }

}
