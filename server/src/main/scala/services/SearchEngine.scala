package services

import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsOnline, TripsServers}
import dgraph._
import rat.shared._

import scala.collection.immutable.TreeMap
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.getquill._
import Impls._


class SearchEngine(db:Tasks) {
  import db.db._
  import DGraph._
  import dgraph.DGraphDSL._

  case class QN(id:String, tpe:String, direction:String, props:List[(String,String)])

  private def createQN(s:DGraph[IndexedSeq[String], String]) = {
    val s1 = s.map(
      nodeMapper = (strs: IndexedSeq[String]) => strs.map(x => {
        val ys = x.split("::")
        if (!x.contains("::")) x -> x
        else if (ys.length > 1) ys(0) -> ys(1)
        else ys(0) -> ys(0)
      }).toList,
      edgeMapper = (str: String) => str)

    val s2 = s1.map(
      nodeMapper = (mp: List[(String, String)]) => {
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

    val edges = s2.nodes.filter(sn => sn._2.value.props.exists(e => e._1 == "edge" && e._2.contains(","))).toList.flatMap(sn => {
      sn._2.value.props.filter(e => e._1 == "edge" && e._2.contains(",")).map { case(ed,vl) =>
        val res = vl.split(",")
        (sn._2.value.id, res(0), res(1))
      }
    })


    val nodes = s2.nodes.filterNot(_._2.value.id == "").map(x => x._2.value.copy(props = x._2.value.props.filterNot(_._1 == "edge")))
      .zipWithIndex.map{case(qn, id) => id -> Node(qn, id)}.toMap
    val fEdges =
      edges.filter(e => nodes.exists(_._2.value.id == e._3)).toList
        .map { case(nid,label,id) =>
          DEdge(label, nodes.find(_._2.value.id == nid).get._1, nodes.find(_._2.value.id == id).get._1)
        }.map(e => (e.from, e.to) -> e)


    DGraph.from(nodes, TreeMap(fEdges:_*))
  }

  def createQuery(s:DGraph[IndexedSeq[String], String]) = {

    val q1 = createQN(s)

    def typeExact(n:QN): Map[String,String] => Boolean = {
      def x(ndm:Map[String,String]):Boolean = {
        val typeCHeck =
          if(n.tpe != "" && ndm.contains("type"))
            if(n.direction == "=")
              ndm("type").toLowerCase == n.tpe.toLowerCase
            else {
              (ndm("type").toLowerCase :: AlternativeManager.ont.^^(ndm("type").toLowerCase)).contains(n.tpe)
            }

          else true
        val rest = n.props.forall{
          case (ky, vl) =>
            if(ndm.contains(ky)) ndm(ky).toLowerCase == vl.toLowerCase
            else true
        }
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
    val resIds =  run(db.golds).map(gr => (gr.id, db.decodeLF(gr.graph).get, gr))
      .filter{case (gid, gold, gr) =>
        gold.graph.filter(q).nonEmpty}.map(gr => {
      val g = gr._3
      TaskInfo(g.id, g.sentence, db.decodeUserStats(g.usersStats), isFinished = true, 1.00,
        db.decodeComments(g.comments), g.domain)
    })
    resIds
  }

//  def replace(s:DGraph[IndexedSeq[String], String], r:DGraph[IndexedSeq[String], String]) = {
//    val q = createQuery(s)
//    val rep = createQN(r)
//    val graphs = run(db.golds).map(gr => (gr.id, db.decodeLF(gr.graph).get, gr))
//
//
//  }

}
