package rat.shared

import com.github.omidb.nlp.toolsInterface.TripsLF
import dgraph.Node

case class AnnotationTask(id:Int,  users: List[String], agreement: Double, isFinished:Boolean)

case class Comment(user: String, value:String)
case class TaskInfo(id:Int, sentence:String,userStat: Map[String, GraphStatus],
                    isFinished:Boolean, agreement:Double,
                    comments:List[Comment] = List.empty[Comment], domain:String = "unknown")

case class Gold(id:Int, sentence:String, graph:TripsLF)

object LFUtils {

  def diff(lf1:TripsLF, lf2:TripsLF):Double = {

    def fltr(mp:Map[String,String], cnd:(String) => Boolean) = {
      for(
        (ky,vl) <- mp
        if(cnd(ky.toLowerCase))
      ) yield ky.toLowerCase -> vl.toLowerCase
    }
    val nodesMatch =
      for {
        (ni1, n1) <- lf1.graph.nodes
        (ni2, n2) <- lf2.graph.nodes
      } yield {
        ((n1,n2), fltr(n1.value, _ != "id") == fltr(n2.value, _ != "id"))
      }

    val nodesExist = nodesMatch.toList.groupBy(_._1._1).map(x => x._1 -> x._2.exists(_._2))
//    println(nodesExist)
    val nodeScore = nodesExist.count(_._2).toDouble / nodesExist.size
//    println(lf1.graph.nodes.size,lf2.graph.nodes.size)
    val nodesCountScore = 1.0 -
      Math.abs(lf1.graph.nodes.size - lf2.graph.nodes.size).toDouble / (lf1.graph.nodes.size + lf2.graph.nodes.size)

    val edgesMatch =
      for {
        (ei1, e1) <- lf1.graph.edges
        (ei2, e2) <- lf2.graph.edges
      } yield
        ((e1,e2),
          e1.value.toLowerCase == e2.value.toLowerCase &&
            fltr(lf1.graph.nodes(e1.from).value, _ != "id")  ==
              fltr(lf2.graph.nodes(e2.from).value, _ != "id") &&
            fltr(lf1.graph.nodes(e1.to).value, _ != "id") ==
              fltr(lf2.graph.nodes(e2.to).value, _ != "id")
        )

    val edgesExist = edgesMatch.toList.groupBy(_._1._1).map(x => x._1 -> x._2.exists(_._2))
    val edgeScore = edgesExist.count(_._2).toDouble / edgesExist.size
    val edgesCountScore = 1.0 -
      Math.abs(lf1.graph.edges.size - lf2.graph.edges.size).toDouble / (lf1.graph.edges.size + lf2.graph.edges.size)

    println(nodeScore, nodesCountScore, edgeScore, edgesCountScore)
    (nodeScore + nodesCountScore + edgeScore + edgesCountScore) / 4
  }

  def toLisp(lf:TripsLF):String = {
    def nodeStr(nd:Node[Map[String, String]]) = {
      val hasType = nd.value.contains("type")
      val wordType = nd.value.filter(x => x._1 == "word" || x._1 == "type")
        .map{case(k,v) => if(k == "word") "W::"+v else if(k == "type") "ONT::"+v else "" }
      val gr  = if(hasType) " ONT::"+nd.value("type") else ""
      val mp = nd.value.filter(x => x._1 != "word" && x._1 != "type").map{ case(k,v) => s" :$k $v" }
      val zips = lf.graph.outMap(nd.id).map(x => (lf.graph.edges((nd.id,x)).value, x))

        if(nd.value.contains("type") && nd.value("type").startsWith("SA_"))
          s"( ONT::SPEECHACT ONT::${nd.id}$gr ${zips.map{case (e,n) => ":"+e + " ONT::" + n}.mkString(" ")} ${mp.mkString(" ")} )"
        else
          s"($gr ONT::${nd.id} (:* ${wordType.mkString(" ")}) ${zips.map{case (e,n) => ":"+e+ " ONT::" + n}.mkString(" ")} ${mp.mkString(" ")} )"
    }
    println("creating lisp")
    val res = s"(\n${lf.graph.nodes.map(n => nodeStr(n._2)).mkString("\n")}\n)"
    println(res)
    res
  }


}


