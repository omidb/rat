package rat.shared

case class RPoint(x:Double = 0, y:Double = 0)
case class RLine(p1:RPoint, p2:RPoint)
object RLine {
  def empty = RLine(RPoint(), RPoint())
}
case class RVector(x:Double = 0, y:Double = 0)
case class RRec(topLeft:RPoint, size:RVector){
  def centroid = RPoint(topLeft.x + size.x / 2, topLeft.y + size.y / 2)
}
object RRec{
  def empty = RRec(RPoint(), RVector())
}

//package rat.shared
//
//case class RPoint(x:Double, y:Double)
//case class RLine(p1:RPoint, p2:RPoint)
//case class RVector(x:Double, y:Double)
//case class RRec(toplef:RPoint,size:RVector)
//
//
//case class SAtr(key:String,value:String)
//
//case class VizEdge( labelProp:RRec, points:Array[RPoint],
//                    isSelected:Boolean = false, map:Map[String,String] = Map.empty, comments:Option[String] = None)
//
//object VizEdge {
//
//  def empty: VizEdge = VizEdge(RRec(RPoint(0, 0), RVector(0, 0)), Array.empty, false, Map.empty)
//}
//
//case class VizNode(labelProp:RRec, isSelected:Boolean = false, map:Map[String,String] = Map.empty, comments:Option[String] = None)
//
case class NodeAlternative(id:String, typ:String, word:String, path2root:List[String],
                           isWordNetMapping:Boolean, version:String)

case class EdgeAlternative(id:String, value:String)

//object VizNode{
//   def empty:VizNode = VizNode(RRec(RPoint(0,0),RVector(0,0)), false, Map.empty)
//}
//
//case class SNode(id:String, label:String, tpe:Option[String], alternatives: List[NodeAlternative] = List.empty,
//                 viz:VizNode,isWordnet:Boolean, version:String)
//
//case class SNode1(id:String, label2:String/*, tpe:Option[String], alternatives: List[NodeAlternative],
//                   viz:VizNode,isWordnet:Boolean, version:String*/)
//
//
//case class SEdge(id:String, label:String, from:SNode, to:SNode, viz:VizEdge, alternatives:List[EdgeAlternative] = List.empty)
//
//
sealed trait GraphStatus

case object Edited extends GraphStatus

case object Submitted extends GraphStatus

case object UnEdited extends GraphStatus

case object Impossible extends GraphStatus
//
//case class SGraphProps(id:String, sentence:String, checkLater:Boolean = false, status: GraphStatus = UnEdited,
//                       props:Map[String,String] = Map.empty)
//
//
//case class SGraph(nodes: List[SNode], edges: List[SEdge], properties: SGraphProps)
//
//object SGraph{
//
//  def emptyGraph = {
//    SGraph(List.empty, List.empty, SGraphProps("invalid","invalid", true,UnEdited))
//  }
//
//  def toLisp(g: SGraph) = {
//    val inDegree = g.nodes.map(n => n -> g.edges.filter(_.to.id == n.id).size).toMap
//    val inDegRoots = inDegree.filter(_._2 == 0).map(_._1)
//    var visitedNodes = g.nodes.map(n => n.id -> false).toMap
//    val nodesMap = g.nodes.map(n => n.id -> g.edges.filter(_.from.id == n.id)).toMap
//    val allNodes = g.nodes.map(n => n.id -> n).toMap
//    val roots = if(inDegRoots.size > 0) inDegRoots else g.nodes.filter(_.tpe.get.toLowerCase.startsWith("sa_"))
//    var str = ""
//
//
//
//
//    def generateLisp(nd:SNode):String = {
//      val outEdges = g.edges.filter(_.from.id == nd.id)
//      val outNodes = g.nodes.filter(n => outEdges.exists(_.to.id == n.id))
//      val zips = outEdges.map(e => (e, g.nodes.find(_.id == e.to.id).get))
//      val mp = nd.viz.map.filter(x => x._1 != "word" && x._1 != "type").map{ case(k,v) => " :"+k+" "+v }
//      val wordType = nd.viz.map.filter(x => x._1 == "word" || x._1 == "type")
//        .map{case(k,v) => if(k == "word") "W::"+v else if(k == "type") "ONT::"+v else "" }
//
//      val gr  = if(nd.tpe.isDefined) " ONT::"+nd.tpe.get else ""
//
//      if(nd.tpe.isDefined && nd.tpe.get.startsWith("SA_"))
//        "(" + "ONT::SPEECHACT" +" ONT::"+nd.id+ gr +" "+zips.map{case (e,n) => ":"+e.label + " ONT::" + n.id}.mkString(" ") +mp.mkString(" ") +")\n"
//      else
//      "(" + gr + " ONT::"+nd.id + " (:* " + wordType.mkString(" ") +" )"+
//        zips.map{case (e,n) => ":"+e.label + " ONT::" + n.id}.mkString(" ") + mp.mkString(" ") + ")\n"
//    }
//
//
//    def traverse(node: SNode): Unit = {
//      visitedNodes = visitedNodes.updated(node.id,true)
//      str += generateLisp(node)
//      val edges = nodesMap(node.id)
//      edges.foreach(e => {
//        if(!visitedNodes(e.to.id)){
//          traverse(allNodes(e.to.id))
//        }
//      })
//    }
//
//    roots.toList.map(r => {
//      traverse(r)
//      val fr = "(\n"+ str + "\n)"
//      str = ""
//      fr
//    })
//  }
//
//  def modifyEdge(g: SGraph, oldEdge: SEdge, fromID: String, toID: String) = {
//    val es = g.edges.zipWithIndex.filter(_._1 == oldEdge)
//    val n1L = g.nodes.filter(_.id == fromID)
//    val n2L = g.nodes.filter(_.id == toID)
//
//    if(es.size > 0 && n1L.size > 0 && n2L.size > 0){
//      val enwE = es.head._1
//        .copy(id = SharedUtil.getEdgeID(fromID,toID), from = n1L.head, to = n2L.head)
//      val newG = g.copy(edges = g.edges.updated(es.head._2, enwE))
//      newG
//    } else {
//      println("Didn't find any node or edge")
//      g
//    }
//  }
//
//  def compareGraphs(original:SGraph, newG:SGraph):AnnotInfo = {
//    val deletedNodes = original.nodes.filterNot(n => newG.nodes.map(_.id).contains(n.id)).size
//
//    val deletedEdges = original.edges.filterNot(n => newG.edges.map(_.id).contains(n.id)).size
//
//    val addedEdges = newG.nodes.filterNot(n => original.nodes.map(_.id).contains(n.id)).size
//
//    val CommonNodesNew = newG.nodes.filter(n => original.nodes.map(_.id).contains(n.id))
//
//    val CommonEdgesNew = newG.edges.filter(n => original.edges.map(_.id).contains(n.id))
//
//    val nodeMap = CommonNodesNew.map(nn => nn -> original.nodes.filter(_.id == nn.id).head)
//    val edgeMap = CommonEdgesNew.map(nn => nn -> original.edges.filter(_.id == nn.id).head)
//
//    val senseCount = nodeMap.filter{case(x,y) => x.tpe != y.tpe}.size
//    val roleCount = edgeMap.filter{case(x,y) => x.label!= y.label}.size
//    AnnotInfo("ALl", deletedNodes,deletedEdges,addedEdges,senseCount,roleCount)
//  }
//
//
//  def removeCyclesAndGetConnected(g:SGraph) = {
//    val inDegree = g.nodes.map(n => n -> g.edges.filter(_.to.id == n.id).size).toMap
//    val inDegRoots = inDegree.filter(_._2 == 0).map(_._1)
//    val roots = if(inDegRoots.size > 0) inDegRoots else g.nodes.filter(_.tpe.get.toLowerCase.startsWith("sa_"))
////    println(s"rootsize: ${roots.size}")
//    val dumbRoot = SNode("rooot", "rooot", Some("dmbroot"), List.empty, VizNode.empty,false,"VV")
//    val dumbRootEdges = roots.map(r => SEdge("rr" + r.id, "dumbedge", dumbRoot, r, VizEdge.empty))
//    val newG = SGraph(dumbRoot :: g.nodes, dumbRootEdges.toList ::: g.edges, g.properties)
//
//    var visitedNodes = newG.nodes.map(n => n.id -> false).toMap
//    var visitedEdges = newG.edges.map(n => n.id -> false).toMap
//    val nodesMap = newG.nodes.map(n => n.id -> newG.edges.filter(_.from.id == n.id)).toMap
//    val allNodes = newG.nodes.map(n => n.id -> n).toMap
//    def traverse(node: SNode): Unit = {
//      visitedNodes = visitedNodes.updated(node.id,true)
//      val edges = nodesMap(node.id)
//      edges.foreach(e => {
//        if(!visitedNodes(e.to.id)){
//          visitedEdges = visitedEdges.updated(e.id,true)
//          traverse(allNodes(e.to.id))
//        }
//      })
//    }
//
//    def isLoopBetweenAtoB(a:SNode, b:SNode):Boolean = {
////      println(s"loop from ${a.label+"-"+a.id}  to ${b.label+"-"+b.id}")
//      var vsnds = newG.nodes.map(n => n.id -> false).toMap
//      def getAllPathNodes(nd:SNode, nds:List[SNode]):List[SNode] = {
////        println(s"checking on ${nd.label+"-"+nd.id}")
//        val edges = nodesMap(nd.id)
//        vsnds = vsnds.updated(nd.id, true)
//        val nnds = nd :: nds
//        val edgs =
//          for {
//            e <- edges
//            if(vsnds(e.to.id) == false)
//          }
//          yield {
////            println(s"passing on ${e.to.label+"-"+e.to.id} by ${e.label+"-"+e.id}")
//            getAllPathNodes(allNodes(e.to.id), nnds)
//          }
//        nnds ::: edgs.flatten
//        //nnds ::: edges.map(e => getAllPathNodes(e.to, nnds)).flatten
//      }
//
//      val ab = getAllPathNodes(a,List.empty).toSet
//      vsnds = newG.nodes.map(n => n.id -> false).toMap
//      val ba = getAllPathNodes(b,List.empty).toSet
////      ab.foreach(abi => println(abi.label +"-"+ abi.id))
////      println("____")
////      ba.foreach(bai => println(bai.label+"-"+ bai.id))
////      println("**********")
//      if(ab.size >0 && ba.size > 0)
//        (ab.contains(b) && ba.contains(a))
//      else
//        false
//    }
//    traverse(dumbRoot)
//    val loopyEdges = visitedEdges.filter(_._2 == false).map(q =>newG.edges.filter(_.id == q._1).head -> q._2)
//      .filter(e => isLoopBetweenAtoB(e._1.from, e._1.to))
//    (dumbRoot,newG.copy(edges = newG.edges.filterNot(e => loopyEdges.contains(e))))
//
////      .foreach(e => println(e._1.from.label + "->" + e._1.to.label))
//  }
//
//  def toAMR(g:SGraph):String = {
//
//    if(g.nodes.size == 0) ""
//    else {
//      val (dumbRoot, newG) = removeCyclesAndGetConnected(g)
////      println("last: " + g.nodes.size + ", "  + g.edges.size)
////      println("new: " + newG.nodes.size + ", "  + newG.edges.size)
//      val allNodes = newG.nodes.map(n => n.id -> n).toMap
//      val nodesMap = newG.nodes.map(n => n.id -> newG.edges.filter(_.from.id == n.id)).toMap
//      val alphabet = "abcdefghijklmnopqrstuvwxyz"
//      val nodesIDMap = newG.nodes.sortBy(_.id).zipWithIndex
//        .map {
//        case (n, i) => n.id -> alphabet.charAt(i)
//      }.toMap
//
//      var AMRnodesUsed = nodesIDMap.keys.map(k => k-> false).toMap
//      //println(newG.nodes.map(node => s"${node.tpe.getOrElse("")+node.label}").mkString(" "))
//      def search(node: SNode, tabLevel: Int): String = {
////        println(s"${node.tpe.getOrElse("")+node.label}")
//        val edges = nodesMap(node.id)
////        println(s"edges:${edges.size}")
//        val spacing = if (tabLevel == 0) "" else ("\t   ") * tabLevel
//        if(!AMRnodesUsed(node.id)) {
//          AMRnodesUsed = AMRnodesUsed.updated(node.id, true)
//          val str1 = s"(${nodesIDMap(node.id)} / ${normalizeString(node.tpe.getOrElse("")+node.label)}"
//          val str2 =
//            if (edges.size > 0) {
//              val es = edges
//              val newS =
//                for (e <- es)
//                  yield {
//                    val o1 = "\n" + spacing + s":${normalizeString(e.label)} "
//                    val o2 = search(allNodes(e.to.id), 1 + tabLevel)
//                    o1 + o2
//                  }
//              newS.mkString(" ")
//            }
//            else ""
//          val str3 = ")"
//          //println("now "+ str1 + str2 + str3)
//          return str1 + str2 + str3
//        }
//        else " " + nodesIDMap(node.id)
//      }
//      def normalizeString(s: String) = {
//        s.toLowerCase().replace("-", "").replace("_", "")
//      }
//
//      search(dumbRoot, 0)
//
//    }
//  }
//}
