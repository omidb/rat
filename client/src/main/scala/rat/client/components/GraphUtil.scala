package rat.client.components

//import Dagre._
import com.github.omidb.nlp.toolsInterface.TripsLF
import dgraph.{DEdge, DGraph, Node}
import japgolly.scalajs.react.{Callback, _}

import scala.collection.immutable.TreeMap
import scala.scalajs.js
import rat.shared._

case class NodeViz(value:Map[String,String], topString:String = "-", downString:String = "-", rect:RRec = RRec.empty,
                   isSelected:Boolean = false, fake:Boolean = false)

case class EdgeViz(value:String,
                   label:String = "-", labelLoc:RRec = RRec.empty, points:List[RPoint] = List.empty[RPoint],
                   color:String = "black", isSelected:Boolean = false, startOn:Boolean = false, endOn:Boolean = false,
                   fake:Boolean = false)
object EdgeViz {
  def empty = EdgeViz("content", "content")
}

case class TripsLFViz(rootNode:Option[Int], graphViz:DGraph[NodeViz,EdgeViz])


object GraphUtil {

  type EdgeCallback = ((Int,Int)) => Callback
  def emptyEdgeCallback(x: (Int, Int)) = Callback.empty

  type NodeCallback = (Int) => Callback
  def emptyNodeCallback(id: Int) = Callback.empty

  val testG = DGraph.from[Map[String,String], String](
    Map(
      0 -> Node(Map("type" -> "speechact", "value" -> "S0"), 0),
      1 -> Node(Map("type" -> "hunman", "value" -> "S1"), 1),
      2 -> Node(Map("type" -> "goeat", "value" -> "S2"), 2),
      3 -> Node(Map("type" -> "blah", "value" -> "S3"), 3),
      4 -> Node(Map("type" -> "blah blah", "value" -> "S4"), 4)
    ),
    TreeMap(
      (0, 1) -> DEdge("0-1", 0, 1),
      (0, 2) -> DEdge("0-2", 0, 2),
      (2, 1) -> DEdge("2-1", 2, 1),
      (1, 2) -> DEdge("1-2", 1, 2),
      (2, 3) -> DEdge("2-3", 2, 3),
      (2, 4) -> DEdge("2-4", 2, 4)
    )
  )

  def move(p: Array[Double]) = s"translate(${p(0)},${p(1)})"

  def tripsLF2tripsLFViz(lfo: TripsLF): TripsLFViz = {
    val extensibleNodes = lfo.graph.nodes
      .filter(_._2.value.keys.exists(str => str.toLowerCase == "scale" || str.toLowerCase == "unit"))
    var gr = lfo.graph.copy(nodes = lfo.graph.nodes.map(n => n._1 -> n._2.copy(value = n._2.value.filterNot(str => str._1.toLowerCase == "unit" || str._1.toLowerCase == "scale"))))

    for((enId, en) <- extensibleNodes) {
      if (en.value.keys.exists(str => str.toLowerCase == "scale")) {
        val scale = en.value.filter(_._1.toLowerCase == "scale").head._2
        val (newNode, newgr) = gr.addNode(Map("type" -> scale, "scale" -> scale, "isFake" -> ""), "scale", enId)
        gr = newgr
      }

      if (en.value.keys.exists(str => str.toLowerCase == "unit")) {
        val unit = en.value.filter(_._1.toLowerCase == "unit").head._2
        gr = gr.addNode(Map("type" -> unit, "unit" -> unit, "isFake" -> ""), "unit", enId)._2
      }
    }

    val lf = lfo.copy(graph = gr)
    val lfsViz =
      TripsLFViz(lf.rootNode,
        lf.graph.map(
          (nId, n) => {
            val (topStr,downStr) = createNodeText(n)
            NodeViz(n, topStr, downStr, RRec(RPoint(), RVector()), fake = n.contains("isFake"))
          },
          (eId, e) => EdgeViz(e, e)
        ))
    lfsViz
  }

  def tripsLFViz2tripsLF(lfvizo: TripsLFViz): TripsLF = {

    val fakeNodes = lfvizo.graphViz.nodes.filter(_._2.value.fake)
    var gr = lfvizo.graphViz
    for ((fId, fn) <- fakeNodes) {
      val ins = gr.inMap(fId)
      val edges = ins.map(i => gr.edges.filter(_._1 == (i ,fId)).head)
      for((eID,e) <- edges){
        if(e.value.value == "scale")
          gr = gr.copy(
            nodes = gr.nodes.updated(eID._1,
              gr.nodes(eID._1).copy(value =
                gr.nodes(eID._1).value.copy(value =
                  gr.nodes(eID._1).value.value.updated("scale", fn.value.value("type"))))))

        if(e.value.value == "unit")
          gr = gr.copy(
            nodes = gr.nodes.updated(eID._1,
              gr.nodes(eID._1).copy(value =
                gr.nodes(eID._1).value.copy(value =
                  gr.nodes(eID._1).value.value.updated("unit", fn.value.value("type"))))))
      }
      edges.foreach(e => gr = gr.removeEdge(e._1._1, e._1._2))
    }
    fakeNodes.foreach(n => gr = gr.removeNode(n._1))
    val lfviz = lfvizo.copy(lfvizo.rootNode, gr)
    val lf =
      TripsLF(
        rootNode = lfviz.rootNode,
        graph = lfviz.graphViz.map(
            (nId, n) => {
              n.value
            },
            (eId, e) => e.value
          )
      )
    lf
  }

  def textBoundingBox(p: Array[String]) = {
    RVector(p.map(_.length).max * 12 + p.maxBy(_.length).count(x => x == ' ') * 25, p.length * 20)
  }

  def createNodeText(vl:Map[String, String]) = {
    val lower = vl.find(_._1 == "word").map(_._2).getOrElse("-")
    val upper = vl.find(_._1 == "type").map(_._2).getOrElse("-")
    (upper, lower)
  }

  def createDagreGraph(lf: TripsLFViz): Graph = {
    val g = js.Dynamic
      .newInstance(Dagre.dagre.graphlib.Graph.asInstanceOf[js.Dynamic])()
      .asInstanceOf[Graph]

    g.setGraph(js.Dynamic.literal())
    g.setDefaultEdgeLabel(() => js.Dynamic.literal())

    lf.graphViz.nodes.foreach { case (nid, n) =>
      val nSize = textBoundingBox(Array(n.value.topString, n.value.downString))
      g.setNode(nid.toString, GraphNode(nid.toString, nSize.x, nSize.y))
    }

    lf.graphViz.edges.foreach { case ((from, to), e) =>
      g.setEdge(from.toString, to.toString, GraphNode(e.value.label))
    }
    g

  }

  def layoutGraph(lf: TripsLFViz, g: Graph, offset: RVector = RVector(0, 0)):TripsLFViz = {
    Dagre.dagre.layout(g)
    lf.copy(graphViz =
      lf.graphViz.map(
        (nid, n) => {
          val nd = g.node(nid.toString)
          n.copy(
            rect = RRec(RPoint((nd.x + offset.x) - nd.width/2, (nd.y + offset.y) - nd.height/2),
              RVector(nd.width, nd.height))
          )
        },
        (eId, e) => {
          val ed = g.edge(GraphEdge(eId._1.toString, eId._2.toString))
          e.copy(labelLoc = RRec(RPoint(ed.x.get + offset.x, ed.y.get + offset.y), textBoundingBox(Array(e.label))),
            points = ed.points.map(p => RPoint(p.x + offset.x, p.y + offset.y)).toList,
            startOn = false, endOn = false
          )
        }
      )
    )
  }

  def layoutGraph(lfv:TripsLFViz):TripsLFViz = {
    val lfv2 =
      lfv.copy(
        graphViz = lfv.graphViz.copy(
          nodes =
            lfv.graphViz.nodes.map(x => {
              val (up, low) = createNodeText(x._2.value.value)
              (x._1, x._2.copy(value = x._2.value.copy(topString = up, downString = low)))
            }),
          edges =
            lfv.graphViz.edges.map(x => x._1 -> x._2.copy(value = x._2.value.copy(label = x._2.value.value)))
        )
      )
    layoutGraph(lfv2, createDagreGraph(lfv2))
  }

  def layoutGraph(lf:TripsLF):TripsLFViz = {
    layoutGraph(tripsLF2tripsLFViz(lf))
  }


}
