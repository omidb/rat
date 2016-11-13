package rat.client.components

import com.github.omidb.nlp.toolsInterface.TripsLF
import japgolly.scalajs.react.vdom.svg.prefix_<^._
import japgolly.scalajs.react.vdom.Attrs._
import japgolly.scalajs.react.vdom.SvgAttrs
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo, ReactComponentB, vdom, ReactMouseEventI}
import paths.mid.Bezier
import rat.shared._
import GraphUtil.{EdgeCallback,NodeCallback, emptyEdgeCallback, emptyNodeCallback}


object GraphComponent {

  private def nmuEmpty(id:Int): (ReactMouseEventI) => Callback = {
    def x(e:ReactMouseEventI) = {
      Callback.empty
    }
    x
  }

  case class Props(lf: TripsLFViz,
                   gMouseUp: Callback = Callback.empty,
                   nodeMouseUp: (Int) => (ReactMouseEventI) => Callback = nmuEmpty,
                   nodeMouseDown: NodeCallback = emptyNodeCallback,

                   edgeClick: EdgeCallback = emptyEdgeCallback,
                   edgeStartOut: EdgeCallback = emptyEdgeCallback, edgeEndOut: EdgeCallback = emptyEdgeCallback,
                   edgeStartDown: EdgeCallback = emptyEdgeCallback, edgeEndDown: EdgeCallback = emptyEdgeCallback,
                   edgeStartOver: EdgeCallback = emptyEdgeCallback, edgeEndOver: EdgeCallback = emptyEdgeCallback)

  class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props) = {
      <.g(
        onMouseUp --> p.gMouseUp,
        p.lf.graphViz.edges.map(e => drawEdge(p, e._2.value, e._1)),
        p.lf.graphViz.nodes.map(n => drawNode(p, n._2.value, n._1))

      )
    }

    def drawNode(p: Props, node: NodeViz, id:Int) = {
      val color = if (node.isSelected) "gray" else "gray"
      val filled = if (node.isSelected) "red" else "white"
      val topLeft = node.rect.topLeft
      val size = node.rect.size

      <.g(key := s"node$id",
        onMouseUp ==> p.nodeMouseUp(id),
        <.rect(^.y := topLeft.y, ^.x := topLeft.x,
          ^.width := size.x, ^.height := size.y,
          ^.rx := 5, ^.ry := 5, ^.stroke := color, ^.fill := filled, ^.fillOpacity := 0.5, ^.strokeWidth := 3),

        <.line(^.y1 := topLeft.y + size.y / 2, ^.x1 := topLeft.x,
          ^.y2 := topLeft.y + size.y / 2, ^.x2 := topLeft.x + size.x,
          ^.stroke := color, ^.fill := filled, ^.fillOpacity := 0.5, ^.strokeWidth := 3),

        <.text(
          className := "noselect",
          ^.x := node.rect.centroid.x,
          ^.y := node.rect.centroid.y - 5,
          ^.textAnchor := "middle",
          node.topString),
        <.text(
          className := "noselect",
          ^.x := node.rect.centroid.x,
          ^.y := node.rect.centroid.y + 15,
          ^.textAnchor := "middle",
          node.downString)
      )
    }


    def drawEdge(p: Props, edge: EdgeViz, id:(Int, Int)) = {

      def makeMorePoints(points: List[RPoint]): List[RPoint] = {
        val intermediate = points.sliding(2).flatMap(x => Array(
          RPoint((x(0).x + x(1).x) / 2, (x(0).y + x(1).y) / 2),
          x(1))
        ).toList
        points.head +: intermediate
      }

      def normalizeVec(vector: RVector) = {
        val norm = Math.sqrt(vector.x * vector.x + vector.y * vector.y)
        RVector(vector.x / norm, vector.y / norm)
      }

      def bezierPath(points: List[RPoint], color: String, id:(Int,Int)) = {
        val ps = points.map(p => (p.x, p.y))
        val bez = Bezier(ps).path
        <.g(key := s"bezier$id",
          <.defs(
            <.marker(^.id := s"head${id._1},${id._2}", vdom.ReactAttr("orient") := "auto",
              ^.markerWidth := 2, ^.markerHeight := 4, vdom.ReactAttr("refX") := 2.2, vdom.ReactAttr("refY") := 2,
              <.path(^.d := "M0,0 V4 L2,2 Z", ^.fill := color)
            )
          ),
          <.path(^.d := bez.print, ^.stroke := color, ^.fill := "none", ^.strokeWidth := 3,
            ^.markerEnd := s"url(#head${id._1},${id._2})")
        )
      }
      val lastVector = RVector(edge.points.last.x - edge.points(edge.points.size - 2).x,
        edge.points.last.y - edge.points(edge.points.size - 2).y)

      val lastVectorNorm = normalizeVec(lastVector)

      val firstVector = RVector(edge.points(1).x - edge.points(0).x,
        edge.points(1).y - edge.points(0).y)

      val firstVectorNorm = normalizeVec(firstVector)
      val edgeColor = if(edge.isSelected) "red" else edge.color
      <.g(key := s"edge${id.toString}",
        onClick --> p.edgeClick(id),
        bezierPath(makeMorePoints(edge.points), edgeColor, id),
        <.text(
          className := "noselect",
          ^.transform := GraphUtil.move(Array(edge.labelLoc.topLeft.x, edge.labelLoc.topLeft.y)),
          ^.textAnchor := "middle",
          edge.label, ^.fill := edgeColor),


        <.circle(
          onMouseOver --> p.edgeStartOver(id),
          onMouseOut --> p.edgeStartOut(id),
          onMouseDown --> p.edgeStartDown(id),
          ^.stroke := "purple", ^.fill := "blue", if (edge.startOn) ^.opacity := 0.7 else ^.opacity := 0.01,
          ^.transform := GraphUtil.move(
            Array(edge.points.head.x + firstVectorNorm.x * 9, edge.points.head.y + firstVectorNorm.y * 9)),
          ^.r := 10,
          ^.pointerEvents := "all"),
        <.circle(
          onMouseOver --> p.edgeEndOver(id),
          onMouseOut --> p.edgeEndOut(id),
          onMouseDown --> p.edgeEndDown(id),
          ^.stroke := "purple", ^.fill := "blue", if (edge.endOn) ^.opacity := 0.7 else ^.opacity := 0.01,
          ^.transform := GraphUtil.move(
            Array(edge.points.last.x - lastVectorNorm.x * 9, edge.points.last.y - lastVectorNorm.y * 9)),
          ^.r := 10,
          ^.pointerEvents := "all")
      )
    }
  }

  val component = ReactComponentB[Props]("graphComp")
    .stateless
    .renderBackend[Backend]
    .build


  def apply(p: Props) = component(p)
}
