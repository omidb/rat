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


case class NodeAlternative(id:String, typ:String, word:String, path2root:List[String],
                           isWordNetMapping:Boolean, version:String)

case class EdgeAlternative(id:String, value:String)


sealed trait GraphStatus

case object Submitted extends GraphStatus

case object UnEdited extends GraphStatus

case object Impossible extends GraphStatus


sealed trait ResultStatus

case object SuccessResult extends ResultStatus

case object FailResult extends ResultStatus