package rat.shared

case class AnnotInfo(
                      name: String,
                      DeletedNodes: Double,
                      DeletedEdges: Double,
                      AddedEdges: Double,
                      NodeSense: Double,
                      EdgeRole: Double
                      )

object AnnotInfo{
  def normalaize(a:AnnotInfo) = {
    val sum = Math.max(0.0000001,a.AddedEdges + a.DeletedEdges + a.DeletedNodes + a.EdgeRole + a.NodeSense)
    def pr(q:Double):Double = q*100/sum
    AnnotInfo(a.name,pr(a.DeletedNodes),pr(a.DeletedEdges),pr(a.AddedEdges),pr(a.NodeSense), pr(a.EdgeRole))
  }
}

case class AnnotationStats(size:Int, agreementHistory:Seq[RPoint], dataSize:Int, annotatedSize:Int, annotInfo:AnnotInfo)

case class AnnotationStatistics(myRemaining:Int, allSubmitted:Int, ready2Gold:Int ,goldSize:Int)
