package rat.shared

case class AnnotationTask(id:Int,  users: List[User], agreement: Double, isFinished:Boolean)

//case class TaskInfo(id:Int, sentence:String,userStat: Map[User, GraphStatus], isFinished:Boolean, agreement:Double)

case class GraphInfo(id:Int, sentence:String, graphStatus: GraphStatus)

//case class GoldGraph(graph: SGraph, users: List[User])

