package rat.shared

import com.github.omidb.nlp.toolsInterface.TripsLF

case class AnnotationTask(id:Int,  users: List[User], agreement: Double, isFinished:Boolean)

case class Comment(user: User, value:String)
case class TaskInfo(id:Int, sentence:String,userStat: Map[User, GraphStatus],
                    isFinished:Boolean, agreement:Double, comments:List[Comment] = List.empty[Comment])

case class GoldInfo(id:Int, sentence:String, userStat: Map[User, GraphStatus], comment: List[Comment] = List.empty)
case class Gold(id:Int, sentence:String, graph:TripsLF)

//case class GraphEntity(id:Int, sentence:String, lf:TripsLF)

//case class GoldGraph(graph: SGraph, users: List[User])

