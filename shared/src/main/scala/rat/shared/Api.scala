//package rat.shared
//
//trait Api {
//
//  //login
//  def signIn(userName: UserName): User
//
//  //def getUser
//  def getAllGraphProps(user: User): Seq[SGraphProps]
//
//  def getGraph(graphProps: SGraphProps, user: User): SGraph
//
//  def saveGraph(graph: SGraph, user: User)
//
//  def parseForUser(theUser: User, theIn: String, users: List[User], goldAlready:Boolean, tag:String, parser:String):String
//
//
//  def submitGraph(user: User,graph: SGraph)
//
//  def getTaskInfoList(): Seq[TaskInfo]
//
//  def recalculateAgreements()
//
//  def getSubmittedGraphs(taskID:Int): Map[User,SGraph]
//
//  def getAnnotationStats(user: User): AnnotationStatistics
//
//  def getNodeAlternatives(s:String): List[NodeAlternative]
//
//  def getUserSubmittedList(user: User):List[SGraphProps]
//
//  def getSubmittedGraph(graphProps:SGraphProps, user: User):SGraph
//
//  def reSubmittGraph(user:User, graph:SGraph)
//
//  def getOriginalGraph(graphID:String):Option[SGraph]
//
//  def moveToGoldList(user:User, taskID:Int, users:List[User])
//
//  def getGoldList():Seq[GoldGraph]
//
//  def parseAndEvaluate(graph:SGraph, useSkeleton:Boolean, parser:String):(SGraph,Double,String)
//
//  def rollBackGold(user:User, graphID: String):String
//
//  def deleteTask(user:User, taskID:Int):String
//
////  def getAllGraph(graphProps: SGraphProps, user: User): Seq[SGraph]
//
//}
