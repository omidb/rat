package services

import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsOnline, TripsServers}
import rat.shared._

case class DB(tasks:List[TaskInfo], graphs:Map[(User, Int), TripsLF], golds:List[Gold])

object DB {

  def init() = {
    val tasks = List(
      TaskInfo(0, "I have a book",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty),
      TaskInfo(1, "He killed the man",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty),
      TaskInfo(2, "He ate the burger",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty),
      TaskInfo(3, "I am not happy",
        Map(
          UserManager.users.find(_.id == "james").get -> UnEdited,
          UserManager.users.find(_.id == "omid").get -> UnEdited
        ), isFinished = false, agreement = 0.0, List.empty)
    )

    val onlineParser = new TripsOnline()
    val graphs = tasks.flatMap(t =>
      t.userStat.map(u =>
        (u._1, t.id) -> TripsHelper.doc2lf(onlineParser.onlineParse(TripsServers.stepDev, t.sentence)))
    ).toMap
    db = DB(tasks, graphs, List.empty)
  }

  var db = DB(List.empty, Map.empty, List.empty)



}
