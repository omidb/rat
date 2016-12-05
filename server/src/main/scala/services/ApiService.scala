package services

import com.github.omidb.nlp.formats.SExpression
import rat.shared._
import com.github.omidb.nlp.toolsInterface.{TripsLF, TripsOnline, TripsServers}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.getquill._
import Impls._

class ApiService(db:Tasks, se:SearchEngine) extends Api2{
  import db.db._

  def calcLStDiff() = {
    run(db.golds).flatMap{ g =>
      db.decodeLF(g.graph).get.graph.nodes
        .filter(n => n._2.value.contains("word") && n._2.value.contains("type"))
        .map(n => {
          val value = n._2.value("word").toLowerCase
          n._2 -> {
            if (value.toLowerCase.startsWith("sa_"))
              SharedUtil.speechActAlters
            else
              AlternativeManager.getAllSenses(value).map(_.typ.toLowerCase)

          }
        })
        .filterNot(n => n._2.contains(n._1.value("type").toLowerCase))
        .map(n => OntologyStat(g.id, n._1.value("word"), n._1.value("type"),
          AlternativeManager.ont.-->(n._1.value("type").toLowerCase).map(_.words).getOrElse(List.empty)))
    }.toList.sortBy(_.word)
  }
  var lstDiff = calcLStDiff()


  override def signIn(userName: UserName):User = {
    UserManager.checkUser(userName)
  }



  override def getAnnotationStats(user: User): AnnotationStatistics = {

    val gldSize = run(db.golds).size
    val tsks = run(db.tasks).map(TaskRow.toTaskInfo)
    AnnotationStatistics(
      tsks.count(t => t.userStat.contains(user.id)), tsks.size, tsks.count(t => t.agreement == 1.0), gldSize, lstDiff)
  }

  override def calculateAgreement(user:User): Unit = {
    lstDiff = calcLStDiff()
    run(db.tasks).map(TaskRow.toTaskInfo)
      .foreach(t => {
        val x = run(db.graphs.filter(_.id == lift(t.id)).map(_.graph)).flatMap(str => db.decodeLF(str))
        println(t.id)
        val score = if(t.userStat.values.toList.contains(UnEdited)) 0.0 else calcsScore(x)
        val isFinished = if(score == 1.0 || t.userStat.forall(_._2 == Impossible)) 1 else 0
        run(db.tasks.filter(_.id == lift(t.id)).update(_.score -> lift(score), _.isFinished -> lift(isFinished)))
      })
  }

  override def getUserTasks(user: User): Option[List[TaskInfo]] = {
    val res = Some(run(db.tasks).map(tr => TaskRow.toTaskInfo(tr)).filter(_.userStat.keys.toList.contains(user.id)))
    res
  }

  override def getUserGraph(user: User, id: Int): Option[TripsLF] = {
    run(db.graphs.filter(g => g.user == lift(user.id) && g.id == lift(id)).map(_.graph))
      .headOption.flatMap(x => db.decodeLF(x))
  }

  override def getNodeAlters(value:String): Option[List[NodeAlternative]] = {
    if(value.toLowerCase.startsWith("sa_"))
      Some(SharedUtil.speechActAlters.map(s => NodeAlternative("0", s, s, List.empty, isWordNetMapping = false, "0" )))
    else
      Some(AlternativeManager.getAllSenses(value.toString))
  }

  override def getEdgeAlters(value:String): Option[List[EdgeAlternative]] = {
    Some(List(EdgeAlternative("1", "EdgeAlter1"), EdgeAlternative("2", "EdgeAlter2")))
  }


  override def getAllTasks(user: User): Option[List[TaskInfo]] = {
    Some(run(db.tasks).map(TaskRow.toTaskInfo))
  }

  override def deleteTask(user:User, taskID:Int): ResultStatus = {
    val r = run(db.tasks.filter(_.id == lift(taskID)).delete)
    if(r == 1) SuccessResult else FailResult
  }

  override def goldTask(user:User, taskID:Int): ResultStatus = {
    val r = transaction {
      val lf = run(db.graphs.filter(_.id == lift(taskID)).map(_.graph)).head
      val tsk = run(db.tasks.filter(_.id == lift(taskID)).map(x => (x.id, x.sentence, x.domain, x.usersStats, x.comments))).head
      run(db.golds.insert(lift(GoldRow(tsk._1, tsk._2, tsk._3, tsk._4, lf, tsk._5))))
      run(db.tasks.filter(_.id == lift(taskID)).delete)
      run(db.graphs.filter(_.id == lift(taskID)).delete)
    }
    if(r == 1) SuccessResult else FailResult
  }

  override def saveTask(user:User, taskID:Int, lf: TripsLF): ResultStatus = {
    val otherGraphs = run(db.tasks.filter(_.id == lift(taskID)))
      .flatMap(tr => db.decodeUserStats(tr.usersStats).keys.toList)
      .filterNot(_ == user.id)
      .flatMap(u => getUserGraph(UserManager.users(u), taskID))

    val agreement = calcsScore(lf :: otherGraphs)

    val grphStr = lf.asJson.noSpaces
    val r = transaction {
      run(db.graphs.filter(gr => gr.id == lift(taskID) && gr.user == lift(user.id))
          .update(_.graph -> lift(grphStr)))
//
      val us = run(db.tasks.filter(_.id == lift(taskID)).map(x => x.usersStats))
        .headOption.map(str => db.decodeUserStats(str).updated(user.id, Submitted).toList)
      val usStr = us.head.asJson.noSpaces
      run(db.tasks.filter(_.id == lift(taskID)).update(_.usersStats -> lift(usStr), _.score -> lift(agreement)))
    }
    if(r == 1) SuccessResult else FailResult
  }

  override def saveGold(user:User, taskID:Int, lf: TripsLF): ResultStatus = {
    val newG = lf.asJson.noSpaces
    val r = run(db.golds.filter(_.id == lift(taskID)).update(_.graph -> lift(newG)))
    if(r == 1) SuccessResult else FailResult
  }

  override def getTask(user:User, taskID:Int): Map[User, TripsLF] =  {
    run(db.graphs.filter(_.id == lift(taskID)).map(gr => gr.user -> gr.graph)).map{
      case (usr, gr) => UserManager.users(usr) -> db.decodeLF(gr).head
    }.toMap
  }

  override def getAllGolds(): Option[List[TaskInfo]] = {
    Some(run(db.golds).map(g =>
      TaskInfo(g.id, g.sentence, db.decodeUserStats(g.usersStats), isFinished = true, 1.00, db.decodeComments(g.comments),
        g.domain)))
  }

  override def rollBack(goldID: Int) = {
    val r = transaction{
      val gld = run(db.golds.filter(_.id == lift(goldID))).head
      val task = TaskRow(goldID, gld.sentence, gld.domain, gld.usersStats, 1.00, 1, gld.comments)
      run(db.golds.filter(_.id == lift(goldID)).delete)
      run(db.tasks.insert(lift(task)))
      db.decodeUserStats(task.usersStats).keys.toList.foreach(u =>
        run(db.graphs.insert(lift(GraphRow(task.id, u, gld.graph))))
      )
    }
    if(r == 1) SuccessResult else FailResult
  }

  override def getGold(taskID:Int): Option[TripsLF] = {
    run(db.golds.filter(_.id == lift(taskID))).headOption.flatMap(gr => db.decodeLF(gr.graph))
  }

  override def searchGolds(search:String): Option[List[TaskInfo]] = {
    val res = SExpression.parse(search)
    res.map(s =>
      se.search(s)
    )
  }


  override def getAllDomainGold(): Option[Map[String, List[TaskInfo]]] = {
    val golds = getAllGolds()
    golds.map(gld => gld.groupBy(_.domain))
  }

  override def evalGoldID(id:Int, parser:String):Double = {
//    println(parser, SharedUtil.parserAddress.getOrElse(parser, TripsServers.drumDev))
    val onlineParser = new TripsOnline()
    val g2 = TripsHelper.doc2lf(
      onlineParser.onlineParse(SharedUtil.parserAddress.getOrElse(parser, TripsServers.drumDev),
        run(db.golds.filter(_.id == lift(id))).head.sentence))

    val g1 = db.decodeLF(run(db.golds.filter(_.id == lift(id)).map(_.graph)).head).get

//    println(g2)
//    println("---------------")
//    println(g1)
    val res = LFUtils.diff(g1, g2)
//    println(res)
    res
  }

  def calcsScore(lfs:List[TripsLF]) = {
    if(lfs.size == 1) 1.0
    else {
      val dbs = lfs.combinations(2).toList
      dbs.map(lfs2 => {
        val s = LFUtils.diff(lfs2.head, lfs2(1))
        s
      }).sum / dbs.size
    }
  }

  override def getLispForGolds(ids:List[Int]):String = {
    val glds = run(db.golds).map(g => (g.id, g.sentence, db.decodeLF(g.graph).get))
    ids.map(i => glds.find(_._1 == i).get).map(g => {
      s";;;;;;;;;;;;;;;;;;;\n;;;${g._2}\n${LFUtils.toLisp(g._3)}"
    }).mkString("\n\n")
  }

}
