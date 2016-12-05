package rat.shared

import com.github.omidb.nlp.toolsInterface.TripsServers

object SharedUtil {
  val availableParsers = List("drum-dev", "drum", "step", "rochester", "ihmc", "step-dev", "cabot")

  val parserAddress = Map("step" -> TripsServers.step, "drum" -> TripsServers.drum, "step-dev" -> TripsServers.stepDev,
    "drum-dev" -> TripsServers.drumDev, "ihmc" -> TripsServers.ihmc, "rochester" -> TripsServers.rochester, "cabot"-> "http://trips.ihmc.us/parser/cgi/cabot")

  def getEdgeID(e1:String, e2:String) = e1 + "-->" + e2
  def parseEdgeID(e:String) = e.split("-->")

  def move(p: Array[Double]) = s"translate(${p(0)},${p(1)})"

  def textBoundingBox(p: Array[String]) =
    RVector(p.map(_.size).max * 12 + p.maxBy(_.length).count(x => x == ' ') * 25, p.length * 20)

  def edgeAlterList = Map(
    "speechact" -> List(
    EdgeAlternative("1", "content"),
    EdgeAlternative("2", "focus")),
    "pronoun" -> List(
    EdgeAlternative("6", "name-of")),
    "predicate" -> List(
    EdgeAlternative("7", "proform"),
    EdgeAlternative("8", "of"),
    EdgeAlternative("9", "val"),
    EdgeAlternative("10", "figure"),
    EdgeAlternative("11", "ground"),
    EdgeAlternative("12", "scale"),
    EdgeAlternative("75", "standard"),
    EdgeAlternative("77", "condition")),
    "quantities" -> List(
    EdgeAlternative("13", "unit"),
    EdgeAlternative("14", "amount")),
    "quantified" -> List(
    EdgeAlternative("15", "quan"),
    EdgeAlternative("16", "refset")),
    "sequences" -> List(
    EdgeAlternative("17", "operator"),
    EdgeAlternative("18", "sequence"),
    EdgeAlternative("19", "sequence1"),
    EdgeAlternative("20", "sequence2"),
    EdgeAlternative("21", "sequence3"),
    EdgeAlternative("22", "sequence4"),
    EdgeAlternative("23", "sequence5")),
    "time/date" -> List(
    EdgeAlternative("24", "year"),
    EdgeAlternative("25", "month"),
    EdgeAlternative("26", "day"),
    EdgeAlternative("27", "dow"),
    EdgeAlternative("28", "am-pm"),
    EdgeAlternative("29", "hour"),
    EdgeAlternative("30", "minute"),
    EdgeAlternative("31", "century"),
    EdgeAlternative("32", "era"),
    EdgeAlternative("33", "phase")),
  "semantic-roles" -> List(
    EdgeAlternative("34", "agent"),
    EdgeAlternative("35", "agent1"),
    EdgeAlternative("36", "agent2"),
    EdgeAlternative("37", "agent3"),
    EdgeAlternative("38", "affected"),
    EdgeAlternative("39", "affected1"),
    EdgeAlternative("40", "affected2"),
    EdgeAlternative("41", "affected3"),
    EdgeAlternative("42", "affected-result"),
    EdgeAlternative("43", "neutral"),
    EdgeAlternative("44", "neutral1"),
    EdgeAlternative("45", "neutral2"),
    EdgeAlternative("46", "neutral3"),
    EdgeAlternative("47", "formal"),
    EdgeAlternative("48", "formal1"),
    EdgeAlternative("49", "formal2"),
    EdgeAlternative("50", "result"),
    EdgeAlternative("51", "result1"),
    EdgeAlternative("52", "result2"),
    EdgeAlternative("53", "method"),
    EdgeAlternative("54", "manner"),
    EdgeAlternative("55", "time"),
    EdgeAlternative("56", "time1"),
    EdgeAlternative("57", "time2"),
    EdgeAlternative("58", "extent"),
    EdgeAlternative("59", "frequency"),
    EdgeAlternative("60", "beneficiary"),
    EdgeAlternative("61", "reason"),
    EdgeAlternative("62", "location"),
    EdgeAlternative("63", "location1"),
    EdgeAlternative("64", "source"),
    EdgeAlternative("65", "experiencer"),
    EdgeAlternative("66", "experiencer1"),
    EdgeAlternative("67", "experiencer2"),
    EdgeAlternative("68", "qualification"),
    EdgeAlternative("72", "degree"),
    EdgeAlternative("73", "size"),
    EdgeAlternative("76", "quantity"),
    EdgeAlternative("74", "orientation"),
      EdgeAlternative("69", "other")),
  "mod-link" -> List(
    EdgeAlternative("3", "mod"),
    EdgeAlternative("4", "assoc-with"),
    EdgeAlternative("5", "assoc-poss"),
    EdgeAlternative("70", "suchthat"),
    EdgeAlternative("77", "identified-as")),
  "multi-word" -> List(EdgeAlternative("71", "multi"))
  )



  lazy val allEdgeAlterMappings = (("all" -> edgeAlterList.toList.flatMap(_._2)) ::edgeAlterList.toList).toMap


  val speechActAlters = List("sa_ack"
    , "sa_apologize"
    , "sa_close"
    , "sa_confirm"
    , "sa_discourse-manage"
    , "sa_discourse-signal"
    , "sa_evaluation"
    , "sa_expressive"
    , "sa_go-offline"
    , "sa_go-online"
    , "sa_greet"
    , "sa_nolo-comprendez"
    , "sa_question"
    , "sa_how-question"
    , "sa_query"
    , "sa_tag-question"
    , "sa_wh-question"
    , "sa_what-if-question"
    , "sa_why-question"
    , "sa_yn-question"
    , "sa_reject"
    , "sa_request"
    , "sa_suggest"
    , "sa_tell"
    , "sa_thank"
    , "sa_welcome")


  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }
}


