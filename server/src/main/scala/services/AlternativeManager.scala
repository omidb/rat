package services


import java.io.FileWriter
import java.nio.ByteBuffer
import java.nio.file.Files

import rat.shared._

import scala.collection.TraversableLike


object AlternativeManager {
  import strips.ontology._
  import strips.util.OntologyFromXML

  import strips.lexicon._
  import strips.util.LexiconFromXML

//  val ontPath = "./ont.bin"
//  val lexPath = "./lex.bin"

  println(new java.io.File(".").getAbsolutePath)
  println("Staring to load Trips Lexicon")

  val ont = {
    println("Loading Ontology")
    val data = SOntology(OntologyFromXML("./flaming-tyrion/lexicon/data/"))
    val s = upickle.default.write(data)
    //SimpleWriter.write(s, ontPath)
    data
  }
  val lex = {
    println("Loading Lexicon")
    val data = TripsLexicon(LexiconFromXML("./flaming-tyrion/lexicon/data/"))
    val s = upickle.default.write(data)
    //SimpleWriter.write(s, lexPath)
    data
  }

  def getAllSenses(s: String): List[NodeAlternative] = {
    val stmWrd = PorterStemmer.stem(s)
    val wrds =
      if (s == stmWrd)
        List(s)
      else
        List(s, stmWrd)

    val altrWrd1 = wrds.flatMap(wrd =>
      (ont.!@(wrd).map(w => w.name -> false)
        ::: lex.!(wrd).map(w => w -> true) ::: ont.-->(wrd).map(w => w.name -> false).toList
        )
    )

    val altrWrd = altrWrd1.map(_._1).distinct.map(d => {
      val all = altrWrd1.filter(_._1 == d)
      if (all.exists(_._2 == false))
        all.filter(_._2 == false).head
      else
        all.head
    })

    val alter = altrWrd.map { case (t, b) =>
      val path2root = ont.^^(t)
      NodeAlternative(t, t, t, path2root, b, "VV")
    }
    alter
  }

}

object SimpleWriter {
  def write(data: String, name: String) = {
    val fw = new FileWriter(name, false)
    fw.write(data)
    fw.close()
  }
}

