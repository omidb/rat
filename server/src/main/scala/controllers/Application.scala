package controllers

import java.nio.ByteBuffer

import boopickle.Default._
import com.github.omidb.nlp.formats.SExpression
import com.google.inject.Inject
import io.getquill._
//import play.api.db.evolutions.Evolutions
import play.api.{Configuration, Environment}
import play.api.mvc._
import rat.shared.Api2
import services.{AlternativeManager, ApiService, SearchEngine, Tasks}

import scala.concurrent.ExecutionContext.Implicits.global

object Router extends autowire.Server[ByteBuffer, Pickler, Pickler] {
  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)

}

class Application @Inject() (implicit val config: Configuration, env: Environment) extends Controller {

  lazy val db = new JdbcContext[SqliteDialect, CamelCase]("db")
  lazy val tasks = new Tasks(db)
//  tasks.repair()
  lazy val se = new SearchEngine(tasks)
  val apiService = new ApiService(tasks, se)

  def index = Action {
    Ok(views.html.index("RocNLP"))
  }

  def autowireApi(path: String) = Action.async(parse.raw) {
    implicit request =>
      println(s"Request path: $path")

      // get the request body as ByteString
      val b = request.body.asBytes(parse.UNLIMITED).get

//      println(b)
      // call Autowire route
      Router.route[Api2](apiService)(
        autowire.Core.Request(path.split("/"), Unpickle[Map[String, ByteBuffer]].fromBytes(b.asByteBuffer))
      ).map(buffer => {
        val data = Array.ofDim[Byte](buffer.remaining())
        buffer.get(data)
        Ok(data)
      })
  }

  def logging = Action(parse.anyContent) {
    implicit request =>
      request.body.asJson.foreach { msg =>
        println(s"CLIENT - $msg")
      }
      Ok("")
  }
}
