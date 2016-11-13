package rat.client.components

import paths.high.{Pie}

import scala.scalajs.js
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all.key
import japgolly.scalajs.react.vdom.svg.all._
import rat.client.components.Colors.Color
object piechart {

  class Backend($: BackendScope[List[PieChartValue], Unit]) {


    def render(props: List[PieChartValue]) ={
      val pie = Pie[PieChartValue](
        data = props,
        accessor = _.piVal,
        r = 0,
        R = 160.0,
        center = (0, 0)
      )

      val slices = pie.curves map { curve =>
        g(key := curve.item.name)(
          linearGradient(
            id := s"grad-${ curve.index }",
            stop(stopColor := Colors.string(palette(curve.index)), offset := "0%"),
            stop(stopColor := Colors.string(Colors.lighten(palette(curve.index))), offset := "100%")
          ),
          path(d := curve.sector.path.print , fill := s"url(#grad-${ curve.index })")
        )
      }
      val texts = pie.curves map { curve =>
        g(key := "txt"+curve.item.name)(
          text(
            transform := move(curve.sector.centroid),
            textAnchor := "middle",
            curve.item.name
          )
        )
      }

      svg(width := 500, height := 500,
        g(transform := move(js.Array(250, 250)),
          slices ++ texts
        )
      )
    }

  }
  case class PieChartValue(name: String, piVal: Int)

  private def move(p: js.Array[Double]) = s"translate(${ p(0) },${ p(1) })"
  private val palette = Colors.mix(Color(130, 140, 210), Color(180, 205, 150))

  val PieChart = ReactComponentB[List[PieChartValue]]("Pie chart")
    .stateless
    .renderBackend[Backend]
    .build
}




object Colors {
  case class Color(r: Double, g: Double, b: Double, alpha: Double = 1)

  def cut(x: Double) = x.floor min 255

  def multiply(factor: Double) = { c: Color =>
    Color(cut(factor * c.r), cut(factor * c.g), cut(factor * c.b), c.alpha)
  }

  def average(c1: Color, c2: Color) =
    Color(
      cut((c1.r + c2.r) / 2),
      cut((c1.g + c2.g) / 2),
      cut((c1.b + c2.b) / 2),
      (c1.alpha + c2.alpha) / 2
    )

  val lighten = multiply(1.2)
  val darken = multiply(0.8)

  def mix(c1: Color, c2: Color) = {
    val c3 = average(c1, c2)
    List(
      lighten(c1),
      c1,
      darken(c1),
      lighten(c3),
      c3,
      darken(c3),
      lighten(c2),
      c2,
      darken(c2)
    )
  }

  def transparent(c: Color, alpha: Double = 0.7) = c.copy(alpha = alpha)

  def string(c: Color) =
    if (c.alpha == 1) s"rgb(${ c.r.floor },${ c.g.floor },${ c.b.floor })"
    else s"rgba(${ c.r.floor },${ c.g.floor },${ c.b.floor },${ c.alpha })"
}


//import scala.scalajs.js.{ Date, Math }
//import org.scalajs.dom.window
//import japgolly.scalajs.react.BackendScope
//
//
//object animate {
//  trait Interpolable[A] {
//    def mix(a: A, b: A, t: Double): A
//  }
//
//  case class AnimateOptions(
//                             duration: Double = 500,
//                             easing: Double => Double = Easing.easeInQuad,
//                             done: Unit => Unit = { _ => () }
//                           )
//
//  object Easing {
//    private val s = 1.70158
//
//    def linear(t: Double) = t
//
//    def easeInQuad(t: Double) = t * t
//
//    def easeOutQuad(t: Double) = -t * (t - 2)
//
//    def easeInOutQuad(t: Double) =
//      if (t < 1/2) 2 * t * t else -2 * t * t + 4 * t - 1
//
//    def easeInElastic(t: Double) = {
//      val q = t - 1
//      -Math.pow(2, 10 * q) * Math.sin((2 * q / 0.3 - 0.5) * Math.PI)
//    }
//
//    def easeOutElastic(t: Double) =
//      Math.pow(2, -10 * t) * Math.sin( (2 * t / 0.3 - 0.5)* Math.PI) + 1
//
//    def easeInOutElastic(t: Double) = {
//      val q = 2 * t - 1
//
//      if (t < 1/2) -0.5 * Math.pow(2, 10 * q) * Math.sin((q / 0.225 - 0.5) * Math.PI)
//      else Math.pow(2,-10 * q) * Math.sin((q / 0.225 - 0.5) * Math.PI) * 0.5 + 1
//    }
//
//    def easeInBack(t: Double) = t * t * ((s + 1) * t - s)
//
//    def easeOutBack(t: Double) = {
//      val q = t - 1
//
//      q * q * ((s + 1) * q + s) + 1
//    }
//
//    def easeInOutBack(t: Double) = {
//      val r = s * 1.525
//      if (t < 1 / 2) 2 * t * t * ((r + 1) * 2 * t - r)
//      else {
//        val q = t - 1
//        2 * q * q * ((r + 1) * 2 * q + r) + 1
//      }
//    }
//
//    def easeInBounce(t: Double) = 1 - easeOutBounce(1 - t)
//
//    def easeOutBounce(t: Double) = {
//      val q = 2.75 * t
//      val l = 7.5625
//      if (q < 1) l * t * t
//      else if (q < 2) {
//        val p = t - 1.5 / 2.75
//        l * p * p + 0.75
//      }
//      else if (q < 2.5) {
//        val p = t - 2.25 / 2.75
//        l * p * p + 0.9375
//      }
//      else {
//        val p = t - 2.625 / 2.75
//        l * p * p + 0.984375
//      }
//    }
//
//    def easeInOutBounce(t: Double) =
//      if (t < 1/2) easeInBounce(2 * t) / 2 else (easeOutBounce(2 * t - 1) + 1) / 2
//  }
//
//  implicit val interpolateNumbers = new Interpolable[Double] {
//    def mix(a: Double, b: Double, t: Double) = a + (b - a) * t
//  }
//
//  implicit class AnimateBackendScope[P, S](val $: BackendScope[P, S]) extends AnyVal {
//    def animateState(finalState: S, options: AnimateOptions = AnimateOptions())(implicit i: Interpolable[S]) = {
//      val start = Date.now()
//
//      def update(d: Double = 0): Unit = {
//        val now = Date.now()
//        val t = ((now - start) / options.duration) min 1
//        $.modState(initState => i.mix(initState, finalState, options.easing(t)))
//
//        if (t < 1) window.requestAnimationFrame(update _) else options.done(())
//      }
//
//      update()
//    }
//
//    def animateModState(mod: S => S, options: AnimateOptions = AnimateOptions())(implicit i: Interpolable[S]) = {
//      val ss = $.state.map(s => animateState(mod(s), options))
//      ss
//    }
//  }
//}
