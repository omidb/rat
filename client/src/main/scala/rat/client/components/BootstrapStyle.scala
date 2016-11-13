package rat.client.components

import scalacss.Defaults._
import scalacss.mutable
import rat.client.components.Bootstrap.CommonStyle._

class BootstrapStyles(implicit r: mutable.Register) extends StyleSheet.Inline()(r) {

  import dsl._

  val csDomain = Domain.ofValues(default, primary, success, info, warning, danger)

  val contextDomain = Domain.ofValues(success, info, warning, danger)

  def commonStyle[A](domain: Domain[A], base: String) = styleF(domain)(opt =>
    styleS(addClassNames(base, s"$base-$opt"))
  )

  def newStyle[A](domain: Domain[A], base: String, secondory:String) = styleF(domain)(opt =>
    styleS(addClassNames(base, s"$base-$secondory-$opt"))
  )

  def styleWrap(classNames: String*) = style(addClassNames(classNames: _*))


  val buttonOpt = commonStyle(csDomain, "btn")

  val buttonOutlineOpt = newStyle(csDomain, "btn", "outline")

  val buttonListOpt = commonStyle(csDomain, "btn")

  val button = buttonOpt(default)

  val panelOpt = commonStyle(csDomain, "panel")

  val panel = panelOpt(default)

  val labelOpt = commonStyle(csDomain, "label")

  val label = labelOpt(default)

  val tagOpt = commonStyle(csDomain, "tag")

  val tag = tagOpt(default)

  val alert = commonStyle(contextDomain, "alert")

  val panelHeading = styleWrap("panel-heading")

  val panelBody = styleWrap("panel-body")

  val cardOpt = commonStyle(csDomain, "card")

  val card = panelOpt(default)

  val cardHeading = styleWrap("card-header")

  val cardBlock = styleWrap("card-block")

  val cardTitle = styleWrap("card-title")

  val cardText = styleWrap("card-text")

  // wrap styles in a namespace, assign to val to prevent lazy initialization
  object modal {
    val modal = styleWrap("modal")
    val fade = styleWrap("fade")
    val dialog = styleWrap("modal-dialog")
    val content = styleWrap("modal-content")
    val header = styleWrap("modal-header")
    val body = styleWrap("modal-body")
    val footer = styleWrap("modal-footer")
  }

  val _modal = modal

  object listGroup {
    val listGroup = styleWrap("list-group")
    val item = styleWrap("list-group-item")
    val itemOpt = commonStyle(contextDomain, "list-group-item")
  }

  val _listGroup = listGroup
  val pullRight = styleWrap("pull-right")
  val buttonXS = styleWrap("btn-sm")
  val close = styleWrap("close")

  val labelAsBadge = style(addClassName("label-as-badge"), borderRadius(1.em))

  val navbar = styleWrap("nav", "navbar-nav")

  val formGroup = styleWrap("form-group")
  val formControl = styleWrap("form-control")
}
