package codes.quine.labo.unref

import scala.scalajs.js.annotation.JSExportTopLevel

/** export is a scope object to expose functions to JS. */
object export {

  /** Returns a regular expression resolved back-references. */
  @JSExportTopLevel("unref")
  def unrefJS(s: String): String = {
    val b = BRE.parse(s).getOrElse(throw new IllegalArgumentException(s"Invalid BRE: /$s/"))
    val r = unref(b)
    r.toString
  }
}
