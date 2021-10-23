package codes.quine.labo.unref

class UtilSuite extends munit.FunSuite:
  test("Util.escape") {
    assertEquals(Util.escape('x'), "x")
    assertEquals(Util.escape('|'), "\\|")
    assertEquals(Util.escape('*'), "\\*")
    assertEquals(Util.escape('('), "\\(")
    assertEquals(Util.escape(')'), "\\)")
    assertEquals(Util.escape('\\'), "\\\\")
  }
