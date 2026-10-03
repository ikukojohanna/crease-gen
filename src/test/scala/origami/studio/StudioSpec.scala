package origami.studio

import origami.geometry.Tol
import origami.output.TrueScaleSvg

class StudioSpec extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)

  test("a query string becomes a design; junk keeps the defaults and sizes stay small") {
    val d = Design.parse(Map("kind" -> "voronoi", "size" -> "500", "s" -> "0.4", "twist" -> "x", "paper" -> "-3"))
    assertEquals(d.kind, "voronoi")
    assertEquals(d.size, 10)
    assertEquals(d.s, 0.4)
    assertEquals(d.twist, Design().twist)
    assertEquals(d.paperMm, 20.0)
    assertEquals(Design.parse(Map("kind" -> "nonsense")).kind, Design().kind)
  }

  test("the paper is as many millimetres across as asked, for every tiling") {
    for kind <- Design.kinds do
      val (t, _) = Design(kind = kind, size = 2, paperMm = 240).tiling
      val b = t.paper.bounds
      assertEqualsDouble(math.max(b.width, b.height), 240.0, 1e-6, kind)
  }

  test("the exported SVG is drawn at true size in millimetres") {
    val d = Design(kind = "hexagonal", paperMm = 180)
    val cp = d.pattern(d.tiling._1).fold(p => fail(p.explain), identity)
    val svg = TrueScaleSvg.render(cp)
    assert(svg.contains("""width="180.000mm""""), svg.take(200))
    assert(svg.contains("""viewBox="0 0 180.000"""), svg.take(200))
  }

  test("a rejected design still has a preview, so the page can show what breaks") {
    val d = Design(twist = 10)
    val t = d.tiling._1
    assert(d.pattern(t).isLeft)
    assert(d.preview(t).creases.nonEmpty)
  }

  test("JSON strings escape quotes, backslashes and control characters") {
    assertEquals(Json.str("a\"b\\c\nd\u0001"), "\"a\\\"b\\\\c\\nd\\u0001\"")
  }
