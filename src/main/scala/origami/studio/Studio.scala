package origami.studio

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import origami.folding.FoldedModel
import origami.geometry.Tol
import origami.laws.Laws
import origami.output.{Fold, FoldedOptions, FoldedSvg, Svg, SvgOptions, TrueScaleSvg}

import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{CompletableFuture, Executors, TimeUnit, TimeoutException}
import java.util.concurrent.atomic.AtomicBoolean

/** The local studio: `scala-cli run . -- serve [port]`, then open http://localhost:8080. */
object Studio:

  /** One layer solve at a time: a solve that times out keeps running, and must finish before the next starts. */
  private val solving = AtomicBoolean(false)
  private val solver = Executors.newSingleThreadExecutor(r => { val t = Thread(r, "layer-solver"); t.setDaemon(true); t })

  def serve(port: Int)(using Tol): Unit =
    val server = HttpServer.create(InetSocketAddress("127.0.0.1", port), 0)
    server.setExecutor(Executors.newFixedThreadPool(4))
    route(server, "/")(_ => ("text/html; charset=utf-8", page, None))
    route(server, "/pattern")(q => json(pattern(Design.parse(q))))
    route(server, "/fold")(q => json(fold(Design.parse(q), q.get("timeout").flatMap(_.toDoubleOption).getOrElse(20))))
    route(server, "/export.svg")(q => exported(Design.parse(q), "svg"))
    route(server, "/export.fold")(q => exported(Design.parse(q), "fold"))
    server.start()
    println(s"studio -> http://localhost:$port")

  /** The pattern, its local-law violations and narrow pleats marked; a rejected design is still drawn. */
  private def pattern(d: Design)(using Tol): String =
    val (tiling, dropped) = d.tiling
    val built = d.pattern(tiling)
    val cp = built.getOrElse(d.preview(tiling))
    val violations = Laws.checkAll(cp.planarize)
    val narrow = d.narrowPleats(tiling)
    val marks = violations.map(_.where) ++ narrow
    val svg = Svg.render(cp, SvgOptions(scale = 560 / d.paperMm, margin = 16, showLegend = true, marks = marks))
    Json.obj(
      "ok" -> Json.bool(built.isRight && violations.isEmpty),
      "problem" -> built.left.toOption.fold(Json.nul)(p => Json.str(p.explain)),
      "violations" -> Json.arr(violations.take(50).map(v => Json.str(v.explain))),
      "violationCount" -> violations.length.toString,
      "warnings" -> Json.arr(
        Option.when(narrow.nonEmpty)(f"${narrow.length} pleat(s) under ${d.minPleatMm}%.1f mm wide or long").toSeq ++
          Option.when(dropped > 0)(s"$dropped refinement point(s) dropped to keep pleats foldable").toSeq map Json.str),
      "stats" -> Json.str(s"${tiling.tiles.length} tiles, ${cp.creases.length} creases"),
      "svg" -> Json.str(svg))

  /** Runs the layer solver for at most `seconds`. A timeout is reported as a timeout, not as a verdict. */
  private def fold(d: Design, seconds: Double)(using Tol): String =
    val (tiling, _) = d.tiling
    d.pattern(tiling) match
      case Left(p) => Json.obj("status" -> Json.str("rejected"), "message" -> Json.str(p.explain))
      case Right(cp) if solving.compareAndSet(false, true) =>
        val job = CompletableFuture.supplyAsync(() =>
          try FoldedModel.of(cp).map(m => (m, m.thickness)) finally solving.set(false), solver)
        val started = System.nanoTime
        try
          val result = job.get((seconds * 1000).toLong, TimeUnit.MILLISECONDS)
          val millis = (System.nanoTime - started) / 1000000
          result match
            case Right((m, layers)) =>
              Json.obj("status" -> Json.str("folds"), "layers" -> layers.toString, "millis" -> millis.toString,
                "svg" -> Json.str(FoldedSvg.render(m, FoldedOptions(size = 520))))
            case Left(why) =>
              Json.obj("status" -> Json.str("does not fold"), "message" -> Json.str(why.explain), "millis" -> millis.toString)
        catch
          case _: TimeoutException =>
            Json.obj("status" -> Json.str("timeout"),
              "message" -> Json.str(f"no answer within $seconds%.0f s; the solver is still running and must finish first"))
      case Right(_) =>
        Json.obj("status" -> Json.str("busy"), "message" -> Json.str("an earlier check is still running; try again shortly"))

  private def exported(d: Design, format: String)(using Tol): (String, String, Option[String]) =
    val (tiling, _) = d.tiling
    d.pattern(tiling) match
      case Left(p) => ("text/plain; charset=utf-8", s"not exported: ${p.explain}\n", None)
      case Right(cp) =>
        val name = f"${d.kind}-s${d.s}%.2f-a${d.twist}%.0f-${d.paperMm}%.0fmm"
        if format == "svg" then ("image/svg+xml", TrueScaleSvg.render(cp), Some(s"$name.svg"))
        else ("application/json", Fold(cp, name), Some(s"$name.fold"))

  private def json(body: String): (String, String, Option[String]) = ("application/json", body, None)

  private def route(server: HttpServer, path: String)(handle: Map[String, String] => (String, String, Option[String])): Unit =
    server.createContext(path, (ex: HttpExchange) =>
      val response =
        if path == "/" && ex.getRequestURI.getPath != "/" then None
        else Some(try handle(query(ex)) catch case e: Exception => ("text/plain", s"error: $e", None))
      response match
        case None => ex.sendResponseHeaders(404, -1)
        case Some((contentType, body, download)) =>
          val bytes = body.getBytes(UTF_8)
          ex.getResponseHeaders.set("Content-Type", contentType)
          download.foreach(n => ex.getResponseHeaders.set("Content-Disposition", s"attachment; filename=\"$n\""))
          ex.sendResponseHeaders(200, bytes.length)
          ex.getResponseBody.write(bytes)
      ex.close())

  private def query(ex: HttpExchange): Map[String, String] =
    Option(ex.getRequestURI.getRawQuery).toVector.flatMap(_.split("&")).flatMap: kv =>
      kv.split("=", 2) match
        case Array(k, v) => Some(URLDecoder.decode(k, UTF_8) -> URLDecoder.decode(v, UTF_8))
        case _           => None
    .toMap

  private lazy val page: String =
    val in = Option(getClass.getResourceAsStream("/studio.html")).getOrElse(throw IllegalStateException("studio.html missing"))
    try String(in.readAllBytes(), UTF_8) finally in.close()
