import java.lang.String
import org.apache.commons.lang.StringUtils

object spam extends App {
  val s = List(
    "offer is secret"
    , "click secret link"
    , "secret sports link")
  val h = List(
    "play sports today"
    , "went play sports"
    , "secret sports event"
    , "sports is today"
    , "sports costs money")
  val m = List(
    //    "sports"
    "secret is secret"
    //    "today is secret"
  )

  val bd = BigDecimal
  type bd = BigDecimal

  implicit def withWords(s: String) = new {def words = StringUtils.split(s)}

  implicit def withDict(s: Seq[String]) = new {def dict = s.flatMap(_.words).groupBy(identity).map {case (s, l) => (s, bd(l.size))}.toMap.withDefaultValue(bd(0))}

  println((s ++ h).dict)

  val `p(s)` = bd(s.size) / (s ++ h).size
  val `p(h)` = bd(h.size) / (s ++ h).size

  def `p(m|_)`(prob: List[String]) = (bd(1) /: m.flatMap(_.words).map(prob.dict))((i, n) => i * n / prob.dict.values.sum)

  val `p(m|h)` = `p(m|_)`(h)
  val `p(m|s)` = `p(m|_)`(s)
  val `p(s|m)` = `p(m|s)` * `p(s)` / (`p(m|s)` * `p(s)` + `p(m|h)` * `p(h)`)
  println(`p(m|h)`)
  println(`p(m|s)`)
  println(`p(s|m)`)
}