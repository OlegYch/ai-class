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

  def `p(m)`(m: String, dict: Map[String, bd]) = dict(m) / dict.values.sum

  def `ls(m)`(m: String, dict: Map[String, bd], allDict: Map[String, bd])(k: bd) = (dict(m) + k) / (dict.values.sum + k * allDict.size)

  def `p(m|_)`(prob: List[String]) = m.flatMap(_.words).map(`p(m)`(_, prob.dict)).product

  def `ls(m|_)`(m: Seq[String])(prob: Seq[String], allProb: Seq[String])(k: bd) = m.flatMap(_.words).map(`ls(m)`(_, prob.dict, allProb.dict)(k)).product

  val `p(m|h)` = `p(m|_)`(h)
  val `p(m|s)` = `p(m|_)`(s)
  val `p(s|m)` = `p(m|s)` * `p(s)` / (`p(m|s)` * `p(s)` + `p(m|h)` * `p(h)`)
  println(`p(m|h)`)
  println(`p(m|s)`)
  println(`p(s|m)`)

  val k = 1

  def `ls(s)`(p: bd, s: bd) = (s + k) / (p + k * 2)

  //  println(`ls(s)`(1, 1))
  //  println(`ls(s)`(10, 6))
  //  println(`ls(s)`(100, 60))
  println(`ls(s)`((s ++ h).size, s.size))
  println(`ls(s)`((s ++ h).size, h.size))
  println(`ls(m|_)`("today" :: Nil)(s, s ++ h)(k))
  println(`ls(m|_)`("today" :: Nil)(h, s ++ h)(k))
}