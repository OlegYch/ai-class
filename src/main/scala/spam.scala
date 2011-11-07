import org.apache.commons.lang.StringUtils

object spam extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

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
    //    "secret is secret"
    //    "today"
    "today is secret"
  )

  val movie = List(
    "a perfect world"
    , "my perfect woman"
    , "pretty woman"
  )
  val song = List(
    "a perfect day"
    , "electric storm"
    , "another rainy day"
  )
  val allTitles = movie ++ song
  val query = "perfect storm"

  implicit def withWords(s: String) = new {def words = StringUtils.split(s)}

  implicit def withDict(s: Seq[String]) = new {def dict = s.flatMap(_.words).groupBy(identity).map {case (s, l) => (s, bd(l.size))}.toMap.withDefaultValue(bd(0))}

  //  println((s ++ h).dict)

  val `p(s)` = bd(s.size) / (s ++ h).size
  val `p(h)` = bd(h.size) / (s ++ h).size

  def `p(m)`(m: String, dict: Map[String, bd]) = dict(m) / dict.values.sum

  def `ls(m)`(m: String, dict: Map[String, bd], allDict: Map[String, bd])(k: bd) = (dict(m) + k) / (dict.values.sum + k * allDict.size)

  def `p(m|_)`(prob: List[String]) = m.flatMap(_.words).map(`p(m)`(_, prob.dict)).product

  def `ls(m|_)`(m: Seq[String])(prob: Seq[String], allProb: Seq[String])(k: bd) = m.flatMap(_.words).map(`ls(m)`(_, prob.dict, allProb.dict)(k)).product

  val `p(m|h)` = `p(m|_)`(h)
  val `p(m|s)` = `p(m|_)`(s)
  val `p(s|m)` = `p(m|s)` * `p(s)` / (`p(m|s)` * `p(s)` + `p(m|h)` * `p(h)`)
  //  println(`p(m|h)`)
  //  println(`p(m|s)`)
  //  println(`p(s|m)`)

  val k = 0
  //  val k = 1

  def `ls(_)`(p: bd, s: bd) = (s + k) / (p + k * 2)

  //  println(`ls(s)`(1, 1))
  //  println(`ls(s)`(10, 6))
  //  println(`ls(s)`(100, 60))
  val `ls(s)` = `ls(_)`((s ++ h).size, s.size)
  val `ls(h)` = `ls(_)`((s ++ h).size, h.size)
  "ls(movie)" =: `ls(_)`(allTitles.size, movie.size)
  "ls(song)" =: `ls(_)`(allTitles.size, song.size)
  //  println(`ls(s)`)
  //  println(`ls(h)`)

  val `ls(m|s)` = `ls(m|_)`(m)(s, s ++ h)(k)
  val `ls(m|h)` = `ls(m|_)`(m)(h, s ++ h)(k)

  for (clazz <- List("movie" -> movie, "song" -> song)) {
    for (w <- query.words) {
      "ls(%s|%s)".format(w, clazz._1) =: `ls(m|_)`(w :: Nil)(clazz._2, allTitles)(k)
    }
    "ls(query|%s)".format(clazz._1) =: `ls(m|_)`(query :: Nil)(clazz._2, allTitles)(k)
  }

  //  println(`ls(m|s)`)
  //  println(`ls(m|h)`)
  val `ls(s|m)` = `ls(m|s)` * `ls(s)` / (`ls(m|s)` * `ls(s)` + `ls(m|h)` * `ls(h)`)
  "ls(movie|query)" =: "ls(query|movie)".bd * "ls(movie)" / ("ls(query|movie)".bd * "ls(movie)" + "ls(query|song)".bd * "ls(song)")

  //  println(`ls(s|m)`)

}