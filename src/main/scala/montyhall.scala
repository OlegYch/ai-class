object montyhall extends App {
  val choices = 0 until 3
  val samples = 0 to 10000

  def choose[T](s: Seq[T]): T = {
    s(util.Random.nextInt(s.size))
  }

  val cars = samples.map(_ => choose(choices))
  val firstChoice = samples.map(_ => choose(choices))
  val secondChoice = cars.zip(firstChoice).map {
    case (car, choice) =>
      val chosenByMonty = if (car == choice) {
        Some(choose(choices.toList - car - choice))
      } else {
        None
      }
      //      val chosenByMonty = choose(choices.toList - car - choice)
      val remaining = choices.toList -- chosenByMonty.toList - choice
      choose(remaining)
  }

  def wins(choice: IndexedSeq[Int]) = cars.zip(choice).collect {
    case (a, b) if (a == b) => a
  }.size

  println(wins(firstChoice))
  println(wins(secondChoice))
}