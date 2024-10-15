import scala.util.Random

/**
  * Created by alvaro on 13/07/17.
  */
object PeopleRandomlyGivingDollars extends App{


  val n = 45
  val initialDollars = 100
  val step = 5
  val ticks = 500000

  val random = new Random
  val people = Array.tabulate(n)( _ => initialDollars)

  def tick( p : Array[Int] ) : Array[Int] = {

    def randomNotEqualTo(i: Int) : Int = random.nextInt(n) match{
      case r if( r != i ) => r
      case i => randomNotEqualTo(i)
    }

    val deltas = for(i <- 0 until n if(p(i)>= step)) yield {
      val dest = randomNotEqualTo(i)
      dest
    }

    val given = p.map( d => d - step max 0)

    val ret = deltas.foldLeft(given) { (p, index) =>
      p(index) += step
      p
    }

    assert( ret.sum == n*initialDollars)

    println( p.sorted.mkString("\t"))

    ret
  }


  (1 to ticks).foldLeft(people)((p,_) => tick(p))
}
