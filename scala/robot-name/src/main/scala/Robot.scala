import scala.collection.mutable
import scala.util.Random

class Robot() {

  private var roboName = NameGenerator.newName()

  def name: String = roboName

  def reset(): Unit = {
    NameGenerator.putNameBack(roboName)

    roboName = NameGenerator.newName()
  }
}

object NameGenerator {

  private def genNames() = for {
    a <- 'A' to 'Z'
    b <- 'A' to 'Z'
    c <- '0' to '9'
    d <- '0' to '9'
    e <- '0' to '9'
  } yield a.toString + b + c + d + e

  private val pool =
    genNames()
      .par
      .foldLeft(collection.mutable.Queue.empty[String])((queue, name) => {
        queue.enqueue(name)
        queue
      })
  
  def putNameBack(name: String): Unit = pool += name

  def newName(): String = {
    val name = pool.dequeue()

    name
  }
}