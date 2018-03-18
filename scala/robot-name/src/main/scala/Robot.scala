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
    c <- 100 to 999
  } yield a.toString + b + c
  
  private val pool =
  genNames()
    .foldLeft(collection.mutable.HashSet.empty[String])((set, name) => {
        set.add(name)
        set
      })

  def putNameBack(name: String): Unit = pool.add(name)

  def newName(): String = {
    val name = pool.head

    pool.remove(name)

    name
  }
}