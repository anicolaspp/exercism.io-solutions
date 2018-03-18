object PerfectNumbers { 

	def classify(n: Int): Either[String,  NumberType] = 
		if (n < 0)
			Left("Classification is only possible for natural numbers.")
		else if (n == 0) 
			Left("Classification is only possible for natural numbers.")
		else
			Right(clasifySum((1 until n).filter(i => n % i == 0).sum, n))

	def clasifySum(sum: Int, n: Int) = if (sum == n) NumberType.Perfect else if (sum < n) NumberType.Deficient else NumberType.Abundant

}

sealed trait NumberType

object NumberType {

case object Perfect extends NumberType
case object Abundant extends NumberType
case object Deficient extends NumberType
}


