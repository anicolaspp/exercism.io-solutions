object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = factors.flatMap(factor => generate(factor, limit)).sum

  def generate(factor: Int, limit: Int): List[Int] = {

  	def gen(value: Int, i: Int, xs: List[Int]): List[Int] =
			if (value >= limit)
				xs
			else
				gen(factor * i, i + 1, value :: xs)

  	gen(factor, 1, List.empty)
  }
}

