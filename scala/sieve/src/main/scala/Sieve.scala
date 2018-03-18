object Sieve {
  
  def primes(n: Int) = from(2, n, (2 to n).toList)

  private def from(k: Int, to: Int, result: List[Int]): List[Int] =
    if (k > to) {
      result
    } else {
      result.filterNot(v => v > k && v % k == 0) match {
        case Nil    =>  Nil
        case primes =>  primes.find(_ > k) match {
          case Some(value)  =>  from(value, to, primes)
          case None         =>  primes
        }
      }
    }

}
