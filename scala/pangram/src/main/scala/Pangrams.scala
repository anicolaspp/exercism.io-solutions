object Pangrams {
  def isPangram(input: String): Boolean = {
  	def check(s: String) = ('a' to 'z').forall(c => s.contains(c))

  	check(input.toLowerCase)
  }
}

