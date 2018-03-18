object BracketPush {
	def isPaired(s: String) = check(s.toCharArray.toList)

	def check(s: List[Char]) = {

		def checkPairs(a: Int, b: Int, c: Int, s: List[Char]): Boolean = s match {
			case '[' :: Nil => sum(a + 1, b, c)
			case ']' :: Nil => sum(a - 1, b, c)
			case '{' :: Nil => sum(a, b + 1, c)
			case '}' :: Nil => sum(a, b - 1, c)
			case '(' :: Nil => sum(a, b, c + 1)
			case ')' :: Nil => sum(a, b, c - 1)


			case '[' :: t => checkPairs(a + 1, b, c, t)
			case ']' :: t => checkPairs(a - 1, b, c, t)
			case '{' :: t => checkPairs(a, b + 1, c, t)
			case '}' :: t => checkPairs(a, b - 1, c, t)
			case '(' :: t => checkPairs(a, b, c + 1, t)
			case ')' :: t => checkPairs(a, b, c - 1, t)				

		}

		checkPairs(0, 0, 0, s)
	}

	def sum(a: Int, b: Int, c: Int) = (a + b + c) == 0
}