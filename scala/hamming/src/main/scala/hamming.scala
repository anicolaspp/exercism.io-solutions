object Hamming {
	def distance(a: String, b: String): Option[Int] = if (a.length != b.length) None else countDistance(a.toCharArray.toList, b.toCharArray.toList, 0)

	def countDistance(a: List[Char], b: List[Char], count: Int): Option[Int] = (a, b) match {
		case (Nil, Nil)				=>	Some(count)
		case (ah :: at, bh :: bt)	=>	if (ah == bh) countDistance(at, bt, count) else countDistance(at, bt, count + 1)		
	}
}