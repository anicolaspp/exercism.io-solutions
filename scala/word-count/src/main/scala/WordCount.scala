// WordCount("word").countwords

class WordCounter(s: String) {
	def countwords(): Map[String, Int] =
		s.split("\\s|,\\R".r.regex).map(w => (w, 1)).groupBy(_._1).map { case(key, vl) =>
			(key, vl.map(_._2).sum)
	}
}


object WordCount {
	def apply(s: String) = new WordCounter(s)
}