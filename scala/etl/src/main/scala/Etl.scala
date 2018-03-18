object Etl {
	def transform(m: Map[Int, Seq[String]]): Map[String, Int] =
		m.flatMap { case (k, lv) => lv.map(_.toLowerCase -> k) }


	def isIsogram(s: String) =
		s
			.toCharArray
			.map(c => (c, 1))
  		.groupBy(_._1)
    	.map { case (v, lv) => (v, lv.map(_._2).sum) }
			.filter(_._1 isLetter)
			.forall(_._2 == 1)

}