object Raindrops {
  def convert(n: Int): String = (n % 3, n % 5, n % 7) match {
  	 case (0, 0, 0)	=>	"PlingPlangPlong"
  	 case (0, 0, _) =>	"PlingPlang"
  	 case (0, _, 0) =>	"PlingPlong"
  	 case (_, 0, 0) =>	"PlangPlong"
  	 case (0, _, _) => 	"Pling"
  	 case (_, 0, _) => 	"Plang"	 	
  	 case (_, _, 0) => 	"Plong"
  	 case (_, _, _) =>	n.toString
  }
}

