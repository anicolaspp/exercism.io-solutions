object Strain {

	def keep[A](xs: Seq[A], f: A => Boolean): List[A] = xs.toList match {
		case Nil	=>	Nil
		case h :: t =>	if (f(h)) h :: keep(t, f) else keep(t, f)
	}

	def discard[A](xs: Seq[A], f: A => Boolean): List[A] = xs.toList match {
		case Nil 	=>	Nil
		case h :: t =>	if (f(h)) discard(t, f) else h :: discard(t, f)
	}

}