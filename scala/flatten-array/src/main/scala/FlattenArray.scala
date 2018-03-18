


object FlattenArray {
  def flatten(list: List[Any]) = flat(list, List.empty).reverse
  
  def flat(xs: List[Any], result: List[Any]): List[Any] = xs match {
    case Nil => result
    case h :: tail => h match {
      case null             => flat(tail, result)
      case list: List[Any]  => flat(tail, flat(list, List.empty) ++ result)
      case _                => flat(tail, h :: result)
    }
  }
}
