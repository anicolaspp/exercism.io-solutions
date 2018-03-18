object Isogram {
  def isIsogram(s: String) =
    s
      .toLowerCase
      .toCharArray
      .filter(_.isLetter)
      .distinct
      .length == s.toCharArray.filter(_.isLetter).length

}