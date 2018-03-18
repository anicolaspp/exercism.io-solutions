import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAll}


val numbers = Gen.choose(2, 1000)

val sumChecker = forAll(numbers, numbers) { (a: Int, b: Int) =>

  a + b <= a * b

}

sumChecker.check


def myConcat(a: String, b: String) = "hello" + a + b


val concat = forAll { (a: String, b: String) =>
  myConcat(a,b).startsWith("hello" + a) && myConcat(a, b).endsWith(b)
}
concat.check


