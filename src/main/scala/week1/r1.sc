abstract class JSON

case class JSeq (elems: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNum (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool (b: Boolean) extends JSON
case object JNull extends JSON


val json = JObj(Map(
  "firstName" -> JStr("lastname"),
  "nums" -> JSeq(List(JNum(1), JNum(2), JNull)),
))

def show(json: JSON): String = json match {
  case JSeq(elems) =>
    "[" + (elems map show  mkString ", ") + "]"
  case JObj(bindings) =>
    val assocs = bindings map {
      case (key, value) => "\"" + key + "\": " + show(value)
    }
    "{" + (assocs mkString ", ") + "}"
  case JNum(num) => num.toString
  case JStr(str) => '\"' + str + '\"'
  case JBool(bool) => bool.toString
  case JNull => "null"
}

println(show(json))


val f: String => String = { case "ping" => "pong"}
println(f("ping"))
println(f("pong")) // no compiler error thrown (will crash at runtime ofc)

val f2: PartialFunction[String, String] = { case "ping" => "pong" }
f2("ping")
f2.isDefinedAt("pong")



val list = List(List(1, 2, 3), List(4, 5, 6))
list flatMap (sublist => sublist flatMap (n => (1 to n).toList))
for {
  subL <- list
  n <- subL
  i <- 1 to n
} yield i



val jsonList = List(JNull, json)
for {
  JObj(bindings) <- jsonList
  JSeq(nums) = bindings("nums")
  JNum(n) <- nums
} yield n
