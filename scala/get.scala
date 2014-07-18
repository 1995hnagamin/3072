import scala.io.Source
import scala.util.parsing.json.JSON

object Main {
  def main(args : Array[String]) : Unit = {

    val json = Source.fromURL(
      "http://2048.semantics3.com/hi/start/json","utf8")

    println(json.getLines.mkString)
  }
}
