import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    
    val lines = Source.fromFile("Input.txt").getLines.toList
    val imageEnhancer = lines.head


    println(s"Lines count: ${lines.size}")

    println(s"Image enhancer length: ${imageEnhancer.length()}")
  }
}
