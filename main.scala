import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    
    val padding = 5
    val lines = Source.fromFile("Input.txt").getLines.toList
    val imageEnhancer = toBoolArray(lines.head)

    var imageArray = toEmptyArrays(110, padding) ++ lines.drop(2).map(toBoolArray(_, 5)) ++ toEmptyArrays(110, padding)
    
    println(s"Lines count: ${lines.size}")

    println(s"Image enhancer length: ${imageEnhancer.length}")

    println(s"Image array length: ${imageArray.length}")
  }

  def toBoolArray(line: String, padding: Int = 0): Array[Boolean] = {
      val arrayLength = line.length() + 2 * padding
      var result = Array.fill(arrayLength)(false)
      

      result
  }

  def toEmptyArrays(length: Int, numArrays: Int): List[Array[Boolean]] = {
      val is: IndexedSeq[Array[Boolean]] = (1 to numArrays).map(_ => Array.fill(length)(false))
      is.toList
  }
}
