import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
    
        val padding = 5
        val lines = Source.fromFile("Input.txt").getLines.toList
        val imageEnhancer = lines.head.map(_ == '#').toList

        println(s"Lines count: ${lines.size}")

        println(s"Image enhancer length: ${imageEnhancer.length}")


        var imageBoard = new ImageBoard(100, padding);
        println(s"Image board total width: ${imageBoard.totalWidth}")

        imageBoard.setLines(lines.drop(2))

        imageBoard.print()
    }
}

class ImageBoard(var size: Int, var padding: Int) {
    val totalWidth = size + 2 * padding
    var imageArray = toEmptyArrays(totalWidth, totalWidth).map(_ => Array.fill(totalWidth)(false)).toArray

    def setValue(v: Boolean, x: Int, y: Int) = {
        imageArray(toReal(y))(toReal(x)) = v
    }

    def getValue(x: Int, y: Int): Boolean = {
        imageArray(toReal(y))(toReal(x))
    }

    def getEnhancementNum(x: Int, y: Int): Int = {
        val bools = List((x-1, y-1), (x, y-1), (x+1, y-1), 
            (x-1, y), (x, y), (x+1, y), 
            (x-1, y+1), (x, y+1), (x+1, y+1)).map(t => getValue(t._1, t._2))

        bools.foldLeft(0) {(agg, v) => agg * 2 + (if (v) 1 else 0)}
    }

    private def toReal(coord: Int): Int = coord + padding

    def setLines(lines: List[String]) = {
        var y = 0;
        for (line <- lines) {
            var x = 0;
            for (b <- line.map(_ == '#')) {
                setValue(b, x, y)
                x += 1
            }
            
            y += 1
        }
    }

    def toEmptyArrays(length: Int, numArrays: Int): List[Array[Boolean]] = {
        (1 to numArrays).map(_ => Array.fill(length)(false)).toList
    }

    def print() {
        for (row <- imageArray) {
            val symbols = row.map(if (_) '#' else '.').mkString
                println(symbols)
        }
    }
}
