import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
    
        val padding = 300
        var boardSize = 100
        val lines = Source.fromFile("Input.txt").getLines.toList
        val imageEnhancer = lines.head.map(_ == '#').toList

        println(s"Lines count: ${lines.size}")

        println(s"Image enhancer length: ${imageEnhancer.length}")


        var imageBoard = new ImageBoard(boardSize, padding, imageEnhancer)
        println(s"Image board total width: ${imageBoard.totalWidth}")

        imageBoard.setLines(lines.drop(2))
      
        // imageBoard.print()
        println()
        println(s"Count lit original = ${imageBoard.countLit()}")

        var curr = imageBoard

        for (generation <- 1 to 50) {
            val next = curr.nextGen()

            // next.print()
            // println()
            println(s"Count lit generation ${generation} board = ${next.countLitIgnoreBorder(100)}")
            println()

            curr = next
        }
    }
}

class ImageBoard(var size: Int, var padding: Int, val imageEnhancer: List[Boolean]) {
    val totalWidth = size + 2 * padding
    var imageArray = toEmptyArrays(totalWidth, totalWidth).map(_ => Array.fill(totalWidth)(false)).toArray

    def setValue(v: Boolean, x: Int, y: Int) = {
        setReal(v, toReal(x), toReal(y))
    }

    def setReal(v: Boolean, x: Int, y: Int) = {
        imageArray(y)(x) = v
    }

    def getValue(x: Int, y: Int): Boolean = {
        getReal(toReal(x), toReal(y))
    }

    def getReal(x: Int, y: Int): Boolean = {
        imageArray(y)(x)
    }

    def getEnhancementNum(xReal: Int, yReal: Int): Int = {
        val x = xReal
        val y = yReal
        val bools = List((x-1, y-1), (x, y-1), (x+1, y-1), 
            (x-1, y), (x, y), (x+1, y), 
            (x-1, y+1), (x, y+1), (x+1, y+1)).map(t => imageArray(t._2)(t._1))

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

    def countLit(): Int = {
        var count = 0
        for (row <- imageArray) {
            for (b <- row) {
                if (b) {
                    count += 1
                }
            }
        }

        count
    }

    def countLitIgnoreBorder(border: Int): Int = {
        
        val numToCheck = imageArray.length - 2 * border
        var count = 0
        for (row <- imageArray.drop(border).take(numToCheck)) {
            for (b <- row.drop(border).take(numToCheck)) {
                if (b) {
                    count += 1
                }
            }
        }

        count
    }

    def nextGen(): ImageBoard = {
        var newBoard = new ImageBoard(size, padding, imageEnhancer)
        
        for (yReal <- 1 until (totalWidth-1)) {
            for (xReal <- 1 until (totalWidth-1)) {
                // println(s"    (${x}, ${y})")

                val enhanceIndex = getEnhancementNum(xReal, yReal)
                var enhanced = imageEnhancer(enhanceIndex)

                newBoard.setReal(enhanced, xReal, yReal)
            }
        }

        newBoard
    }
}

// 27232 is too high
