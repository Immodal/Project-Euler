import scala.io.Source

object P11 {
  val dataPath = "data/p11/"

  /**
    * Retrieve the matrix from txt file
    */
  def getGrid(path: String): List[List[Long]] = {
    Source.fromFile(path)
      .getLines()
      .map(_.split(" ").toList.map(_.toLong))
      .toList
  }

  /**
    * Slice grid in any direction on a 2D matrix
    */
  def slice(n: Int, grid: List[List[Long]], startI: Int,  iInc: Int, startJ: Int, jInc: Int): List[Long] = {
    def go(i: Int, j: Int, acc: List[Long]): List[Long] = {
      if(acc.length==n) acc
      else go(i+iInc, j+jInc, grid(i)(j) +: acc)
    }

    val iFinal = startI + ((n-1) * iInc)
    val jFinal = startJ + ((n-1) * jInc)
    // If the end of the slice is out of bounds, return an empty list
    if (iFinal>=0 && iFinal<grid.length && jFinal>=0 && jFinal<grid(startI).length) 
      go(startI, startJ, List())
    else List()
  }

  /**
    * Get largest product for slices of grid in directions given by iInc and jInc
    */
  def prod(n: Int, grid: List[List[Long]], iInc: Int, jInc: Int): Long = {
    grid
      .zipWithIndex.map { // Produces the tuple (row, index)
        case (row, i) => row.zipWithIndex.map { // Produces the tuple (value, index)
          case (value, j) => slice(n, grid, i, iInc, j, jInc) // Slice from [i,j] in [iInc, jInc] direction
        } 
      }
      .map(_.filter(l => l.length>0)) // Filter out invalid slices from rows (they are List())
      .filter(_.length>0) // Filter out empty rows
      .flatMap(_.map(a => a.product)) // Get the product of every remaining list and flatten
      .max
  }

  def main(args: Array[String]) {
    // Assuming input is "euler.txt 4"
    val n = args(1).toInt
    val grid = getGrid(dataPath+args(0))
    val paProd = prod(n, grid, _, _) // Partial application

    println(List(paProd(0, 1), paProd(1, 0), paProd(1, 1), paProd(1, -1)).max)
  }
}