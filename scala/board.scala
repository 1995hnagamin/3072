object Board {

type Row = Array[Int]
type Board = Array[Row]

def padding_left(r: Row): Row = {
  return if (r.size == 0) Array(0, 0, 0, 0)
  else if   (r.size == 1) Array(r(0), 0, 0, 0)
  else if   (r.size == 2) Array(r(0), r(1), 0, 0)
  else if   (r.size == 3) Array(r(0), r(1), r(2), 0)
  else r
}

def padding_right(r: Row): Row = {
  return if (r.size == 0) Array(0, 0, 0, 0)
  else if   (r.size == 1) Array(0, 0, 0, r(0))
  else if   (r.size == 2) Array(0, 0, r(0), r(1))
  else if   (r.size == 3) Array(0, r(0), r(1), r(2))
  else r
}

def fold(r: Row, padding: (Row) => Row): Row = {
  var row: Row = r.filter(_ != 0)
  return padding(
    if (row.size <= 1)
      row
    
    else if (row.size == 2)
      if (row(0) == row(1)) Array(2 * row(0))
      else row
  
    else if (row.size == 3)
      if (row(0) == row(1)) Array(2 * row(0), row(2))
      else if (row(1) == row(2)) Array(row(0), 2 * row(1))
      else row

    else
      if ((row(0) == row(1)) && (row(2) == row(3)))
        Array(2 * row(0), 2 * row(2))
      else if (row(0) == row(1))
        Array(2 * row(0), row(2), row(3))
      else if (row(1) == row(2))
        Array(row(0), 2 * row(1), row(3))
      else if (row(2) == row(3))
        Array(row(0), row(1), 2 * row(2))
      else
        row
      )
}

def move_left(b: Board): Board = {
  return b.map(fold(_, padding_left))
}

def move_right(b: Board): Board = {
  return b.map(fold(_, padding_right))
}

def move_up(b: Board): Board = {
  return move_left(b.transpose).transpose
}

def move_down(b: Board): Board = {
  return move_right(b.transpose).transpose
}

def print_row(row: Row): Unit = {
  println(s"${row(0)}\t${row(1)}\t${row(2)}\t${row(3)}")
}

def print(b: Board): Unit = {
  b.foreach(print_row)
  println("")
}

def void_places(b: Board): Array[Array[Int]] = {
  var ans: Array[Array[Int]] = Array()
  
  for (i <- 0 to 3)
    for (j <- 0 to 3)
      if (b(i)(j) == 0)
        ans = ans ++ Array(Array(i, j))
      else false

  return ans
}

def put_number(b: Board, num: Int): Board = {
  var space = void_places(b)
  return b
}

def main(args: Array[String]): Unit = {
  var row: Row = Array(1, 1, 2, 0)
  var b: Board = Array(row, row.reverse, row, row.reverse)
  print(b)
  print(move_left(b))
  print(move_right(b))
  print(move_up(b))
  print(move_down(b))

  void_places(b).foreach { pair =>
    println(s"(${pair(0)} ${pair(1)})")
  }
}

}
