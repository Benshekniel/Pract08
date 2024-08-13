object Multiple {
  val numberDivide: Int => Unit = (number => {
    if (number % 3 == 0) {
      if (number % 5 == 0) {
        println("Multiple of Both Three and Five")
      } else {
        println("Multiple of Three")
      }
    } else {
      if (number % 5 == 0) {
        println("Multiple of Five")
      } else {
        println("Not a Multiple of Three or Five")
      }
    }
  })

  def main(args: Array[String]): Unit = {
    print("Enter the number you want to test: ")
    val num = scala.io.StdIn.readInt()
    Multiple.numberDivide(num)
  }
}


