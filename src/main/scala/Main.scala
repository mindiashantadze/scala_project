import scala.io.StdIn.readLine
import java.lang.Integer
import scala.collection.mutable.Stack

// Implement solution for counting letters in the string
def countLetters(string: String): Unit = {
  val str: String = string.replaceAll(" ", "").toLowerCase()
  var letterMap: Map[Char, Int] = Map[Char, Int]()

  for i <- 0 until str.length do
    val l: Char = str(i)
    letterMap =
      if !letterMap.contains(l) then letterMap + (l -> 1)
      else letterMap.updated(l, letterMap.apply(l).+(1))

  letterMap.foreach(pair => {
    println(pair._1 + " - " + pair._2)
  })
}

// Decode string
def expandString(str: String): Unit = {
  val counts: Stack[Int] = Stack[Int]()
  val result: Stack[String] = Stack[String]()
  var res: String = ""
  var index: Int = 0

  for char <- str.toCharArray do
    if char.isDigit then
      var count: Int = 0
      while str.charAt(index).isDigit do
        count = 10 * count + (str.charAt(index)- '0')
        index += 1
      counts.push(count)
    else if char.equals('[') then
      result.push(res)
      res = ""
      index += 1
    else if char.equals(']') then
      var temp: String = result.pop()
      val count: Int = counts.pop()

      for i <- (0 until count) do
        temp += res
        index += 1
      res = temp
    else
      res += (str.charAt(index))
      index += 1
  println(res)
}

// If you can divide a number from array without remain you replace that number with word
def remainWord (numList: List[Int]): Unit = {
  for num <- numList do
    if (num % 3 == 0) && (num % 5 != 0) then println ("aaa")
    else if (num % 5 == 0) && (num % 3 != 0) then println ("bbb")
    else if (num % 5 == 0) && (num % 3 == 0) then println ("ccc")
    else println (num)
}

@main def hello: Unit  =
  print("Enter a word: ")
  countLetters(readLine())

  println
  expandString("3[a2[b]]")

  println()
  remainWord(List(1, 2, 3, 4, 5, 6, 9, 15, 10, 12))