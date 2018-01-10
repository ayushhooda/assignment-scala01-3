class Operations {

  // Area of Shapes
  def areaOfQuadrilateral(shape: String, base: Int, height: Int, f: (Int, Int) => Int): Int = {
    f(base, height)
  }

  // Multiply all list elements with 2
  def doubleListElements(list: List[Int]): List[Int] = {
    list.map(x => 2 * x)
  }

  // Reverse a list
  def reverseList(list: List[Int]): List[Int] = {
    val vector = for(i <- list.length-1 until -1 by -1) yield list(i)
    vector.toList
  }

  // Nth fibonacci using tail recursion
  def fibonacci(n: Int, firstElement: Int = 0, secondElement: Int = 1): Int = {
    n match {
      case 0 => firstElement
      case 1 => secondElement
      case _ => fibonacci(n - 1, secondElement, firstElement + secondElement)
    }
  }


  // Number of elements in list
  def numberOfElementsInList(list: List[Int], count: Int = 0): Int = {
    list match {
      case head :: tail => numberOfElementsInList(tail, count + 1)
      case _ => count
    }
  }

  // Finding kth element of list
  def kthElementOfList(k: Int, list: List[Int], count: Int = 0): Int = {

  }

  // Adding two lists
  def addTwoList(list1: List[Int], list2: List[Int]): List[Int] = {
    val vector = for(i <- 0 until list1.length) yield list1(i) + list2(i)
    vector.toList
  }

}

object Operations extends App{
  val obj = new Operations
  val list1Element1 = 1
  val list1Element2 = 2
  val list1Element3 = 3
  val list1Element4 = 4

  val list2Element1 = 4
  val list2Element2 = 3
  val list2Element3 = 2
  val list2Element4 = 1

  val list1 = List(list1Element1, list1Element2, list1Element3, list1Element4)
  val list2 = List(list2Element1, list2Element2, list2Element3, list2Element4)

  val doubledList = obj.doubleListElements(list1)
  print(doubledList)

  val addedElementsOfList = obj.addTwoList(list1,list2)
  print(addedElementsOfList)

  val n = 10
  val nthFibonacci = obj.fibonacci(n)
  println(nthFibonacci)

  val k = 2
  val kthElement = obj.kthElementOfList(k,list1)
  print(s"$k th Element of list is $kthElement")

  val reverseList = obj.reverseList(list1)
  print(reverseList)

  val numberOfElements = obj.numberOfElementsInList(list1)
  print(numberOfElements)
}