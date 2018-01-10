class Operations {

  // Area of Shapes
  def areaOfQuadrilateral(shape: String, base: Int, height: Int, f: (Int, Int) => Int): Int = {
    if (shape.toLowerCase == "square" || shape.toLowerCase == "rectangle" || shape.toLowerCase == "rhombus") {
      f(base, height)
    }
    else {
      -1
    }
  }

  // Multiply all list elements with 2
  def doubleListElements(list: List[Int]): List[Int] = {
    list.map(x => 2 * x)
  }

  // Reverse a list
  def reverseList(list: List[Int]): List[Int] = {
    val vector = for (i <- list.length - 1 until -1 by -1) yield list(i)
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
  def kthElementOfList(k: Int, list: List[Int], count: Int = 1): Int = {
    list match {
      case head :: tail => {
        if (k != count) {
          kthElementOfList(k, tail, count + 1)
        }
        else {
          list(count - 1)
        }
      }
    }
  }

  // Adding two lists
  def addTwoList(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list1.length == list2.length) {
      val vector = for (i <- 0 until list1.length) yield list1(i) + list2(i)
      vector.toList
    }
    else {
      List()
    }
  }

}

object Operations extends App {
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

  // Two list addition method call
  val addedElementsOfList = obj.addTwoList(list1, list2)
  if (addedElementsOfList.length > 0) {
    print(addedElementsOfList)
  }
  else {
    print("Both list were unequal in length, so can't be added ")
  }
  print("\n")


  // nth fibonacci
  val n = 10
  val nthFibonacci = obj.fibonacci(n)
  print(nthFibonacci)
  print("\n")


  // Higher order function call
  val base = 4
  val height = 5
  val shape = "rectangle"
  val area = obj.areaOfQuadrilateral(shape, base, height, (base, height) => base * height)
  if (area != -1) {
    print(s"Area of $shape is $area")
  }
  else {
    print(s"Not yet defined for $shape")
  }
  print("\n")


  // method call for Doubling numbers of list
  val doubledList = obj.doubleListElements(list1)
  print(doubledList)
  print("\n")


  // Method call to find kth element of list
  val k = 2
  val kthElement = obj.kthElementOfList(k, list1)
  print(s"$k th Element of list is $kthElement")
  print("\n")


  // Method call for counting number of elements of list
  val numberOfElements = obj.numberOfElementsInList(list1)
  print(numberOfElements)
  print("\n")


  // Method call for reversing a list
  val reverseList = obj.reverseList(list1)
  print(reverseList)
  print("\n")


}
