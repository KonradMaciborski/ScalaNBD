import sun.security.ec.point.ProjectivePoint.Mutable

import scala.annotation.tailrec


object Main extends App {

  println("Zadanie 1")

  var dniTygodnia = List[String](
    "Poniedziałek",
           "Wtorek",
           "Sroda",
           "Czwartek",
           "Piątek",
           "Sobota",
           "Niedziela"
  )

  println("Zadanie 1a\n")

  var napisDniTygodnia = ""

  for(dzien <- dniTygodnia) {

    napisDniTygodnia += dzien

    if(dniTygodnia.last != dzien){
      napisDniTygodnia += ", "
    }

  }

  println(napisDniTygodnia)


  println("\nZadanie 1b\n")

  napisDniTygodnia = ""

  for(dzien <- dniTygodnia) {

    if(dzien.toLowerCase().startsWith("p")) {
      napisDniTygodnia += dzien + ", "
    }

  }
  napisDniTygodnia = napisDniTygodnia.dropRight(2)

  println(napisDniTygodnia)


  println("\nZadanie 1c\n")

  napisDniTygodnia = ""

  {
    var i = 0
    while(i < dniTygodnia.length){
        napisDniTygodnia += dniTygodnia(i) + ", "
        i += 1
    }
  }
  napisDniTygodnia = napisDniTygodnia.dropRight(2)

  println(napisDniTygodnia)


  println("\nZadanie 2")
  println("Zadanie 2a\n")

  napisDniTygodnia = ""

  def createStringWithCommasFromList[T](list: List[T]): String = list match {
    case Nil => ""
    case _ => list.head + ", " + createStringWithCommasFromList(list.tail)
    }


  napisDniTygodnia = createStringWithCommasFromList(dniTygodnia).dropRight(2)
  println(napisDniTygodnia)


  println("\nZadanie 2b\n")

  napisDniTygodnia = ""

  def createStringWithCommasFromListDesc[T](list: List[T]): String = list match {

    case Nil => ""
    case _ => createStringWithCommasFromListDesc(list.tail) + ", " + list.head

  }

  napisDniTygodnia = createStringWithCommasFromListDesc(dniTygodnia).drop(2)
  println(napisDniTygodnia)


  println("\nZadanie 3\n")

  napisDniTygodnia = ""

  def createStringWithCommasFromListTail[T](list: List[T]): String = {

    @tailrec
    def appendStr[T](list: List[T], str: String): String = list match {
        case Nil => str.dropRight(2)
        case head :: tail => appendStr(tail, str + head + ", ")
      }
    appendStr(list, "")
  }

  println(createStringWithCommasFromListTail(dniTygodnia))


  println("\nZadanie 4")
  println("Zadanie 4a\n")

  def createStringWithCommasFromListFoldl(list: List[String]): String = {
    list.foldLeft(""){ (acc, item) =>
      if(acc.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFromListFoldl(dniTygodnia))


  println("\nZadanie 4b\n")

  def createStringWithCommasFromListFoldr(list: List[String]): String = {
    list.foldRight(""){ (acc, item) =>
      if(item.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFromListFoldr(dniTygodnia))


  println("\nZadanie 4c\n")

  def createStringWithCommasFromListFoldlOnlyP(list: List[String]): String = {
    list.filter(_.toLowerCase()
                 .startsWith("p")
               ) .foldLeft(""){ (acc, item) =>
      if(acc.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFromListFoldlOnlyP(dniTygodnia))



  println("\nZadanie 5\n")

  val produktCena = Map (
    "Owoce" -> 20d,
    "Warzywa" -> 15d,
    "Owoce Morza" -> 25d,
    "Mięso" -> 30d
  )
  println("Przed zmianą:")
  for(i <- produktCena) println(i._1 + " - " + i._2)
  println()

  val produktCena10 = produktCena.transform((_, v) => v * 0.9)
  println("Przed zmianie:")
  for(i <- produktCena10) println(i._1 + " - " + i._2)


  println("\nZadanie 6\n")

  val tuple1 = ("Konrad", 25, 114.2)
  val tuple2 = ('k', Math.PI, true)

  def printTuple[A, B, C](tup: (A, B, C)): Unit = {
    println(tup._1 + " - " + tup._2 + " - " + tup._3)
  }

  printTuple(tuple1)
  printTuple(tuple2)


  println("\nZadanie 7\n")

  val texts = List[String] ("1", "jeden", "2", "dwa")

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case _: Exception => None
    }
  }

  for(n <- texts) println(toInt(n))


  println("\nZadanie 8\n")

  def returnListWithNoZero(list: List[Int]): List[Int] = {

    @tailrec
    def accNewList(list: List[Int], listR: List[Int]): List[Int] = list match{
      case Nil => listR
      case head :: tail => {
        if(head == 0) accNewList(tail, listR)
        else accNewList(tail, listR.appended(head))
      }
    }
    accNewList(list, List.empty[Int])
  }

  var noZeros = ""
  for(wrt <- returnListWithNoZero(List[Int] (1, 2, 3, 0, 4, 0, 1))){
    noZeros += wrt + ", "
  }
  println(noZeros.dropRight(2))


  println("\nZadanie 9\n")

  def increaseEveryElemByOne(list: List[Int]) = list.map(x => x + 1)

  var list = List[Int] (1, 2, 3, 4, 5, 6, 7)
  println("Lista przed inkrementacją: " + list)
  list = increaseEveryElemByOne(list)
  println("Lista po inkrementacji: " + list)


  println("\nZadanie 10\n")

  val realNumbers = List[Double](Math.PI, -Math.E, 5.5, 10.2, 15.9, 2, 4, 7, 8.8, 21.37, -2.6, -7.7, -5)

  def returnListOfAbsValInRange(list: List[Double], r1: Int, r2: Int): List[Double] = {
    list.filter(x => x >= r1)
        .filter(x => x <= r2)
        .map(x => x.abs)
  }

  println("Lista przed filtracją: " + realNumbers)
  println("Lista po filtracji: " + returnListOfAbsValInRange(realNumbers, -5, 12))


}
