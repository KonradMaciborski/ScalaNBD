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

  println("Zadanie 1a")

  var napisDniTygodnia = ""

  for(dzien <- dniTygodnia) {

    napisDniTygodnia += dzien

    if(dniTygodnia.last != dzien){
      napisDniTygodnia += ", "
    }

  }

  println(napisDniTygodnia)


  println("Zadanie 1b")

  napisDniTygodnia = ""

  for(dzien <- dniTygodnia) {

    if(dzien.toLowerCase().startsWith("p")) {
      napisDniTygodnia += dzien + ", "
    }

  }
  napisDniTygodnia = napisDniTygodnia.dropRight(2)

  println(napisDniTygodnia)


  println("Zadanie 1c")

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


  println("Zadanie 2")
  println("Zadanie 2a")

  napisDniTygodnia = ""

  def createStringWithCommasFromList[T](list: List[T]): String = list match {
    case Nil => ""
    case _ => list.head + ", " + createStringWithCommasFromList(list.tail)
    }


  napisDniTygodnia = createStringWithCommasFromList(dniTygodnia).dropRight(2)
  println(napisDniTygodnia)


  println("Zadanie 2b")

  napisDniTygodnia = ""

  def createStringWithCommasFromListDesc[T](list: List[T]): String = list match {

    case Nil => ""
    case _ => createStringWithCommasFromListDesc(list.tail) + ", " + list.head

  }

  napisDniTygodnia = createStringWithCommasFromListDesc(dniTygodnia).drop(2)
  println(napisDniTygodnia)


  println("Zadanie 3")

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


  println("Zadanie 4")
  println("Zadanie 4a")

  def createStringWithCommasFromListFoldl(list: List[String]): String = {
    list.foldLeft(""){ (acc, item) =>
      if(acc.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFromListFoldl(dniTygodnia))


  println("Zadanie 4b")

  def createStringWithCommasFromListFoldr(list: List[String]): String = {
    list.foldRight(""){ (acc, item) =>
      if(item.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFromListFoldr(dniTygodnia))


  println("Zadanie 4c")

  def createStringWithCommasFromListFoldlOnlyP(list: List[String]): String = {
    list.filter(_.toLowerCase()
                 .startsWith("p")).foldLeft(""){ (acc, item) =>
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

}
