
object dependency {
  def validateName(s: String): Option[String] = ???
  def validateAge(n: Int): Option[Int] = ???
  def validateEmail(s: String): Option[String] = ???
  case class Person(name: String, age: Int, email: String)

  def validatePerson(n: String, a: Int, e: String): Option[Person] = 
   validateName(n).flatMap(name =>
   validateAge(a).flatMap(age =>
   validateEmail(e).map(email => 
    Person(name, age, email)
   )))

  def validatePerson(n: String, a: Int, e: String): Option[Person] = 
      val name = validateName(n)
      val age = validateAge(a)
      val email = validateEmail(e)
      if (name != null && age != null && )_

}
