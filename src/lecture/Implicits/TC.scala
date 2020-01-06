package lecture.Implicits

object TC {

  ////// option 1 - simple trait

  // (class) describes a collection of methods and properties sth
  // must have to belong to given class
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name</div>"
  }

  // - works only for our types
  // - only one impl
  User("John", 32, "esapl@.com").toHtml

  ////// option 2 - pattern matching
  object HTMLSerializerObj {
    def serializeToHtml(value: Any) = value match {
      case User(n, a, e) => s"<div>$n</div>"
      case _ => "none"
    }
  }

  // - lost type safety
  // - need to modify the code every time
  // - still one impl

  ////// option 3 - parametrized trait (typeclass)

  // (typeclass) describes a set of properties a type must have to
  // belong to given typeclass
  trait TypeClass[T] {
    def action(a: T, b: T): Boolean
  }
  
  object TypeClass {
    def apply[T](implicit instance: TypeClass[T]) = instance
  }

  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  // can define serializer for other types
  // different impl could be passed implicitly (ad-hoc polymorphism)

  // typeclass instances
  implicit object UserSerializer extends HTMLSerializer[User] {
    override def serialize(value: User): String = s"<div>${value.name}</div>"
  }

  import java.util.Date
  implicit object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(value: Date): String = s"<div>${value.toString}</div>"
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div>$value</div>"
  }

  // IntSerializer is passed implicitly
  HTMLSerializer.serialize(42)
  HTMLSerializer.serialize(User("John", 23, "kek"))
  HTMLSerializer.serialize(new Date())

  // access to the entire typeclass interface (due to apply method)
  HTMLSerializer[Int].serialize(42)


  // typeclasses with type enrichments
  implicit class HTMLEnrichment[T](value: T) {
    def toHtml(implicit serializer: HTMLSerializer[T]): String  = serializer.serialize(value)
  }

  // new HTMLEnrichment[User](john).toHtml(UserSerializer)
  User("John", 23, "kek").toHtml

  // extend to new types
  // choose your implementation based on type
  // expressive

  // typeclass -> typeclass instance (implicit) -> class conversion (implicit)

  // context bounds:

  // def f[A : B](a: A) = g(a)
  // there must by B instance for A
  // g requires an implicit value of type B[A]

  // htmlBoilerplate :: (HTMLSerializer T) => T -> String
  def htmlBoilerplate[T](content: T)(implicit serializer: HTMLSerializer[T]): String = {
    s"<html> ${content.toHtml(serializer)}</html>"
  }
  def htmlBoilerplate2[T : HTMLSerializer](content: T) = s"<html> ${content.toHtml}</html>"

  // implicitly
  case class Permissions(mask: String)
  implicit val defaultPermission: Permissions = Permissions("0744")

  // use implicit val as a normal val
  val standardPerms = implicitly[Permissions]
}

object TypeEnrichment {
  // decorate existing classes (to which we may not have access to)
  // with additional methods and attributes
  // inn this case only one implicit search is made

  // val and AnyVal due to optimizations
  implicit class RichInt(val value: Int) extends AnyVal {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
  }

  implicit class RichString(val value: String) extends AnyVal {
    def asInt: Int = Integer.valueOf(value)
    def encrypt(distance: Int): String = value.map(c => {
      val x = (c + distance) // implicit conversion
      x.asInstanceOf[Char]
    })
  }

  new RichInt(42).sqrt

  // used as RichInt
  42.isEven

  import scala.concurrent.duration._
  3.seconds

  // implicit conversions as methods, discouraged
  implicit def stringToInt(string: String): Int = Integer.valueOf(string)
  val res = "6" / 2

  // eq of implicit class
  class RichAltInt(value: Int)
  implicit def enrich(value: Int): RichAltInt = new RichAltInt(value)

}
