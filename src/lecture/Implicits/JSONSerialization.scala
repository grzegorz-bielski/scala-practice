package lecture.Implicits

import java.util.Date

object JSONSerialization extends App {
  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  //  1. intermediate data types
  //  2. type classes for conversion to intermediate data types
  //  3. serialize to JSON

  // 1. intermediate data types
  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    def stringify: String = "\"" + value + "\""
  }
  final case class JSONNumber(value: Double) extends JSONValue {
    def stringify: String = value.toString
  }
  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }
  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    def stringify: String = values.map {
      case (key, value) => "\"" + key + "\":" + value.stringify
    } mkString("{", ",", "}")
  }

  // 2.1. type class

  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }

  // 2.2. instances

  // type class instances (exisiting date types)
  implicit object StringConverter extends JSONConverter[String] {
    def convert(value: String): JSONValue = JSONString(value)
  }
  implicit object NumberConverter extends JSONConverter[Double] {
    def convert(value: Double): JSONValue = JSONNumber(value)
  }
  // custom data types
  implicit object UserConverter extends JSONConverter[User] {
    def convert(user: User): JSONValue = JSONObject(Map(
      "name" -> JSONString(user.name),
      "age" -> JSONNumber(user.age),
      "email" -> JSONString(user.email)
    ))
  }
  implicit object PostConverter extends JSONConverter[Post] {
    def convert(post: Post): JSONValue = JSONObject(Map(
      "content" -> JSONString(post.content),
      "created" -> JSONString(post.createdAt.toString)
    ))
  }

  implicit object FeedConverter extends JSONConverter[Feed] {
    def convert(feed: Feed): JSONValue = JSONObject(Map(
      "user" -> feed.user.toJSON,
      "posts" -> JSONArray(feed.posts.map(_.toJSON))
    ))
  }

  // 2.3 conversions
  implicit class JSONOps[T](value: T) {
    def toJSON(implicit converter: JSONConverter[T]): JSONValue = converter.convert(value)
  }

  //

  val now = new Date(System.currentTimeMillis())
  val john = User("john", 34, "john@example.com")
  val feed = Feed(john, List(
    Post("hello", now),
    Post("Hi", now)
  ))

  val jsonFeed = feed.toJSON.stringify

  println(jsonFeed)
}
