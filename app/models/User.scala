package models

import play.api.libs.json.Json


case class User (name: String, password: String){
  def checkPassword(password: String): Boolean = this.password == password
}

object User {

  implicit val userFormatter = Json.format[User]


  def find(username: String):Option[User] =DB.query[User].whereEqual("name",username).fetchOne()
}