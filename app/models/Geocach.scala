package models

/**
  * Created by piotr on 03.12.2015.
  */
import play.api.libs.json.Json

import scala.collection.mutable.ListBuffer


case class Geocach (name: String, description: String,address: String,long: Double,lat: Double,radius: Int,author: User,visitors: Set[User]){

}

object Geocach {

  implicit val geocachFormatter = Json.format[Geocach]

}