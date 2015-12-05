package models

import play.api.libs.json.Json

/**
  * Created by piotr on 03.12.2015.
  */
case class GeocachData(name: String, description: String,address: String,long: Double,lat: Double,radius: Int) {

}

object GeocachData{
  implicit val geocachFormatter = Json.format[GeocachData]
}