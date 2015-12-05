package controllers

import models.User
import play.api.mvc.RequestHeader

/**
  * Created by piotr on 03.12.2015.
  */

object AuthUtils {


  def parseUserFromCookie(implicit request: RequestHeader) = request.session.get("username").flatMap(username => User.find(username))

  def parseUserFromQueryString(implicit request:RequestHeader) = {

    val query = request.queryString.map { case (k, v) => k -> v.mkString }

    val username = query get ("username")

    val password = query get ("password")

    (username, password) match {

      case (Some(u), Some(p)) => User.find(u).filter(user => user.checkPassword(p))

      case _ => None

    }

  }
  def parseUserFromRequest(implicit request:RequestHeader):Option[User] = {

    parseUserFromQueryString orElse parseUserFromCookie

  }


}

