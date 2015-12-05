package controllers

import models.{GeocachData, Geocach, DB, User}
import play.api._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import play.api.data.format.Formats._

class Application extends Controller {

  def index = Action { request =>

    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        Ok(views.html.index(request.session.get("currentUser").get))
      }else{
        Ok(views.html.index(""))
      }
    }else{
      Ok(views.html.index(""))
    }

  }

  def addUserPage = Action{
    Ok(views.html.addUser("Login to app"){
      "Title"
    })
  }

  val userForm: Form[User] = Form {
    mapping(
      "name" -> nonEmptyText , "password" -> nonEmptyText
    )(User.apply)(User.unapply)
  }

  def addUser = Action { implicit request =>

    userForm.bindFromRequest().fold(
      formWithErrors => {
        Ok(views.html.error("ADDUSER_FORM_ERROR",""))
      },
      userData => {

        var userCheck = User.find(userData.name)

        userCheck match{
          case Some(user) =>
            Ok(views.html.error("USERNAME_EXIST_ERROR",""))
          case None =>
            val incomingUser = User(userData.name,userData.password)
            DB.save(incomingUser)
            Redirect(routes.Application.index()).withSession("currentUser" -> incomingUser.name,"password" -> incomingUser.password)
        }
      }
        )
  }

  def getUsers = Action { request =>
    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){

        val users = DB.query[User].fetch.toList
        val usersList = users.toList
        Ok(views.html.listUsers(usersList))


      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }

  }

  def inspectUser(userName: String) = Action {request =>

    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        var user = User.find(request.session.get("currentUser").get)

        user match{
          case Some(user) =>
            val inspectedUser = DB.query[User].whereEqual("name",userName).fetchOne()
            inspectedUser match{
              case Some(inspectedUser) =>
                Ok(views.html.inspectUser(inspectedUser,user))
              case None =>
                Ok(views.html.error("USER_NOT_EXIST_ERROR",""))
            }
          case None =>
            Ok(views.html.error("NOT_LOGGED_ERROR",""))
        }

      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }


  }



  def addGeocachPage = Action{request =>
    var responseText = ""

    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        responseText = request.session.get("currentUser").get
        Ok(views.html.addGeocach(responseText))
      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }


  }


  val geocachForm: Form[GeocachData] = Form {
    mapping(
      "name" -> nonEmptyText , "description" -> text,"address" -> text,"lat" -> of(doubleFormat),"long" -> of(doubleFormat),"radius" -> number
    )(GeocachData.apply)(GeocachData.unapply)
  }

  def addGeocach = Action { implicit request =>

    geocachForm.bindFromRequest().fold(

    formWithErrors => {
      Ok(views.html.error("ADDGEO_FORM_ERROR",""))
    },
      incomingData => {

        val testIfExists = DB.query[Geocach].whereEqual("name",incomingData.name).fetchOne()

        if(!testIfExists.isEmpty){
          Ok(views.html.error("GEONAME_EXIST_ERROR",""))
        }

        var geocachData = GeocachData(incomingData.name,incomingData.description,incomingData.address,incomingData.long,incomingData.lat,incomingData.radius)
        var user = User.find(request.session.get("currentUser").get)



        user match{
          case Some(user) =>

            var lat: Double = geocachData.lat
            var long: Double = geocachData.long

            if(lat <= 0 && long <=0 && !geocachData.address.isEmpty){
              val longLat = AddresUtils.fetchLatitudeAndLongitude(geocachData.address)

              if(!longLat.isEmpty){
                lat = longLat.get._1
                long = longLat.get._2
              }
            }

            var geo = Geocach(geocachData.name,geocachData.description,geocachData.address,long,lat,geocachData.radius,user,Set(user))
            DB.save(geo)
          case None =>
        }


        Redirect(routes.Application.index())
      }
    )


  }

  def getGeocachesCurrentUser = Action { request =>
    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        var user = User.find(request.session.get("currentUser").get)


        user match{
          case Some(user) =>
            val usersGeocahces = DB.query[Geocach].whereEqual("author",user).fetch()
            val usersGeocahcesList = usersGeocahces.toList
            Ok(views.html.listGeocach(usersGeocahcesList))
          case None =>
            Ok(views.html.index(request.session.get("currentUser").get))
        }

      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }

  }

  def getGeocachesAll = Action { request =>
    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){

            val usersGeocahces = DB.query[Geocach].fetch()
            val usersGeocahcesList = usersGeocahces.toList
            Ok(views.html.listGeocach(usersGeocahcesList))


      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }

  }

  def inspectGeocach(geocachName: String) = Action {request =>

    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        var user = User.find(request.session.get("currentUser").get)

        user match{
          case Some(user) =>
            val geocahce = DB.query[Geocach].whereEqual("name",geocachName).fetchOne()
            geocahce match{
              case Some(geocahce) =>
                Ok(views.html.inspectGeocach(geocahce,user))
              case None =>
                Ok(views.html.error("GEO_NOT_EXIST_ERROR",""))
                //Ok(views.html.index(request.session.get("currentUser").get))
            }
          case None =>
            Ok(views.html.error("NOT_LOGGED_ERROR",""))
        }

      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }


  }

  def deleteGeocach(geocachName: String) = Action { request =>
    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        var user = User.find(request.session.get("currentUser").get)

        user match{
          case Some(user) =>
            val geocahce = DB.query[Geocach].whereEqual("author",user).whereEqual("name",geocachName).fetchOne()
            geocahce match{
              case Some(geocahce) =>
                DB.delete[Geocach](geocahce)
                Ok(views.html.index(request.session.get("currentUser").get))
              case None =>
                Ok(views.html.index(request.session.get("currentUser").get))
            }
          case None =>
            Ok(views.html.error("NOT_LOGGED_ERROR",""))
        }

      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }

  }


  def markGeocachAsVisited(geocachName: String) = Action { request =>
    if(!(request.session.get("currentUser").isEmpty && request.session.get("password").isEmpty)){
      if(AuthenticateMe(request.session.get("currentUser").get,request.session.get("password").get)){
        var user = User.find(request.session.get("currentUser").get)

        user match{
          case Some(user) =>
            val geocahce = DB.query[Geocach].whereNotEqual("author",user).whereEqual("name",geocachName).fetchOne()
            geocahce match{
              case Some(geocahce) =>
                DB.delete[Geocach](geocahce)

                val geocachAfterMod = Geocach(geocahce.name,geocahce.description,geocahce.address,geocahce.long,geocahce.lat,geocahce.radius,geocahce.author,geocahce.visitors+user)

                DB.save(geocachAfterMod)

                Ok(views.html.index(request.session.get("currentUser").get))
              case None =>
                Ok(views.html.index(request.session.get("currentUser").get))
            }
          case None =>
            Ok(views.html.error("NOT_LOGGED_ERROR",""))
        }

      }else{
        Ok(views.html.error("NOT_LOGGED_ERROR",""))
      }
    }else{
      Ok(views.html.error("NOT_LOGGED_ERROR",""))
    }
  }




  def loginPage = Action{
    Ok(views.html.login("Login to app"){
      "Title"
    })
  }

  def loginAction = Action { implicit request =>

    userForm.bindFromRequest().fold(

    formWithError=>{Ok(views.html.error("LOGIN_FORM_ERROR",""))} ,
      incomingData=>{
    val incomingUser = User(incomingData.name,incomingData.password)}
    )

    val incomingUser = userForm.bindFromRequest().get

    val user:Option[User] = User.find(incomingUser.name).filter(_.checkPassword(incomingUser.password))

    user match {

      case Some(user) =>

        Redirect(routes.Application.index()).withSession("currentUser" -> user.name,"password" -> user.password)

      case None => Forbidden("I don't know you")

    }

  }

  def logOutAction = Action {request =>
    Ok(views.html.index("")).withNewSession
  }


  def AuthenticateMe(username:String, password:String):Boolean = {

    val user:Option[User] = User.find(username).filter(_.checkPassword(password))

    if(user.isEmpty){
      return false
    }else{
      return  true
    }

    /*user match {

      case Some(user) => return true

      case None => return false

    }*/
  }

}
