package kdc

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import javax.servlet.{ServletResponse, ServletRequest, GenericServlet}
import scala.reflect.runtime.universe._


import scala.collection.mutable.HashMap

/**
  * Created by kdc on 12/19/15.
  */
abstract class RestServlet extends GenericServlet {
  import RestServlet._

  private val routes = new HashMap[String, HashMap[String, Handler[_]]]

  def register[A](route: String, method: String, routeHandler: RouteHandler[A], pathValidator: Validator[A])(implicit pt: TypeTag[A]) = {
    routes.get(route) match {
      case Some(r) => r.put(method, Handler(routeHandler, pathValidator, pt))
      case _ => routes.put(route, HashMap(method -> Handler(routeHandler, pathValidator,pt)))
    }
  }

  def isParam( t: Tuple2[String, String] ): Boolean = {

    !t._1.isEmpty && t._1.charAt(0) == '{' && t._1.last == '}'
  }

  def matchParts( t: Tuple2[String, String] ): Boolean = {
    if (t._1 == t._2) return true;
    return isParam( t)
  }

  def filterRoutes(url: String)(route: String): Boolean = {
    val urlParts = url.split('/')
    val routeParts = route.split('/')
    if (urlParts.length != routeParts.length) return false

    val parts = routeParts zip urlParts
    parts.filterNot(matchParts).isEmpty
  }

  def matchUrl(url: String): Option[MatchedRoute] = {
    routes.keySet.find(filterRoutes(url)).map((r) => MatchedRoute(r, url, routes.get(r).get))
  }

  def extractPathParams(matchedRoute: MatchedRoute): Map[String,String] = {
    val params = new HashMap[String, String]
    val parts = matchedRoute.r.split('/') zip matchedRoute.u.split('/')
    parts.filter(isParam).foreach((t) => params.put(t._1.substring(1, t._1.length - 1), t._2))
    return params.toMap;
  }

  override def service(servletRequest: ServletRequest, servletResponse: ServletResponse): Unit = {}
}

object RestServlet {
  type RouteHandler[A] = (HttpServletRequest, HttpServletResponse, A) => Unit;

  type Validator[A] = (Map[String, String] => Option[A])

  case class Handler[A](h: RouteHandler[A], pathValidator: Validator[A], pathType: TypeTag[A])
  case class MatchedRoute(r: String, u: String, m: HashMap[String, Handler[_]])

  val NoPathParams: Validator[Unit] = (m) => None;
  val NoQueryParams: Validator[Unit] = (m) => None;
  val NoBodyParams: Validator[Unit] = (m) => None;
}
