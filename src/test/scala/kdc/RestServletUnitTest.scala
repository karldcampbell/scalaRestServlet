package kdc

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

import org.junit.{Test, Before}
import org.junit.Assert._

import scala.collection.immutable.HashMap

/**
  * Created by kdc on 12/19/15.
  */
class RestServletUnitTest {
  import RestServlet._
  import RestServletUnitTest._

  val testServlet = new rs
  @Before def setup = {

  }

  @Test def testSetup() = {
    assertNotNull(testServlet)
  }

  @Test def testStaticRoutes = {
    val h = (r: HttpServletRequest, rs : HttpServletResponse, u:Unit) => {}

    testServlet.register[Unit]("/test", "GET", h, NoPathParams);
    testServlet.register("/test1", "GET", h, NoPathParams);
    testServlet.register("/test2", "GET", h, NoPathParams);
    testServlet.register("/test3", "GET", h, NoPathParams);

    val r = testServlet.matchUrl("/test")
    assertTrue(r.nonEmpty)
    assertEquals(h, r.get.m.get("GET").get.h)

    val a = testServlet.matchUrl("/no")
    assertTrue(a.isEmpty)
  }

  @Test def matchRouteParams = {
    val h0 = (r: HttpServletRequest, rs : HttpServletResponse, u:Unit) => {}
    val h1 = (r: HttpServletRequest, rs : HttpServletResponse, u:Unit) => {}
    val h2 = (r: HttpServletRequest, rs : HttpServletResponse, u:Unit) => {}

    testServlet.register("/{test}", "GET", h0, NoPathParams);
    testServlet.register("/{test}/blah", "POST", h1, NoPathParams);
    testServlet.register("/blah/barf", "POST", h2, NoPathParams);

    val r = testServlet.matchUrl("/test")
    assertTrue(r.nonEmpty)
    assertEquals(h0, r.get.m.get("GET").get.h)

    val a = testServlet.matchUrl("/no")
    assertEquals(h0, a.get.m.get("GET").get.h)

    val b = testServlet.matchUrl("/no/blah")
    assertEquals(h1, b.get.m.get("POST").get.h)

    val c = testServlet.matchUrl("/blah/barf")
    assertEquals(h2, c.get.m.get("POST").get.h)
  }

  @Test def extractRouteParams = {
    var extractedId: String = ""
    val h0 = (r: HttpServletRequest, rs : HttpServletResponse, id: String) => {
      extractedId = id;
    }
    val v0 = (m: Map[String,String])=> {
      println(m.get("id"))
      m.get("id")
    }
    testServlet.register("/{id}", "GET", h0, v0);

    val r = testServlet.matchUrl("/test")
    assertTrue(r.nonEmpty)

    val matched = r.get
    val pathParams = testServlet.extractPathParams(matched);
    assertEquals("test", pathParams.get("id").get)

    val toCall = r.get.m.get("GET").get
    assertEquals(h0, toCall.h)
    val t = toCall.pathType;
    toCall match {
      case x: Handler[t] => x.h.apply(null, null, x.pathValidator.apply(pathParams).get)
    }
    assertEquals("test", extractedId)
  }


  @Test def extractRouteParamsInt = {
    var extractedId: Int = 0
    val h0 = (r: HttpServletRequest, rs : HttpServletResponse, id: Int) => {
      extractedId = id;
    }
    val v0 = (m: Map[String,String])=> {
      println(m.get("id"))
      m.get("id").map((s) => s.toInt)
    }
    testServlet.register("/bobo/{id}", "GET", h0, v0);

    val r = testServlet.matchUrl("/bobo/12345")
    assertTrue(r.nonEmpty)

    val matched = r.get
    val pathParams = testServlet.extractPathParams(matched);
    assertEquals("12345", pathParams.get("id").get)

    val toCall = r.get.m.get("GET").get
    assertEquals(h0, toCall.h)
    val t = toCall.pathType;
    toCall match {
      case x: Handler[t] => x.h.apply(null, null, x.pathValidator.apply(pathParams).get)
    }
    assertEquals(12345, extractedId)
  }


}

object RestServletUnitTest {
  class rs extends RestServlet
}

