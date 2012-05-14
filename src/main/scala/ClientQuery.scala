package main.scala

import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.Http
import java.net.InetSocketAddress
import org.jboss.netty.util.CharsetUtil
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import com.twitter.finagle._
import com.twitter.util._

/**
 * A somewhat advanced example of using Filters with Clients. Below, HTTP 4xx and 5xx
 * class requests are converted to Exceptions. Additionally, two parallel requests are
 * made and when they both return (the two Futures are joined) the TCP connection(s)
 * are closed.
 */
object ClientQuery {
  class InvalidRequest extends Exception

  /**
   * Convert HTTP 4xx and 5xx class responses into Exceptions.
   */

  val handleErrors = new HandleErrors
  val defaultAddress = "http://134.197.34.120"
  val defaultRequest = "/search?q="
  var result = ""
  var done = true              //check to see if query returned results
  var noFinish = false            //check to see if query will not return results

  class HandleErrors extends SimpleFilter[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
      // flatMap asynchronously responds to requests and can "map" them to both
      // success and failure values:
      service(request) flatMap { response =>
        response.getStatus match {
          case OK        => Future.value(response)
          case FORBIDDEN => Future.exception(new InvalidRequest)
          case _         => Future.exception(new Exception(response.getStatus.getReasonPhrase))
        }
      }
    }
  }

  val clientWithoutErrorHandling: Service[HttpRequest, HttpResponse] = ClientBuilder()
    .codec(Http())
    .hosts(new InetSocketAddress("134.197.34.120", 2984))
    .hostConnectionLimit(1)
    //.requestTimeout() //can't figure out util.Duration
    .build()


  def requestWord(word : String) {
    done = false
    noFinish = false
    result = ""

    // compose the Filter with the client:
    val client: Service[HttpRequest, HttpResponse] = handleErrors andThen clientWithoutErrorHandling

    println("))) Issuing request: ")
    val request2 = makeUnauthorizedRequest(word, client)

    // When both request1 and request2 have completed, close the TCP connection(s).
    //client.release()
  }

  private[this] def makeUnauthorizedRequest(word : String, client: Service[HttpRequest, HttpResponse]) = {
    val unauthorizedRequest = new DefaultHttpRequest(
      HttpVersion.HTTP_1_1, HttpMethod.GET, defaultRequest + word)

    // use the onFailure callback since we convert HTTP 4xx and 5xx class
    // responses to Exceptions.
    client(unauthorizedRequest) onSuccess { response =>
      //println("Result : " + response) //+ ",    " + (response.getContent).toString())
      var values = response.getContent //dynamicChannelBuffer
      var b : Byte = 0
      //var result = ""
      println("Size of Result: " + values.capacity)
      for (i<- 0 until values.capacity) {
        b = values.getByte(i);
        result += b.toChar
      }
      result.reverse
      println("Result in Callback (first line): " + result.slice (0, 10))
      done =true
    }
    client(unauthorizedRequest) onFailure { error =>
      println("))) request errored: " + error.getClass.getName + " , " + error.getMessage)
    }
  }
}

/*import com.twitter.finagle.{Service, SimpleFilter}
import java.net.InetSocketAddress
import com.twitter.finagle.builder._
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import org.jboss.netty.handler.codec.http.HttpVersion.HTTP_1_1
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.util.CharsetUtil.UTF_8
import com.twitter.util.Future
import java.net.InetSocketAddress
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.http.Http
import com.twitter.finagle.builder.ClientBuilder
import java.net.InetSocketAddress

object ClientQuery {

  val address: SocketAddress = new InetSocketAddress(10000)
  val defaultAddress = "http://134.197.34.120:2984"
  val defaultRequest = "/search?q="

  val client: Service[HttpRequest, HttpResponse] = ClientBuilder()
    .codec(Http())
    .hosts(address)                                   //get address
    .hostConnectionLimit(1)
    .retries(2)                         // (1) per-request retries
    .build()

  def requestWord(word : String) {
    // Issue a request, get a response:
    val request: HttpRequest = new DefaultHttpRequest(HTTP_1_1, HttpMethod.GET, defaultAddress + defaultRequest + word)
    val responseFuture: Future[HttpResponse] = client(request)
    responseFuture onSuccess { response => println("Received response: " + response)
        }
  }

}*/