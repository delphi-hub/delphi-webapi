package de.upb.cs.swt.delphi.webapi

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.server.HttpApp
import akka.pattern.ask
import akka.util.Timeout
import de.upb.cs.swt.delphi.featuredefinitions.FeatureListMapping
import de.upb.cs.swt.delphi.webapi.ElasticActorManager.{Enqueue, Retrieve}
import de.upb.cs.swt.delphi.webapi.ElasticRequestLimiter.Validate
import spray.json._

/**
  * Web server configuration for Delphi web API.
  */
object Server extends HttpApp with JsonSupport with AppLogging {

  private val configuration = new Configuration()
  private val system = ActorSystem("delphi-webapi")
  private val actorManager = system.actorOf(ElasticActorManager.props(configuration))
  private val requestLimiter = system.actorOf(ElasticRequestLimiter.props(configuration, actorManager))
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  override def routes =
    path("version") {
      version
    } ~
      path("features") {
        features
      } ~
      pathPrefix("search" / Remaining) { query => search(query) } ~
      pathPrefix("retrieve" / Remaining) { identifier => retrieve(identifier) } ~
      pathPrefix("enqueue" / Remaining) { identifier => enqueue(identifier) }


  private def version = {
    get {
      complete {
        BuildInfo.version
      }
    }
  }

  private def features = {
    get {
      complete {
        FeatureListMapping.featureList.toJson
      }
    }
  }

  def retrieve(identifier: String) = {
    get {
      pass { //TODO: Require authentication here
        complete(
          (actorManager ? Retrieve(identifier)).mapTo[String]
        )
      } ~ extractClientIP { ip =>
        complete(
          (requestLimiter ? Validate(ip, Retrieve(identifier))).mapTo[String]
        )
      }
    }
  }

  def enqueue(identifier: String) = {
    get {
      pass { //TODO: Require authorization here
        complete(
          (actorManager ? Enqueue(identifier)).mapTo[String]
        )
      }
    }
  }

  def search(query: String) = {
    get {
      complete {
        query
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val configuration = new Configuration()
    Server.startServer(configuration.bindHost, configuration.bindPort)
    system.terminate()
  }


}


