// Copyright (C) 2018 The Delphi Team.
// See the LICENCE file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package de.upb.cs.swt.delphi.webapi

import akka.NotUsed
import akka.actor.ActorRef
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.scaladsl.Source
import de.upb.cs.swt.delphi.core.model.ArtifactJson._
import de.upb.cs.swt.delphi.webapi.FeatureJson._
import de.upb.cs.swt.delphi.webapi.IpLogActor._
import de.upb.cs.swt.delphi.webapi.StatisticsJson._
import de.upb.cs.swt.delphi.webapi.search.QueryRequestJson._
import de.upb.cs.swt.delphi.webapi.search.{QueryRequest, SearchError, SearchQuery}
import spray.json._

import scala.concurrent.duration._
import scala.util.{Failure, Success}


class DelphiRoutes(requestLimiter: RequestLimitScheduler) extends JsonSupport with AppLogging {

  def routes: Route = {
    requestLimiter.acceptOnValidLimit {
      apiRoutes
    }
  }

  def apiRoutes: Route = {
    path("version") {
      version
    } ~
      path("features") {
        features
      } ~
      path("statistics") {
        statistics
      } ~
      pathPrefix("search") {
        search
      } ~
      pathPrefix("retrieve" / Remaining) { identifier => retrieve(identifier)
      }
  }

  private def version = {
    get {
      complete {
        BuildInfo.version
      }
    }
  }

  private val featureExtractor = new FeatureQuery(configuration)

  private def features = {
    get {
      complete(featureExtractor.featureList.toJson)
    }
  }


  private def statistics = {
    get {
      complete {
        val result = new StatisticsQuery(configuration).retrieveStandardStatistics
        result match {
          case Some(stats) => stats.toJson
          case _ => HttpResponse(StatusCodes.InternalServerError)
        }
      }
    }
  }

  private def retrieve(identifier: String): Route = {
    get {
      complete(
        RetrieveQuery.retrieve(identifier) match {
          case Some(result) => result.toJson
          case None => HttpResponse(StatusCodes.NotFound)
        }
      )
    }
  }

  def search: Route = {
    post {
      entity(as[QueryRequest]) { input =>
        log.info(s"Received search query: ${input.query}")
        complete(
          new SearchQuery(configuration, featureExtractor).search(input) match {
            case Success(result) => result.toJson
            case Failure(e) => {
              e match {
                case se: SearchError => {
                  HttpResponse(StatusCodes.ServerError(StatusCodes.InternalServerError.intValue)(se.toJson.toString(), ""))
                }
                case _ => {
                  HttpResponse(StatusCodes.ServerError(StatusCodes.InternalServerError.intValue)("Search query failed", ""))
                }
              }
            } 
          }
        )
      }
    }
  }
}

object DelphiRoutes {

  private val ipLogActor = system.actorOf(IpLogActor.props)
  private val requestLimiter = new RequestLimitScheduler(ipLogActor)
  private val routes = new DelphiRoutes(requestLimiter).routes

  def apply(): Route = routes
}

private final class RequestLimitScheduler(ipLogActor: ActorRef) extends JsonSupport {
  Source.tick(0.second, refreshRate, NotUsed)
    .runForeach(_ => {
      ipLogActor ! Reset
    })(materializer)

  def acceptOnValidLimit(apiRoutes: Route): Route = {
    extractClientIP { ip =>
      val promise = (ipLogActor ? Accepted(ip.toString())).mapTo[Boolean]
      onSuccess(promise) {
        success =>
          if (success) {
            apiRoutes
          } else {
            val res = Map("msg" -> "Request limit exceeded")
            complete(res.toJson)
          }
      }
    }
  }
}