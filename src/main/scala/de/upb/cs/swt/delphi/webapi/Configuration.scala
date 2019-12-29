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

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.{ElasticsearchClientUri, Index, IndexAndType}
import de.upb.cs.swt.delphi.instancemanagement.InstanceEnums.{ComponentType, InstanceState}
import de.upb.cs.swt.delphi.instancemanagement.{Instance, InstanceLink, InstanceRegistry}

import scala.util.{Failure, Success, Try}

/**
  * @author Ben Hermann
  */
class Configuration( //Server and Elasticsearch configuration
                     val bindHost: String = "0.0.0.0",
                     val bindPort: Int = 8080,
                     val esIndex: String = "delphi",
                     val esType: String = "project",

                     //Actor system configuration
                     val elasticActorPoolSize: Int = 8
                   ) {

  lazy val esProjectIndex: IndexAndType = esIndex / esType
  lazy val elasticsearchClientUri: ElasticsearchClientUri = ElasticsearchClientUri(
    elasticsearchInstance.host + ":" + elasticsearchInstance.portNumber)

  lazy val elasticsearchInstance: Instance = InstanceRegistry.retrieveElasticSearchInstance(configuration = this) match {
    case Success(instance) => instance
    case Failure(_) => Instance(
      None,
      fallbackElasticSearchHost,
      fallbackElasticSearchPort,
      "Default ElasticSearch instance",
      ComponentType.ElasticSearch,
      None,
      InstanceState.Running,
      List.empty[String],
      List.empty[InstanceLink],
      List.empty[InstanceLink])
  }
  val defaultElasticSearchPort: Int = 9200
  val defaultElasticSearchHost: String = "elasticsearch://localhost"
  val instanceName = "MyWebApiInstance"
  val instanceRegistryUri: String = sys.env.getOrElse("DELPHI_IR_URI", "http://localhost:8087")
  lazy val usingInstanceRegistry: Boolean = assignedID match {
    case Some(_) => true
    case None => false
  }
  lazy val assignedID: Option[Long] = InstanceRegistry.handleInstanceStart(configuration = this)

  lazy val fallbackElasticSearchPort: Int = sys.env.get("DELPHI_ELASTIC_URI") match {
    case Some(hostString) => if (hostString.count(c => c == ':') == 3) {
      Try(hostString.split(":")(2).toInt) match {
        case Success(port) => port
        case Failure(_) => defaultElasticSearchPort
      }
    } else {
      defaultElasticSearchPort
    }
    case None => defaultElasticSearchPort
  }

  lazy val fallbackElasticSearchHost: String = sys.env.get("DELPHI_ELASTIC_URI") match {
    case Some(hostString) =>
      if (hostString.count(c => c == ':') == 2) {
        hostString.substring(0, hostString.lastIndexOf(":"))
      } else {
        hostString
      }
    case None => defaultElasticSearchHost

  }
  lazy val instanceId: Option[Long] = InstanceRegistry.handleInstanceStart(configuration = this)

  val jwtSecretKey: String  = sys.env.getOrElse("DELPHI_JWT_SECRET","changeme")

}


