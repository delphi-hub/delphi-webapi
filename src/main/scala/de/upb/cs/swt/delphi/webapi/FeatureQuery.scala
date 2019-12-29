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

import java.io.{BufferedWriter, File, FileWriter}

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.{ElasticClient, RequestSuccess}
import org.slf4j.LoggerFactory
import spray.json._
import de.upb.cs.swt.delphi.webapi.FeatureJson._

import scala.io.Source

class FeatureQuery(configuration: Configuration) {
  private val log = LoggerFactory.getLogger(this.getClass)

  lazy val featureList: Iterable[Feature] =
    Source.fromResource("features.json")
      .getLines()
      .mkString("\n")
      .parseJson
      .asInstanceOf[JsArray]
      .elements
      .map(r => r.convertTo[Feature])
}
