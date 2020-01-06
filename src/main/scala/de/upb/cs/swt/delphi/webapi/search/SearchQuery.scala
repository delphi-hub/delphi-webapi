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

package de.upb.cs.swt.delphi.webapi.search

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.search.SearchHits
import com.sksamuel.elastic4s.http.{ElasticClient, RequestFailure, RequestSuccess}
import com.sksamuel.elastic4s.searches.queries.{NoopQuery, Query}
import de.upb.cs.swt.delphi.core.model._
import de.upb.cs.swt.delphi.core.ql._
import de.upb.cs.swt.delphi.webapi.artifacts.ArtifactTransformer
import de.upb.cs.swt.delphi.webapi.{Configuration, Feature, FeatureQuery, InternalFeature}
import spray.json.JsArray

import scala.io.Source
import scala.util.{Failure, Success, Try}
import spray.json._
import de.upb.cs.swt.delphi.webapi.FeatureJson._


class SearchQuery(configuration: Configuration, featureExtractor: FeatureQuery) {
  private val client = ElasticClient(configuration.elasticsearchClientUri)

  lazy val internalFeatures = Source.fromResource("features.json")
    .getLines()
    .mkString("\n")
    .parseJson
    .asInstanceOf[JsArray]
    .elements
    .map(r => r.convertTo[InternalFeature])

  lazy val externalToInternalFeature = internalFeatures.map(i => i.name -> i.internalName).toMap

  private def checkAndExecuteParsedQuery(parsedQuery: de.upb.cs.swt.delphi.core.ql.Query, limit: Int): Try[SearchHits] = {
    val fields = collectFieldNames(parsedQuery.expr)

    val publicFieldNames = internalFeatures.map(i => i.name)
    val invalidFields = fields.toSet.filter(f => !publicFieldNames.contains(f))

    if (invalidFields.size > 0) return Failure(new IllegalArgumentException(s"Unknown field name(s) used. (${invalidFields.mkString(",")})"))

    val translatedFields = fields.toSet.map(externalToInternalFeature(_))
    def getPrefix (in: String) : String = {
      if (in.contains("*")) {
        in.substring(0, in.indexOf("*"))
      }
      else {
        in
      }
    }
    val selectedFields = parsedQuery
      .selections
      .flatMap(f => internalFeatures.filter(i => i.name.startsWith(getPrefix(f.fieldName))))
      .map(i => i.internalName)


    val query = searchWithType(configuration.esProjectIndex)
      .query(translate(parsedQuery.expr))
      .sourceInclude(ArtifactTransformer.baseFields ++ translatedFields ++ selectedFields)
      .limit(limit)

    val response = client.execute {
      query
    }.await

    response match {
      case RequestSuccess(_, body, _, result) => Success(result.hits)
      case r => Failure(new IllegalArgumentException(r.toString))
    }
  }

  private def translate(node: CombinatorialExpr): Query = {
    node match {
      case AndExpr(left, right) => bool {
        must(
          translate(left),
          translate(right)
        )
      }
      case OrExpr(left, right) => bool {
        should(
          translate(left),
          translate(right)
        )
      }
      case NotExpr(expr) => bool {
        not(translate(expr))
      }
      case XorExpr(left, right) => bool {
        should(
          must(
            translate(left),
            not(translate(right))
          ),
          must(
            not(translate(right)),
            translate(left)
          )
        )
      }
      case EqualExpr(field, value) => matchQuery(externalToInternalFeature(field.fieldName), value)
      case NotEqualExpr(field, value) => bool(not(matchQuery(externalToInternalFeature(field.fieldName), value)))
      case GreaterThanExpr(field, value) => rangeQuery(externalToInternalFeature(field.fieldName)).gt(value.toLong)
      case GreaterOrEqualExpr(field, value) => rangeQuery(externalToInternalFeature(field.fieldName)).gte(value.toLong)
      case LessThanExpr(field, value) => rangeQuery(externalToInternalFeature(field.fieldName)).lt(value.toLong)
      case LessOrEqualExpr(field, value) => rangeQuery(externalToInternalFeature(field.fieldName)).lte(value.toLong)
      case LikeExpr(field, value) => prefixQuery(externalToInternalFeature(field.fieldName), value)
      case _ => NoopQuery
    }
  }

  private def collectFieldNames(node: CombinatorialExpr): Seq[String] = {
    node match {
      case AndExpr(left, right) => collectFieldNames(left) ++ collectFieldNames(right)
      case OrExpr(left, right) => collectFieldNames(left) ++ collectFieldNames(right)
      case NotExpr(expr) => collectFieldNames(expr)
      case XorExpr(left, right) => collectFieldNames(left) ++ collectFieldNames(right)
      case EqualExpr(field, _) => Seq(field.fieldName)
      case NotEqualExpr(field, _) => Seq(field.fieldName)
      case GreaterThanExpr(field, _) => Seq(field.fieldName)
      case GreaterOrEqualExpr(field, _) => Seq(field.fieldName)
      case LessThanExpr(field, _) => Seq(field.fieldName)
      case LessOrEqualExpr(field, _) => Seq(field.fieldName)
      case LikeExpr(field, _) => Seq(field.fieldName)
      case IsTrueExpr(field) => Seq(field.fieldName)
      case FieldReference(name) => Seq(name)
      case _ => Seq()
    }
  }

  def checkValidSize: Option[Int] = {
    import elastic4s.extns._
    import elastic4s.extns.ElasticDslExtn._
    val params = Map("include_defaults" -> true)
    val query = SettingsRequest("delphi", params)
    val res = client.execute {
      query
    }.await
    res match {
      case RequestSuccess(_, b, _, _) => {
        maxResultSize(b, configuration)
      }
      case RequestFailure(_, _, _, _) => {
        None
      }
    }
  }

  def search(query: QueryRequest): Try[SearchResults] = {
    lazy val size = checkValidSize
    val validSize = size.exists(query.limit.getOrElse(defaultFetchSize) <= _)
    if (validSize) {
      val parserResult = new Syntax(query.query).QueryRule.run()
      parserResult match {
        case Failure(e) => Failure(e)
        case Success(parsedQuery) => {
          checkAndExecuteParsedQuery(parsedQuery, query.limit.getOrElse(defaultFetchSize)) match {
            case Failure(e) => Failure(e)
            case Success(hits) => Success(ArtifactTransformer.transformResults(hits))
          }
        }
      }
    }
    else {
      val errorMsg = new SearchError(s"Query limit exceeded default limit:  ${query.limit}>${size}")
      Failure(errorMsg)
    }
  }
}
