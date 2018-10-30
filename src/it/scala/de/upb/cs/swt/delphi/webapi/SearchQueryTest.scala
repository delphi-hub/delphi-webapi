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

import de.upb.cs.swt.delphi.webapi.featuredefinitions.FeatureExtractor
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class SearchQueryTest extends FlatSpec with Matchers {
  "Search query" should "check for fields" in {
    val configuration = new Configuration()
    val q = new SearchQuery(configuration, new FeatureExtractor(configuration))

    val response = q.search("[if_icmpeq (opcode:159)]>1")
    response shouldBe a [Success[_]]
  }
}