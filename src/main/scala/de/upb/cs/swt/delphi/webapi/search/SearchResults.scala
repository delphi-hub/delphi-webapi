package de.upb.cs.swt.delphi.webapi.search

import de.upb.cs.swt.delphi.core.model.Artifact
import de.upb.cs.swt.delphi.core.model.ArtifactJson._
import org.joda.time.DateTime
import spray.json.DefaultJsonProtocol

case class SearchResults(totalHits : Long, hits : Array[Artifact], queried : DateTime = DateTime.now())

object SearchResultsJson extends DefaultJsonProtocol {
  implicit val SearchResultsFormat = jsonFormat3(SearchResults)
}
