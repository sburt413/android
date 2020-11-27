package com.hydrangea.music.library

import java.time.{Instant, ZonedDateTime}

import com.hydrangea.file.{AbsolutePath, FilePath}
import com.hydrangea.music.library.device._
import com.hydrangea.music.track.Tag
import com.sksamuel.elastic4s.analysis.{Analysis, _}
import com.sksamuel.elastic4s.fields._
import com.sksamuel.elastic4s.http.JavaClient
import com.sksamuel.elastic4s.requests.common.RefreshPolicy
import com.sksamuel.elastic4s.requests.indexes.{CreateIndexResponse, IndexResponse}
import com.sksamuel.elastic4s.requests.mappings.MappingDefinition
import com.sksamuel.elastic4s.requests.searches.queries.Query
import com.sksamuel.elastic4s.requests.searches.queries.matches.MatchAllQuery
import com.sksamuel.elastic4s.requests.searches.{SearchHit, SearchResponse}
import com.sksamuel.elastic4s.{Response, _}
import org.slf4j.{Logger, LoggerFactory}
import scalaz.syntax.std.option._

import scala.annotation.tailrec
import scala.util.control.NonFatal

// TODO: Insert configs
object IndexService {
  import com.sksamuel.elastic4s.{ElasticDsl => Elastic}
  import Elastic._

  type Id = String

  // https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-pathhierarchy-tokenizer-examples.html
  private[library] val CUSTOM_PATH_TREE = "custom_path_tree"
  private[library] val CUSTOM_HIERARCHY = "custom_hierarchy"
  private[library] val CUSTOM_PATH_TREE_REVERSED = "custom_path_tree_reversed"
  private[library] val CUSTOM_HIERARCHY_REVERSED = "custom_hierarchy_reversed"

  private[library] val SANITIZER = "sanitizer"

  private[library] val PATH = "path"
  private[library] val PATH_TREE = "tree"
  private[library] val PATH_TREE_REVERSED = "treeReversed"
  private[library] val HASH = "hash"
  private[library] val LAST_MODIFIED = "lastModified"
  private[library] val LAST_INDEXED = "lastIndexed"

  private[library] val TAG = "tag"
  private[library] val TITLE = "title"
  private[library] val ALBUM = "album"
  private[library] val ARTIST = "artist"
  private[library] val YEAR = "year"
  private[library] val TRACK_NUMBER = "trackNumber"
  private[library] val TRACK_COUNT = "trackCount"
  private[library] val DISC_NUMBER = "discNumber"
  private[library] val DISC_COUNT = "discCount"

  private[library] val RAW = "raw"

  private val logger: Logger = LoggerFactory.getLogger(IndexService.getClass)

  def createIndex(indexName: IndexName): Unit =
    withClient { client =>
      val pathAnalyzer = CustomAnalyzer(CUSTOM_PATH_TREE, tokenizer = CUSTOM_HIERARCHY)
      val pathAnalyzerReversed: CustomAnalyzer =
        CustomAnalyzer(CUSTOM_PATH_TREE_REVERSED, tokenizer = CUSTOM_HIERARCHY_REVERSED)
      val customHierarchyTokenizer: PathHierarchyTokenizer =
        PathHierarchyTokenizer(CUSTOM_HIERARCHY, delimiter = FilePath.UnixSeparator)
      val customHierarchyTokenizerReversed: PathHierarchyTokenizer =
        PathHierarchyTokenizer(CUSTOM_HIERARCHY_REVERSED, delimiter = FilePath.UnixSeparator, reverse = true)

      val keywordSanitizer = CustomNormalizer.apply(SANITIZER, Nil, List("lowercase", "asciifolding"))

      val analysis: Analysis =
        Analysis(
          analyzers = List(pathAnalyzer, pathAnalyzerReversed),
          tokenizers = List(customHierarchyTokenizer, customHierarchyTokenizerReversed),
          normalizers = List(keywordSanitizer)
        )

      val rawField: List[ElasticField] = List(TextField(RAW))

      val pathFields: List[ElasticField] = rawField ++ List(TextField(PATH_TREE, analyzer = Some(CUSTOM_PATH_TREE)),
                                                            TextField(PATH_TREE_REVERSED,
                                                                      analyzer = Some(CUSTOM_PATH_TREE_REVERSED)))
      val pathField = TextField(PATH, fields = pathFields)
      val hashField = KeywordField(name = HASH)
      val lastModifiedField = DateField(LAST_MODIFIED)
      val lastIndexedField = DateField(LAST_INDEXED)
      val titleField = KeywordField(TITLE, fields = rawField, normalizer = Some(SANITIZER))
      val albumField = KeywordField(ALBUM, fields = rawField, normalizer = Some(SANITIZER))
      val artistField = KeywordField(ARTIST, fields = rawField, normalizer = Some(SANITIZER))
      val yearField = IntegerField(YEAR)
      val trackNumberField = IntegerField(TRACK_NUMBER)
      val trackCountField = IntegerField(TRACK_COUNT)
      val discNumberField = IntegerField(DISC_NUMBER)
      val discCountField = IntegerField(DISC_COUNT)
      val tagField = ObjectField(
        name = TAG,
        dynamic = Some("strict"),
        enabled = Some(true),
        properties = Seq(titleField,
                         albumField,
                         artistField,
                         yearField,
                         trackNumberField,
                         trackCountField,
                         discNumberField,
                         discCountField)
      )
      val mapping: MappingDefinition =
        Elastic.properties(hashField, pathField, lastModifiedField, lastIndexedField, tagField)

      val response: Response[CreateIndexResponse] =
        client.execute {
          Elastic.createIndex(indexName.value).analysis(analysis).mapping(mapping)
        }.await

      assertSuccess(response, s"create index $indexName")

      client.close()
    }

  def put(indexName: IndexName, record: TrackRecord, forceOverwrite: Boolean = false): Option[TrackRecord] =
    putAll(indexName, Seq(record), forceOverwrite).headOption

  def putAll(indexName: IndexName, records: Seq[TrackRecord], forceOverwrite: Boolean = false): Seq[TrackRecord] =
    withClient { client =>
      val recordByPath: Map[AbsolutePath, Seq[TrackRecord]] = records.groupBy(_.path)

      // Get the existing records to see if we already have as recent/more recent data
      val existingRecordByPath: Map[AbsolutePath, TrackRecord] =
        if (forceOverwrite) {
          // To overwrite, we simply will not bother to see if we have existing data.  Elastic will automatically update
          // the record in the index if there is a collision
          logger.trace(s"Ignoring existing records for: ${recordByPath.keySet.mkString(", ")}")
          Map.empty
        } else {
          // Otherwise, fetch all the existing records for the paths so
          val recordsFromPathQuery: AnyOf = AnyOf(records.map(record => PathQuery(record.path)))
          logger.trace(s"Sending query to find existing records for paths: $recordsFromPathQuery")
          IndexService
            .query(indexName, recordsFromPathQuery, 10000)
            .map(_._2)
            .groupBy(_.path)
            .transform((_, rs) => rs.head)
        }

      // Determine which of the given new records are more recent than what we have indexed for their respective paths
      val recordsToWrite: Seq[TrackRecord] =
        recordByPath.toSeq.flatMap({
          case (path, newRecords) =>
            val existingRecord: Option[TrackRecord] = existingRecordByPath.get(path)
            // Distinct new records just in case the caller made that trivial mistake
            val diff: DiffResult = Diff.mostRecent(newRecords.distinct ++ existingRecord.toSeq: _*)

            diff match {
              case Latest(record) =>
                val possibleRecord: Option[TrackRecord] = Some(record).filter(newRecords.contains)
                val existsMsg: String = possibleRecord.cata(_ => "is new record", "already exists")
                logger.debug(s"Most recent record for path ($path) $existsMsg: $record")
                possibleRecord
              case Same =>
                logger.debug(s"All records for path ($path) match existing records in index.")
                None
              case Conflict =>
                throw new IllegalStateException(
                  s"Encountered conflict attempting to determine latest record for path ($path): New: ${newRecords
                    .mkString(", ")}, Existing: ${existingRecord.mkString(", ")}")
            }
        })

      logger.info(s"Writing ${recordsToWrite.size} records to ${indexName.value}")

      recordsToWrite.foreach { record =>
        logger.debug(s"Indexing record for device ${indexName.value}: $record")
        val indexResponse: Response[IndexResponse] =
          client.execute {
            indexInto(indexName.value)
              .fields(
                HASH -> record.hash,
                PATH -> record.path.raw,
                LAST_MODIFIED -> record.lastModified,
                TAG -> Map(
                  TITLE -> record.tag.title,
                  ALBUM -> record.tag.album,
                  ARTIST -> record.tag.artist,
                  YEAR -> record.tag.year,
                  TRACK_NUMBER -> record.tag.trackNumber,
                  TRACK_COUNT -> record.tag.trackCount,
                  DISC_NUMBER -> record.tag.discNumber,
                  DISC_COUNT -> record.tag.discCount
                )
              )
              .id(record.path.raw)
              .refresh(RefreshPolicy.Immediate)
          }.await

        assertSuccess(indexResponse, s"indexing record $record")
      }

      recordsToWrite
    }

  def remove(indexName: IndexName, path: AbsolutePath): Unit =
    remove(indexName, Seq(path))

  def remove(indexName: IndexName, paths: Seq[AbsolutePath]): Unit =
    withClient { client =>
      paths.foreach(path => {
        logger.info(s"Deleting $path from index ${indexName.value}.")
        client.execute(deleteById(indexName.value, path.raw)).await
      })
    }

  def dropIndex(indexName: IndexName): Unit =
    withClient { client =>
      logger.info(s"Dropping index for ${indexName.value}.")
      client.execute(deleteIndex(indexName.value)).await
    }

  def query(indexName: IndexName, query: DeviceIndexQuery, size: Int): Seq[(Id, TrackRecord)] =
    withClient { client =>
      import com.sksamuel.elastic4s.requests.searches._
      val response: Response[SearchResponse] =
        client.execute {
          val request: SearchRequest =
            Elastic.search(indexName.value).query(QueryBuilder.toElasticsearch(query)).size(size)

          logger.debug(s"Sending query: ${request.show}")
          request
        }.await

      parseResults(response)
    }

  def queryAll(indexName: IndexName): Seq[(Id, TrackRecord)] =
    withClient { client =>
      logger.debug(s"Querying all records for device: ${indexName.value}")
      val response: Response[SearchResponse] =
        client.execute {
          Elastic.search(indexName.value).query(MatchAllQuery()).size(10000)
        }.await

      parseResults(response)
    }

  private def connect(): ElasticClient = {
    val elasticsearchHost = "elasticsearch"
    val elasticsearchPort = 9200
    ElasticClient(JavaClient(ElasticProperties(s"http://$elasticsearchHost:$elasticsearchPort")))
  }

  private def withClient[A](fn: ElasticClient => A): A = {
    val client: ElasticClient = connect()

    var result: Option[A] = None
    try {
      result = Some(fn(client))
    } catch {
      case NonFatal(e) =>
        client.close()
        throw e
    }

    client.close()
    result.get
  }

  private def parseResults(response: Response[SearchResponse]): Seq[(Id, TrackRecord)] =
    response match {
      case results: RequestSuccess[SearchResponse] =>
        val hits: Array[SearchHit] = results.result.hits.hits
        logger.debug(s"Parsing ${hits.length} search hits.")
        hits.map(hit => (hit.id, toRecord(hit.sourceAsMap)))
      case failure: RequestFailure =>
        logger.error("Search failed: " + failure.error)
        throw new RuntimeException(s"Search Failure: ${failure.error}")
    }

  private def toRecord(sourceMap: Map[String, Any]): TrackRecord = {
    val hash: String = sourceMap(HASH).toString
    val path = AbsolutePath(sourceMap(PATH).toString)
    val lastModified = ZonedDateTime.parse(sourceMap(LAST_MODIFIED).toString).toInstant
    val lastIndexed = ZonedDateTime.parse(sourceMap(LAST_INDEXED).toString).toInstant

    val tagMap: Map[String, String] = sourceMap(TAG).asInstanceOf[Map[String, String]]
    val title: String = tagMap(TITLE)
    val album: String = tagMap(ALBUM)
    val artist: String = tagMap(ARTIST)
    val year: Option[Int] = tagMap.get(YEAR).map(_.toInt)
    val trackNumber: Option[Int] = tagMap.get(TRACK_NUMBER).map(_.toInt)
    val trackCount: Option[Int] = tagMap.get(TRACK_COUNT).map(_.toInt)
    val discNumber: Option[Int] = tagMap.get(DISC_NUMBER).map(_.toInt)
    val discCount: Option[Int] = tagMap.get(DISC_COUNT).map(_.toInt)

    TrackRecord(hash,
                path,
                lastModified,
                lastIndexed,
                Tag(title, album, artist, year, trackNumber, trackCount, discNumber, discCount))
  }

  private def assertSuccess(response: Response[_], callName: String): Unit =
    response match {
      case RequestFailure(_, _, _, error) =>
        throw new RuntimeException(s"Exception running $callName: $error", error.asException)
      case _ =>
    }
}

object QueryBuilder {
  import IndexService._
  import com.sksamuel.elastic4s.ElasticDsl._

  @tailrec
  def toElasticsearch(recordQuery: DeviceIndexQuery): Query =
    recordQuery match {
      case AllOf(elements)             => bool(mustQueries = elements.map(toElasticsearchHelper), Nil, Nil)
      case AnyOf(elements)             => bool(Nil, shouldQueries = elements.map(toElasticsearchHelper), Nil)
      case op: DeviceIndexQueryOperand => toElasticsearch(AnyOf(Seq(op)))
    }

  private def toElasticsearchHelper(recordQuery: DeviceIndexQuery): Query =
    recordQuery match {
      case all: AllOf => toElasticsearch(all)
      case any: AnyOf => toElasticsearch(any)
      case PathQuery(path, raw) =>
        val field: String = if (raw) s"$PATH.$RAW" else PATH
        matchQuery(field, path.raw).operator("AND")
      case SubPathQuery(path, raw)          => termQuery(s"$PATH.$PATH_TREE", path.raw)
      case HashQuery(hash)                  => matchQuery(HASH, hash)
      case LastUpdatedBefore(when: Instant) => rangeQuery(LAST_MODIFIED).lt(when.toEpochMilli)
      case LastUpdatedAfter(when: Instant)  => rangeQuery(LAST_MODIFIED).gt(when.toEpochMilli)
      case TitleQuery(title, raw) =>
        val field: String = if (raw) s"$TAG.$TITLE.$RAW" else s"$TAG.$TITLE"
        matchQuery(field, title).operator("AND")
      case AlbumQuery(album, raw) =>
        val field: String = if (raw) s"$TAG.$ALBUM.$RAW" else s"$TAG.$ALBUM"
        matchQuery(field, album).operator("AND")
      case ArtistQuery(artist, raw) =>
        val field: String = if (raw) s"$TAG.$ARTIST.$RAW" else s"$TAG.$ARTIST"
        matchQuery(field, artist).operator("AND")
      case YearQuery(year, raw) =>
        val field: String = if (raw) s"$TAG.$YEAR.$RAW" else s"$RAW.$YEAR"
        matchQuery(field, year).operator("AND")
    }
}

case class IndexName(value: String)
