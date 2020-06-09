package com.hydrangea.music.library.index

import java.time.{Instant, ZonedDateTime}

import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.{AndroidPath, VirtualPath}
import com.hydrangea.music.library._
import com.sksamuel.elastic4s.analysis.{Analysis, _}
import com.sksamuel.elastic4s.fields._
import com.sksamuel.elastic4s.http.JavaClient
import com.sksamuel.elastic4s.requests.common.RefreshPolicy
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
  private[index] val CUSTOM_PATH_TREE = "custom_path_tree"
  private[index] val CUSTOM_HIERARCHY = "custom_hierarchy"
  private[index] val CUSTOM_PATH_TREE_REVERSED = "custom_path_tree_reversed"
  private[index] val CUSTOM_HIERARCHY_REVERSED = "custom_hierarchy_reversed"

  private[index] val PATH = "path"
  private[index] val PATH_TREE = "tree"
  private[index] val PATH_TREE_REVERSED = "treeReversed"
  private[index] val HASH = "hash"
  private[index] val LAST_MODIFIED = "lastModified"

  private[index] val TAG = "tag"
  private[index] val TITLE = "title"
  private[index] val ALBUM = "album"
  private[index] val ARTIST = "artist"
  private[index] val YEAR = "year"
  private[index] val TRACK_NUMBER = "trackNumber"
  private[index] val TRACK_COUNT = "trackCount"
  private[index] val DISC_NUMBER = "discNumber"
  private[index] val DISC_COUNT = "discCount"

  private[index] val RAW = "raw"

  private[index] val logger: Logger = LoggerFactory.getLogger(IndexService.getClass)

  def createIndex(device: Device): Unit =
    withClient { client =>
      val pathAnalyzer = CustomAnalyzer(CUSTOM_PATH_TREE, tokenizer = CUSTOM_HIERARCHY)
      val pathAnalyzerReversed: CustomAnalyzer =
        CustomAnalyzer(CUSTOM_PATH_TREE_REVERSED, tokenizer = CUSTOM_HIERARCHY_REVERSED)
      val customHierarchyTokenizer: PathHierarchyTokenizer =
        PathHierarchyTokenizer(CUSTOM_HIERARCHY, delimiter = AndroidPath.pathSeparator)
      val customHierarchyTokenizerReversed: PathHierarchyTokenizer =
        PathHierarchyTokenizer(CUSTOM_HIERARCHY_REVERSED, delimiter = AndroidPath.pathSeparator, reverse = true)
      val pathAnalysis: Analysis =
        Analysis(analyzers = List(pathAnalyzer, pathAnalyzerReversed),
                 tokenizers = List(customHierarchyTokenizer, customHierarchyTokenizerReversed))

      val rawField: List[ElasticField] = List(KeywordField(RAW))

      val pathFields: List[ElasticField] = rawField ++ List(TextField(PATH_TREE, analyzer = Some(CUSTOM_PATH_TREE)),
                                                            TextField(PATH_TREE_REVERSED,
                                                                      analyzer = Some(CUSTOM_PATH_TREE_REVERSED)))
      val pathField = TextField(PATH, fields = pathFields)
      val hashField = KeywordField(name = HASH)
      val lastModifiedField = DateField(LAST_MODIFIED)
      val titleField = TextField(TITLE, fields = rawField)
      val albumField = TextField(ALBUM, fields = rawField)
      val artistField = TextField(ARTIST, fields = rawField)
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
      val mapping: MappingDefinition = Elastic.properties(hashField, pathField, lastModifiedField, tagField)
      val indexName: String = device.serial
      client.execute {
        Elastic.createIndex(indexName).analysis(pathAnalysis).mapping(mapping)
      }.await

      client.close()
    }

  def put(device: Device, record: TrackRecord, forceOverwrite: Boolean = false): Option[TrackRecord] =
    putAll(device: Device, Seq(record), forceOverwrite).headOption

  def putAll(device: Device, records: Seq[TrackRecord], forceOverwrite: Boolean = false): Seq[TrackRecord] =
    withClient { client =>
      val recordByPath: Map[VirtualPath, Seq[TrackRecord]] = records.groupBy(_.path)

      // Get the existing records to see if we already have as recent/more recent data
      val existingRecordByPath: Map[VirtualPath, TrackRecord] =
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
            .query(device, recordsFromPathQuery, 10000)
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

      recordsToWrite.foreach { record =>
        logger.debug(s"Indexing record for device $device: $record")
        client.execute {
          indexInto(device.serial)
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
      }

      recordsToWrite
    }

  def remove(device: Device, path: VirtualPath): Unit =
    remove(device, Seq(path))

  def remove(device: Device, paths: Seq[VirtualPath]): Unit =
    withClient { client =>
      paths.foreach(path => {
        logger.info(s"Deleting $path from index.")
        client.execute(deleteById(device.serial, path.raw)).await
      })
    }

  def dropIndex(device: Device): Unit =
    withClient { client =>
      logger.info(s"Dropping index for device ${device.serial}.")
      client.execute(deleteIndex(device.serial)).await
    }

  def query(device: Device, query: RecordQuery, size: Int): Seq[(Id, TrackRecord)] =
    withClient { client =>
      import com.sksamuel.elastic4s.requests.searches._
      val response: Response[SearchResponse] =
        client.execute {
          val request: SearchRequest =
            Elastic.search(device.serial).query(QueryBuilder.toElasticsearch(query)).size(size)

          logger.debug(s"Sending query: ${request.show}")
          request
        }.await

      parseResults(response)
    }

  def queryAll(device: Device): Seq[(Id, TrackRecord)] =
    withClient { client =>
      logger.debug(s"Querying all records for device: $device")
      val response: Response[SearchResponse] =
        client.execute {
          Elastic.search(device.serial).query(MatchAllQuery()).size(10000)
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
    val path = AndroidPath(sourceMap(PATH).toString)
    val lastModified = ZonedDateTime.parse(sourceMap(LAST_MODIFIED).toString).toInstant

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
                Tag(title, album, artist, year, trackNumber, trackCount, discNumber, discCount))
  }
}

object QueryBuilder {
  import IndexService._
  import com.sksamuel.elastic4s.ElasticDsl._

  @tailrec
  def toElasticsearch(recordQuery: RecordQuery): Query =
    recordQuery match {
      case AllOf(elements)        => bool(mustQueries = elements.map(toElasticsearchHelper), Nil, Nil)
      case AnyOf(elements)        => bool(Nil, shouldQueries = elements.map(toElasticsearchHelper), Nil)
      case op: RecordQueryOperand => toElasticsearch(AnyOf(Seq(op)))
    }

  private def toElasticsearchHelper(recordQuery: RecordQuery): Query =
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
