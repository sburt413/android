package com.hydrangea.music.library.index

import java.time.{Instant, ZonedDateTime}

import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.{AndroidPath, VirtualPath}
import com.hydrangea.music.library._
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

// TODO: Clean closing of clients
object IndexService {
  import com.sksamuel.elastic4s.{ElasticDsl => Elastic}
  import Elastic._

  type Id = String

  private[index] val logger: Logger = LoggerFactory.getLogger(IndexService.getClass)

  def createIndex(device: Device): Unit =
    withClient { client =>
      val rawField: List[ElasticField] = List(KeywordField("raw"))

      val pathField = TextField("path", fields = rawField)
      val hashField = KeywordField(name = "hash")
      val lastModifiedField = DateField("lastModified")
      val titleField = TextField("title", fields = rawField)
      val albumField = TextField("album", fields = rawField)
      val artistField = TextField("artist", fields = rawField)
      val tagField = ObjectField(name = "tag",
                                 dynamic = Some("strict"),
                                 enabled = Some(true),
                                 properties = Seq(titleField, albumField, artistField))
      val mapping: MappingDefinition = Elastic.properties(hashField, pathField, lastModifiedField, tagField)
      val indexName: String = device.serial
      client.execute {
        Elastic.createIndex(indexName).mapping(mapping)
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
        logger.info(s"Indexing record for device $device: $record")
        client.execute {
          indexInto(device.serial)
            .fields(
              "hash" -> record.hash,
              "path" -> record.path.raw,
              "lastModified" -> record.lastModified,
              "tag" -> Map("title" -> record.tag.title, "album" -> record.tag.album, "artist" -> record.tag.artist)
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
        client.execute(deleteById(device.serial, path.raw))
      })
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
    // new HttpHost(, 9200, "http"), new HttpHost("elasticsearch", 9300, "http")
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
    val hash: String = sourceMap("hash").toString
    val path = AndroidPath(sourceMap("path").toString)
    val lastModified = ZonedDateTime.parse(sourceMap("lastModified").toString).toInstant

    val tagMap: Map[String, String] = sourceMap("tag").asInstanceOf[Map[String, String]]
    val title: String = tagMap("title")
    val album: String = tagMap("album")
    val artist: String = tagMap("artist")

    TrackRecord(hash, path, lastModified, Tag(title, album, artist))
  }
}

object QueryBuilder {
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
        val field: String = if (raw) "path.raw" else "path"
        matchQuery(field, path.raw).operator("AND")
      case HashQuery(hash)                  => matchQuery("hash", hash)
      case LastUpdatedBefore(when: Instant) => rangeQuery("lastModified").lt(when.toEpochMilli)
      case LastUpdatedAfter(when: Instant)  => rangeQuery("lastModified").gt(when.toEpochMilli)
      case TitleQuery(title, raw) =>
        val field: String = if (raw) "tag.title.raw" else "tag.title"
        matchQuery(field, title).operator("AND")
      case AlbumQuery(album, raw) =>
        val field: String = if (raw) "tag.album.raw" else "tag.album"
        matchQuery(field, album).operator("AND")
      case ArtistQuery(artist, raw) =>
        val field: String = if (raw) "tag.artist.raw" else "tag.artist"
        matchQuery(field, artist).operator("AND")
    }
}
