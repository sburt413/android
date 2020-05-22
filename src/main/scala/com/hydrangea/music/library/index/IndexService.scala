package com.hydrangea.music.library.index

import java.time.{Instant, ZonedDateTime}

import com.hydrangea.android.adb.Device
import com.hydrangea.android.file.AndroidPath
import com.hydrangea.music.library.{Diff, Latest, Tag, TrackRecord}
import com.sksamuel.elastic4s._
import com.sksamuel.elastic4s.fields._
import com.sksamuel.elastic4s.http.JavaClient
import com.sksamuel.elastic4s.requests.common.RefreshPolicy
import com.sksamuel.elastic4s.requests.mappings.MappingDefinition
import com.sksamuel.elastic4s.requests.searches.SearchResponse
import com.sksamuel.elastic4s.requests.searches.queries.Query

import scala.annotation.tailrec
import scala.util.control.NonFatal

// TODO: Clean closing of clients
object IndexService {
  import com.sksamuel.elastic4s.{ElasticDsl => Elastic}
  import Elastic._

  type Id = String

  def createIndex(device: Device): Unit =
    withClient { client =>
      val rawField: List[ElasticField] = List(KeywordField("raw"))

      val hashField = KeywordField(name = "hash")
      val pathField = TextField("path", fields = rawField)
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

  def put(device: Device, record: TrackRecord, forceOverwrite: Boolean = false, replace: Boolean = true): Unit =
    withClient { client =>
      val existingRecords: Seq[(Id, TrackRecord)] = IndexService.query(device, PathQuery(record.path))
      val writeRecord: Boolean =
        if (forceOverwrite) {
          true
        } else {
          // Diff the file against all the existing and check the record we're given is the absolute latest of all
          existingRecords
            .map({ case (_, existing) => Diff.fileDiff(record, existing) })
            .forall({
              case Latest(latest) if latest.equals(record) => true
              case _                                       => false
            })
        }

      if (writeRecord) {
        if (replace) {
          // TODO: Seriously, make 'in' query
          existingRecords.foreach({
            case (id, _) =>
              client.execute {
                deleteById(device.serial, id).refreshImmediately
              }.await
          })
        }

        client.execute {
          indexInto(device.serial)
            .fields(
              "hash" -> record.hash,
              "path" -> record.path.raw,
              "lastModified" -> record.lastModified,
              "tag" -> Map("title" -> record.tag.title, "album" -> record.tag.album, "artist" -> record.tag.artist)
            )
            .refresh(RefreshPolicy.Immediate)
        }.await
      }

      client.close()
    }

  def query(device: Device, query: RecordQuery): Seq[(Id, TrackRecord)] =
    withClient { client =>
      val resp =
        client.execute {
          Elastic.search(device.serial).query(QueryBuilder.toElasticsearch(query))
        }.await

      client.close()

      println("---- Search Results ----")
      println(resp)

      val results: Array[(String, TrackRecord)] =
        resp match {
          case results: RequestSuccess[SearchResponse] =>
            println(results.result.hits.hits.toList)
            results.result.hits.hits.map(hit => (hit.id, toRecord(hit.sourceAsMap)))
          case results: RequestSuccess[_] =>
            println(results.result)
            throw new RuntimeException(s"Search Failure: Not the success we are looking for: $results")
          case failure: RequestFailure =>
            println("We failed " + failure.error)
            throw new RuntimeException(s"Search Failure: ${failure.error}")
        }

      results.toSeq
    }

  private def connect(): ElasticClient = {
    // new HttpHost(, 9200, "http"), new HttpHost("elasticsearch", 9300, "http")
    val elasticsearchHost = "elasticsearch"
    val elasticsearchPort = 9200
    ElasticClient(JavaClient(ElasticProperties(s"http://$elasticsearchHost:$elasticsearchPort")))
  }

  private def withClient[A](fn: ElasticClient => A): A = {
    val client: ElasticClient = connect()

    var retval: Option[A] = None
    try {
      retval = Some(fn(client))
    } catch {
      case NonFatal(e) =>
        client.close()
        throw e
    }

    client.close()
    retval.get
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
