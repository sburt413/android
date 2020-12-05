package com.hydrangea.music.library

import java.nio.file.{Files, Path}
import java.time.Instant

import com.hydrangea.file.FilePath._
import com.hydrangea.file.{AbsolutePath, FilePath, LocalRegularFileData}
import com.hydrangea.music.library.record.{
  IndexRecord,
  IndexRecordService,
  LastIndexedRecord,
  RecordCandidate,
  Schedule,
  SynchronizationJob
}
import com.hydrangea.music.library.repository.{Repository, RepositoryScheduler}
import com.hydrangea.music.track.TrackService
import com.hydrangea.process.CLIProcessFactory
import org.slf4j.Logger

import scala.jdk.StreamConverters._

class RepositoryLibraryService(trackService: TrackService) {
  import RepositoryLibraryService._

  def createRepositoryIndex(repository: Repository): IndexRecord = {
    val indexRecord: IndexRecord = createIndexRecord(repository)

    logger.info(
      s"Writing record index for repository ${indexRecord.rootDirectoryPath} with ${indexRecord.childRecords.length} entries.")
    IndexRecordService.writeRecord(indexName(repository), indexRecord)

    logger.debug(s"Creating elasticsearch index for ${repository.rootDirectory}")
    IndexService.createIndex(indexName(repository))
    logger.info(s"Created elasticsearch index for ${repository.rootDirectory}: ${indexName(repository)}")

    indexRecord
  }

  private def createIndexRecord(repository: Repository): IndexRecord = {
    val candidates: LazyList[RecordCandidate] =
      Files
        .walk(repository.rootDirectory.toJavaPath, 1)
        .toScala(LazyList)
        .filter(path => Files.isDirectory(path))
        .filterNot(path => path.equals(repository.rootDirectory.toJavaPath))
        .filterNot(path => path.startsWith("."))
        .map(buildCandidate)

    IndexRecord.create(repository.rootDirectory.location.path, candidates.toList)
  }

  private def buildCandidate(artistFolder: Path): RecordCandidate = {
    val lastModifiedTimes: LazyList[Instant] =
      Files
        .walk(artistFolder)
        .toScala(LazyList)
        .filter(path => path.toString.toLowerCase.endsWith(FilePath.mp3Extension))
        .map(path => Files.getLastModifiedTime(path).toInstant)

    val windowsPath: AbsolutePath = artistFolder.asAbsolutePath
    if (lastModifiedTimes.isEmpty) {
      val directoryModifiedTime: Instant = Files.getLastModifiedTime(artistFolder).toInstant
      RecordCandidate(windowsPath, 0, directoryModifiedTime)
    } else {
      val fileCount: Int = lastModifiedTimes.size
      RecordCandidate(windowsPath, fileCount, lastModifiedTimes.max)
    }
  }

  def scanRepository(repository: Repository): IndexRecord = {
    val record: IndexRecord =
      IndexRecordService
        .getRecord(indexName(repository))
        .getOrElse(throw new IllegalArgumentException(s"No index record exists for repository ($repository)"))

    val candidates: List[RecordCandidate] =
      Files
        .walk(repository.rootDirectory.toJavaPath, 1)
        .toScala(LazyList)
        .filter(path => Files.isDirectory(path))
        .filterNot(path => path.equals(repository.rootDirectory.toJavaPath))
        .filterNot(path => path.startsWith("."))
        .map(buildCandidate)
        .toList

    val (updatedRecord, deletedRecords) = record.reindex(candidates)
    IndexService.remove(indexName(repository), deletedRecords.map(_.directoryPath))
    IndexRecordService.writeRecord(indexName(repository), updatedRecord)

    updatedRecord
  }

  def getRecordsToSynchronize(repository: Repository): Option[List[LastIndexedRecord]] =
    IndexRecordService.getRecord(indexName(repository)).map(record => record.needsUpdating)

  def scheduleSynchronization(repository: Repository, desiredFileCount: Int): Option[RepositorySchedule] =
    IndexRecordService
      .getRecord(indexName(repository))
      .map(record => RepositoryScheduler(repository).schedule(desiredFileCount, record))

  def synchronizeElasticsearchIndex(repository: Repository, schedule: RepositorySchedule): Unit = {
    val indexRecord: IndexRecord =
      IndexRecordService
        .getRecord(indexName(repository))
        .getOrElse(throw new IllegalStateException(s"No index record for repository: $repository"))

    SynchronizationJob(trackService).run(schedule, indexRecord, indexName(repository))
  }

  def dropIndex(repository: Repository): Unit = {
    logger.info(s"Deleting index for ${repository.rootDirectory}.")
    IndexService.dropIndex(indexName(repository))
    IndexRecordService.deleteRecord(indexName(repository))
  }
}

object RepositoryLibraryService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(RepositoryLibraryService.getClass)

  type RepositorySchedule = Schedule[LocalRegularFileData]

  def apply(trackService: TrackService): RepositoryLibraryService =
    new RepositoryLibraryService(trackService)

  def apply(cliProcessFactory: CLIProcessFactory): RepositoryLibraryService =
    apply(TrackService(cliProcessFactory))

  def indexName(repository: Repository): IndexName = {
    val hashCode: Int = repository.hashCode()
    val directoryName: String = repository.rootDirectory.location.path.segments.last.toLowerCase
    IndexName(directoryName + "-" + hashCode)
  }
}
