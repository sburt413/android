package com.hydrangea.music.library

import java.nio.file.{Files, Path}
import java.time.Instant

import com.hydrangea.android.file.{NormalizedPath, VirtualPath, WindowsPath, WindowsRegularFile}
import com.hydrangea.music.library.record.{IndexRecord, LastIndexedRecord, RecordCandidate, Schedule}
import com.hydrangea.music.library.repository.{
  Repository,
  RepositoryIndexRecordService,
  RepositoryScheduler,
  RepositorySynchronizationJob
}
import org.slf4j.Logger

import scala.jdk.StreamConverters._

object RepositoryLibraryService {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(RepositoryLibraryService.getClass)

  def indexName(repository: Repository): IndexName = {
    val hashCode: Int = repository.hashCode()
    val normalized: NormalizedPath = NormalizedPath(repository.rootDirectory.path)
    val directoryName: String = normalized.segments.last.toLowerCase
    IndexName(directoryName + "-" + hashCode)
  }

  type RepositorySchedule = Schedule[WindowsPath, WindowsRegularFile]

  def createRepositoryIndex(repository: Repository): IndexRecord[WindowsPath] = {
    val indexRecord: IndexRecord[WindowsPath] = createIndexRecord(repository)

    logger.info(
      s"Writing record index for repository ${indexRecord.rootDirectoryPath} with ${indexRecord.childRecords.length} entries.")
    RepositoryIndexRecordService.writeRecord(indexName(repository), indexRecord)

    logger.debug(s"Creating elasticsearch index for ${repository.rootDirectory}")
    IndexService.createIndex(indexName(repository))
    logger.info(s"Created elasticsearch index for ${repository.rootDirectory}: ${indexName(repository)}")

    indexRecord
  }

  private def createIndexRecord(repository: Repository): IndexRecord[WindowsPath] = {
    val candidates: LazyList[RecordCandidate[WindowsPath]] =
      Files
        .walk(repository.rootDirectory.javaPath, 1)
        .toScala(LazyList)
        .filter(path => Files.isDirectory(path))
        .filterNot(path => path.equals(repository.rootDirectory.toPath))
        .filterNot(path => path.startsWith("."))
        .map(buildCandidate)

    IndexRecord.create(repository.rootDirectory.path, candidates.toList)
  }

  private def buildCandidate(artistFolder: Path): RecordCandidate[WindowsPath] = {
    val lastModifiedTimes: LazyList[Instant] =
      Files
        .walk(artistFolder)
        .toScala(LazyList)
        .filter(path => path.toString.toLowerCase.endsWith(VirtualPath.mp3Extension))
        .map(path => Files.getLastModifiedTime(path).toInstant)

    val windowsPath: WindowsPath = WindowsPath(artistFolder.toAbsolutePath.toString)
    if (lastModifiedTimes.isEmpty) {
      val directoryModifiedTime: Instant = Files.getLastModifiedTime(artistFolder).toInstant
      RecordCandidate(windowsPath, 0, directoryModifiedTime)
    } else {
      val fileCount: Int = lastModifiedTimes.size
      RecordCandidate(windowsPath, fileCount, lastModifiedTimes.max)
    }
  }

  def scanRepository(repository: Repository): IndexRecord[WindowsPath] = {
    val record: IndexRecord[WindowsPath] =
      RepositoryIndexRecordService
        .getRecord(indexName(repository))
        .getOrElse(throw new IllegalArgumentException(s"No index record exists for repository (${repository})"))

    val candidates: List[RecordCandidate[WindowsPath]] =
      Files
        .walk(repository.rootDirectory.javaPath, 1)
        .toScala(LazyList)
        .filter(path => Files.isDirectory(path))
        .filterNot(path => path.equals(repository.rootDirectory.toPath))
        .filterNot(path => path.startsWith("."))
        .map(buildCandidate)
        .toList

    val (updatedRecord, deletedRecords) = record.reindex(candidates)
    IndexService.remove(indexName(repository), deletedRecords.map(_.directoryPath))
    RepositoryIndexRecordService.writeRecord(indexName(repository), updatedRecord)

    updatedRecord
  }

  def getRecordsToSynchronize(repository: Repository): Option[List[LastIndexedRecord[WindowsPath]]] =
    RepositoryIndexRecordService.getRecord(indexName(repository)).map(record => record.needsUpdating)

  def scheduleSynchronization(repository: Repository, desiredFileCount: Int): Option[RepositorySchedule] =
    RepositoryIndexRecordService
      .getRecord(indexName(repository))
      .map(record => RepositoryScheduler(repository).schedule(desiredFileCount, record))

  def synchronizeElasticsearchIndex(repository: Repository, schedule: RepositorySchedule): Unit = {
    val indexRecord: IndexRecord[WindowsPath] =
      RepositoryIndexRecordService
        .getRecord(indexName(repository))
        .getOrElse(throw new IllegalStateException(s"No index record for repository: $repository"))

    RepositorySynchronizationJob.run(schedule, indexRecord, indexName(repository))
  }

  def dropIndex(repository: Repository): Unit = {
    logger.info(s"Deleting index for ${repository.rootDirectory}.")
    IndexService.dropIndex(indexName(repository))
    RepositoryIndexRecordService.deleteRecord(indexName(repository))
  }
}
