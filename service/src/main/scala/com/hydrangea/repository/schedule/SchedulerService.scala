package com.hydrangea.repository.schedule

import java.nio.file.Path
import java.time.Instant
import java.util.Objects

import argonaut._
import com.google.inject.Inject
import com.hydrangea.Configuration
import com.hydrangea.file.{
  AndroidLocation,
  DirectoryFileData,
  FileData,
  FileLocation,
  FileSystemService,
  LocalFileLocation,
  RegularFileData
}
import com.hydrangea.music.track.{Track, TrackService}
import com.hydrangea.repository.{MusicRepository, RepositoryRecord}

import scala.annotation.tailrec
import scala.reflect.ClassTag

// TODO: Find deleted entries
class SchedulerService @Inject()(configuration: Configuration,
                                 fileSystemService: FileSystemService,
                                 trackService: TrackService,
                                 scheduleDao: ScheduleDAO) {
  def loadSchedule[L <: FileLocation: EncodeJson: DecodeJson](repository: MusicRepository[L]): Option[Schedule[L]] =
    scheduleDao.load[L](scheduleFile(repository))

  def writeSchedule[L <: FileLocation: EncodeJson: DecodeJson](repository: MusicRepository[L],
                                                               schedule: Schedule[L]): Unit =
    scheduleDao.persist(schedule, scheduleFile(repository))

  def createSchedule[L <: FileLocation](source: MusicRepository[L])(implicit classTag: ClassTag[L]): Schedule[L] =
    updateSchedule(source, Schedule(Nil))

  def updateSchedule[L <: FileLocation](source: MusicRepository[L], schedule: Schedule[L])(
      implicit classTag: ClassTag[L]): Schedule[L] = {
    val directories: Seq[DirectoryFileData] =
      fileSystemService
        .list(source.root)
        .collect({
          case directory: DirectoryFileData => directory
        })

    val newRecords: Seq[ScheduleRecord[L]] =
      for {
        directory <- directories
        location <- directory.location.to[L]
      } yield {
        val mostRecentUpdate: Instant =
          fileSystemService
            .mostRecentUpdate(directory.location)
            .getOrElse(
              throw new IllegalStateException(s"No most recent update time for location: ${directory.location}"))

        ScheduleRecord(location, None, mostRecentUpdate)
      }

    schedule.merge(newRecords)
  }

  def splitSchedule[L <: FileLocation](schedule: Schedule[L],
                                       fileCount: Int,
                                       marginOfError: Int): (Schedule[L], Schedule[L]) = {
    @tailrec
    def take(remaining: List[ScheduleRecord[L]],
             taken: List[ScheduleRecord[L]],
             skipped: List[ScheduleRecord[L]],
             countRemaining: Int): (Schedule[L], Schedule[L]) = {
      val noFilesNeeded: Boolean = countRemaining - marginOfError <= 0
      val nothingLeftToTake: Boolean = remaining.isEmpty

      if (noFilesNeeded || nothingLeftToTake) {
        (Schedule(taken), Schedule(skipped ++ remaining))
      } else {
        val current: ScheduleRecord[L] = remaining.head

        // We are guessing that all files are mp3s, that is a good enough for a heuristic
        val fileCount: Int = fileSystemService.regularFileCount(current.directoryLocation)

        if (fileCount <= countRemaining + marginOfError) {
          take(remaining.tail, taken :+ current, skipped, countRemaining - fileCount)
        } else {
          take(remaining.tail, taken, skipped :+ current, countRemaining)
        }
      }
    }

    val (recordsNeedingUpdates, upToDateRecords) = schedule.records.partition(record => record.needsUpdate)
    take(recordsNeedingUpdates.sortBy(_.lastIndexed), Nil, upToDateRecords, fileCount)
  }

  def updateRepository[L <: FileLocation](source: MusicRepository[L], schedule: Schedule[L])(
      implicit classTag: ClassTag[L]): MusicRepository[L] = {
    val indexTime: Instant = Instant.now()
    val newRecordsByLocation: Map[L, List[RepositoryRecord]] =
      schedule.records
        .filter(_.needsUpdate)
        .map(record => {
          val mp3Files: Seq[FileData] = fileSystemService.scan(record.directoryLocation).filter(isMp3)
          val newRecords: List[RepositoryRecord] =
            mp3Files
              .collect({
                case file: RegularFileData =>
                  val track: Track = trackService.readTrack(file)
                  RepositoryRecord(source.root.path, track, indexTime)
              })
              .toList

          record.directoryLocation -> newRecords
        })
        .toMap

    newRecordsByLocation.foldLeft(source)({
      case (currentRepository, (directoryLocation, newDirectoryRecords)) =>
        currentRepository.removeDirectory(directoryLocation).addRecords(newDirectoryRecords)
    })
  }

  private def isMp3(file: FileData): Boolean = file.location.path.raw.endsWith(".mp3")

  private def localLocationFolder: Path =
    configuration.repositoryDataDirectory.resolve("local")

  private def androidLocationFolder: Path =
    configuration.repositoryDataDirectory.resolve("android")

  private def scheduleFile[L <: FileLocation](musicRepository: MusicRepository[L]): Path =
    scheduleFile(musicRepository.root)

  private def scheduleFile(repositoryLocation: FileLocation): Path = {
    val hash: Int = hashLocation(repositoryLocation)
    val indexName: String =
      repositoryLocation match {
        case LocalFileLocation(path) => s"$localLocationFolder/schedule-local-${path.last}-$hash"
        case AndroidLocation(device, path) =>
          s"$androidLocationFolder/schedule-android-${device.serial}-${path.last}-$hash"
      }

    configuration.repositoryDataDirectory.resolve(indexName + ".json")
  }

  private def hashLocation[L <: FileLocation](location: L): Int =
    location match {
      case LocalFileLocation(path)       => Objects.hash("local", path)
      case AndroidLocation(device, path) => Objects.hash(device.serial, path)
    }
}
