package com.hydrangea.repository

import com.google.inject.Guice
import com.hydrangea.file.{
  AbsolutePath,
  FakeDirectory,
  FakeFile,
  FakeFileSystemService,
  FakeRegularFile,
  LocalFileLocation,
  LocalRegularFileData,
  RelativePath,
  UnixPathBase
}
import com.hydrangea.music.track.{Tag, Track, TrackService}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import com.hydrangea.repository.schedule.{
  Schedule,
  ScheduleRecord,
  SchedulerConfiguration,
  SchedulerService,
  SchedulerServiceConfigurationModule
}
import net.codingwell.scalaguice.InjectorExtensions._
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.{RandomStringUtils, RandomUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.ByteArrayOutputStream
import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit

class SchedulerServiceTest extends AnyFlatSpec {
  import com.hydrangea.repository.SchedulerServiceTest._

  "Scheduler Service" should "persist schedules" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)
    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)

    val alice1Record = repositoryRecord(aliceDirectory, alice1RelativePath, yesterday)
    val bob1Record = repositoryRecord(bobDirectory, bob1RelativePath, lastWeek)

    val repository: MusicRepository[LocalFileLocation] = MusicRepository(repositoryRoot, List(alice1Record, bob1Record))

    val files: Seq[FakeFile] =
      Seq(root, aliceDirectory, alice1, bobDirectory, bob1)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))
    val scheduler: SchedulerService = injector.instance[SchedulerService]

    val aliceScheduleRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(aliceLocation, Some(alice1Record.lastIndexed), alice1Record.lastIndexed)
    val bobScheduleRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, bob1Record.lastIndexed)
    val schedule: Schedule[LocalFileLocation] = Schedule(List(aliceScheduleRecord, bobScheduleRecord))
    scheduler.writeSchedule(repository, schedule)

    val loadedSchedule: Schedule[LocalFileLocation] =
      scheduler.loadSchedule(repository).getOrElse(fail("Could not load Schedule"))
    schedule should equal(loadedSchedule)
  }

  it should "create schedules" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)
    val alice3RelativePath = RelativePath(Seq("alice3.mp3"))
    val alice3 = FakeRegularFile(aliceLocation ++ alice3RelativePath, yesterday)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)

    val emptyDirectory = FakeDirectory(charlieLocation, lastWeek)

    val files: Seq[FakeFile] =
      Seq(root, aliceDirectory, alice1, alice2, alice3, bobDirectory, bob1, bob2, emptyDirectory)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val alice1Record = repositoryRecord(aliceDirectory, alice1RelativePath, lastWeek)
    val alice2Record = repositoryRecord(aliceDirectory, alice2RelativePath, lastWeek)
    val repository: MusicRepository[LocalFileLocation] =
      MusicRepository(repositoryRoot, List(alice1Record, alice2Record))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    val schedule: Schedule[LocalFileLocation] = scheduler.createSchedule(repository)

    // The alice2 file has a more recent update (now) than the other records
    val expectedAliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    // The bob2 file has the most recent update (bob2) of the two
    val expectedBobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, tomorrow)
    // The charlie folder has no files, so the directory most recent update is used
    val expectedEmptyRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, None, lastWeek)
    schedule.records should contain theSameElementsAs Seq(expectedAliceRecord, expectedBobRecord, expectedEmptyRecord)
  }

  it should "update schedules" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)
    val alice3RelativePath = RelativePath(Seq("alice3.mp3"))
    val alice3 = FakeRegularFile(aliceLocation ++ alice3RelativePath, yesterday)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)

    val emptyDirectory = FakeDirectory(charlieLocation, lastWeek)

    val files: Seq[FakeFile] =
      Seq(root, aliceDirectory, alice1, alice2, alice3, bobDirectory, bob1, bob2, emptyDirectory)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val alice1Record = repositoryRecord(aliceDirectory, alice1RelativePath, lastWeek)
    val alice2Record = repositoryRecord(aliceDirectory, alice2RelativePath, lastWeek)
    val repository: MusicRepository[LocalFileLocation] =
      MusicRepository(repositoryRoot, List(alice1Record, alice2Record))

    val currentAliceRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(aliceLocation, Some(lastWeek), lastWeek)
    val currentBobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(currentAliceRecord, currentBobRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    val updatedSchedule: Schedule[LocalFileLocation] = scheduler.updateSchedule(repository, currentSchedule)

    val expectedAliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(lastWeek), now)
    val expectedBobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, tomorrow)
    val expectedEmptyRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, None, lastWeek)
    updatedSchedule.records should contain theSameElementsAs Seq(expectedAliceRecord,
                                                                 expectedBobRecord,
                                                                 expectedEmptyRecord)
  }

  it should "split schedules" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)
    val alice3RelativePath = RelativePath(Seq("alice3.mp3"))
    val alice3 = FakeRegularFile(aliceLocation ++ alice3RelativePath, yesterday)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)

    val charlieDirectory = FakeDirectory(charlieLocation, now)
    val charlie1RelativePath = RelativePath(Seq("charlie1.mp3"))
    val charlie1 = FakeRegularFile(charlieLocation ++ charlie1RelativePath, lastWeek)
    val charlie2RelativePath: RelativePath = RelativePath(Seq("charlie2.mp3"))
    val charlie2 = FakeRegularFile(charlieLocation ++ charlie2RelativePath, tomorrow)

    val files: Seq[FakeFile] =
      Seq(root, aliceDirectory, alice1, alice2, alice3, bobDirectory, bob1, bob2, charlieDirectory, charlie1, charlie2)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord, charlieRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    // Take the 3 files under Alice and 2 files under Bob records
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 5, 0)
    takenSchedule.records should contain theSameElementsAs Seq(aliceRecord, bobRecord)
    remainingSchedule.records should contain theSameElementsAs Seq(charlieRecord)
  }

  it should "split schedules skipping large directories" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)
    val bob3RelativePath: RelativePath = RelativePath(Seq("bob3.mp3"))
    val bob3 = FakeRegularFile(bobLocation ++ bob3RelativePath, tomorrow)
    val bob4RelativePath: RelativePath = RelativePath(Seq("bob4.mp3"))
    val bob4 = FakeRegularFile(bobLocation ++ bob4RelativePath, tomorrow)
    val bob5RelativePath: RelativePath = RelativePath(Seq("bob5.mp3"))
    val bob5 = FakeRegularFile(bobLocation ++ bob5RelativePath, tomorrow)

    val charlieDirectory = FakeDirectory(charlieLocation, now)
    val charlie1RelativePath = RelativePath(Seq("charlie1.mp3"))
    val charlie1 = FakeRegularFile(charlieLocation ++ charlie1RelativePath, lastWeek)
    val charlie2RelativePath: RelativePath = RelativePath(Seq("charlie2.mp3"))
    val charlie2 = FakeRegularFile(charlieLocation ++ charlie2RelativePath, tomorrow)

    val files: Seq[FakeFile] =
      Seq(root,
          aliceDirectory,
          alice1,
          alice2,
          bobDirectory,
          bob1,
          bob2,
          bob3,
          bob4,
          bob5,
          charlieDirectory,
          charlie1,
          charlie2)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord, charlieRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    // Take the 2 files under Alice and 2 files under Charlie records, skip 5 under Bob
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 4, 0)
    takenSchedule.records should contain theSameElementsAs Seq(aliceRecord, charlieRecord)
    remainingSchedule.records should contain theSameElementsAs Seq(bobRecord)
  }

  it should "split schedules skipping up to date directories" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)

    val files: Seq[FakeFile] = Seq(root, aliceDirectory, alice1, bobDirectory, bob1)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(now), lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 1, 0)
    takenSchedule.records should contain theSameElementsAs Seq(bobRecord)
    remainingSchedule.records should contain theSameElementsAs Seq(aliceRecord)
  }

  it should "split schedules based on least recently updated" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)

    val charlieDirectory = FakeDirectory(charlieLocation, now)
    val charlie1RelativePath = RelativePath(Seq("charlie1.mp3"))
    val charlie1 = FakeRegularFile(charlieLocation ++ charlie1RelativePath, lastWeek)

    val files: Seq[FakeFile] = Seq(root, aliceDirectory, alice1, bobDirectory, bob1, charlieDirectory, charlie1)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(yesterday), now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, Some(lastWeek), now)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord, charlieRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 2, 0)
    takenSchedule.records should contain theSameElementsAs Seq(bobRecord, charlieRecord)
    remainingSchedule.records should contain theSameElementsAs Seq(aliceRecord)
  }

  it should "split schedules up to a margin of error" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)
    val alice3RelativePath = RelativePath(Seq("alice3.mp3"))
    val alice3 = FakeRegularFile(aliceLocation ++ alice3RelativePath, yesterday)
    val alice4RelativePath = RelativePath(Seq("alice4.mp3"))
    val alice4 = FakeRegularFile(aliceLocation ++ alice4RelativePath, yesterday)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)

    val files: Seq[FakeFile] = Seq(root, aliceDirectory, alice1, alice2, alice3, alice4, bobDirectory, bob1, bob2)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    // We accept the alice record since 4 less within the margin of error with 3
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 3, 1)
    takenSchedule.records should contain theSameElementsAs Seq(aliceRecord)
    remainingSchedule.records should contain theSameElementsAs Seq(bobRecord)
  }

  it should "split schedules within a margin of error" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)

    val files: Seq[FakeFile] = Seq(root, aliceDirectory, alice1, alice2, bobDirectory, bob1, bob2)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    // We stop at alice because 2 is within the margin of error of our target amount of 3
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 3, 1)
    takenSchedule.records should contain theSameElementsAs Seq(aliceRecord)
    remainingSchedule.records should contain theSameElementsAs Seq(bobRecord)
  }

  it should "split schedules taking all available records" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1RelativePath = RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(aliceLocation ++ alice1RelativePath, lastWeek)
    val alice2RelativePath = RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(aliceLocation ++ alice2RelativePath, now)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1RelativePath = RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bobLocation ++ bob1RelativePath, lastWeek)
    val bob2RelativePath: RelativePath = RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bobLocation ++ bob2RelativePath, tomorrow)

    val files: Seq[FakeFile] = Seq(root, aliceDirectory, alice1, alice2, bobDirectory, bob1, bob2)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: SchedulerService = injector.instance[SchedulerService]
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 10, 0)
    takenSchedule.records should contain theSameElementsAs Seq(aliceRecord, bobRecord)
    remainingSchedule.records.isEmpty should equal(true)
  }

  it should "update repositories" in {
    val root = FakeDirectory(repositoryRoot, lastWeek)

    val fileContent1: ByteArrayOutputStream = new ByteArrayOutputStream()
    IOUtils.copy(getClass.getResourceAsStream("/audio/sample1.mp3"), fileContent1)
    val fileContent2: ByteArrayOutputStream = new ByteArrayOutputStream()
    IOUtils.copy(getClass.getResourceAsStream("/audio/sample2.mp3"), fileContent2)
    val fileContent3: ByteArrayOutputStream = new ByteArrayOutputStream()
    IOUtils.copy(getClass.getResourceAsStream("/audio/sample3.mp3"), fileContent3)
    val fileContent4: ByteArrayOutputStream = new ByteArrayOutputStream()
    IOUtils.copy(getClass.getResourceAsStream("/audio/sample4.mp3"), fileContent4)
    val fileContent5: ByteArrayOutputStream = new ByteArrayOutputStream()
    IOUtils.copy(getClass.getResourceAsStream("/audio/sample5.mp3"), fileContent5)

    val aliceDirectory = FakeDirectory(aliceLocation, lastWeek)
    val alice1Location: LocalFileLocation = aliceLocation ++ RelativePath(Seq("alice1.mp3"))
    val alice1 = FakeRegularFile(alice1Location, now, fileContent1.toByteArray)
    val alice2Location: LocalFileLocation = aliceLocation ++ RelativePath(Seq("alice2.mp3"))
    val alice2 = FakeRegularFile(alice2Location, lastWeek, fileContent2.toByteArray)
    val alice3Location: LocalFileLocation = aliceLocation ++ RelativePath(Seq("alice3.mp3"))
    val alice3 = FakeRegularFile(alice3Location, yesterday, fileContent3.toByteArray)

    val bobDirectory = FakeDirectory(bobLocation, now)
    val bob1Location: LocalFileLocation = bobLocation ++ RelativePath(Seq("bob1.mp3"))
    val bob1 = FakeRegularFile(bob1Location, lastWeek, fileContent4.toByteArray)
    val bob2Location: LocalFileLocation = bobLocation ++ RelativePath(Seq("bob2.mp3"))
    val bob2 = FakeRegularFile(bob2Location, tomorrow, fileContent5.toByteArray)

    val charlieDirectory = FakeDirectory(charlieLocation, now)
    val charlie1Location = charlieLocation ++ RelativePath(Seq("charlie1.mp3"))
    val charlie1 = FakeRegularFile(charlie1Location, lastWeek)

    val files: Seq[FakeFile] =
      Seq(root, aliceDirectory, alice1, alice2, alice3, bobDirectory, bob1, bob2, charlieDirectory, charlie1)
    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, configuration, FakeFileSystemService.module(files))

    // alice needs updating; bob hasn't been updated; charlie doesn't need an update
    val aliceSchedule: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(yesterday), now)
    val bobSchedule: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, yesterday)
    val charlieSchedule: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, Some(tomorrow), now)
    val schedule: Schedule[LocalFileLocation] = Schedule(List(aliceSchedule, bobSchedule, charlieSchedule))

    val trackService: TrackService = injector.instance[TrackService]
    val actualAlice1Track: Track = trackService.readTrack(alice1.asFileDataOrThrow[LocalRegularFileData])
    val actualAlice2Track: Track = trackService.readTrack(alice2.asFileDataOrThrow[LocalRegularFileData])
    val actualAlice3Track: Track = trackService.readTrack(alice3.asFileDataOrThrow[LocalRegularFileData])
    val bob1Track: Track = trackService.readTrack(bob1.asFileDataOrThrow[LocalRegularFileData])
    val bob2Track: Track = trackService.readTrack(bob2.asFileDataOrThrow[LocalRegularFileData])

    // Make slight changes so that we know if the data we read is updated or not
    val existingAliceTrack1: Track =
      actualAlice1Track.copy(lastModified = yesterday, tag = actualAlice1Track.tag.copy(title = "Old Alice 1"))
    val alice1RelativePath: RelativePath = alice1Location.path.relativePath(repositoryRoot.path)
    val existingAlice1Record = RepositoryRecord(alice1RelativePath, existingAliceTrack1, yesterday)

    val existingAliceTrack2: Track =
      actualAlice2Track.copy(lastModified = yesterday, tag = actualAlice2Track.tag.copy(title = "Old Alice 2"))
    val alice2RelativePath: RelativePath = alice2Location.path.relativePath(repositoryRoot.path)
    val existingAlice2Record = RepositoryRecord(alice2RelativePath, existingAliceTrack2, yesterday)

    val repository: MusicRepository[LocalFileLocation] =
      MusicRepository(repositoryRoot, List(existingAlice1Record, existingAlice2Record))

    val updateTime = Instant.now().minus(1, ChronoUnit.MILLIS)
    val scheduler: SchedulerService = injector.instance[SchedulerService]
    val updatedRepository: MusicRepository[LocalFileLocation] = scheduler.updateRepository(repository, schedule)

    val expectedRelativePaths: Seq[RelativePath] =
      Seq(alice1, alice2, alice3, bob1, bob2)
        .map(_.location.path)
        .map(_.relativePath(repositoryRoot.path))

    updatedRepository.root should equal(repository.root)
    updatedRepository.records.forall(_.lastIndexed.isAfter(updateTime)) should equal(true)
    updatedRepository.records.map(_.track) should contain theSameElementsAs Seq(actualAlice1Track,
                                                                                actualAlice2Track,
                                                                                actualAlice3Track,
                                                                                bob1Track,
                                                                                bob2Track)
    updatedRepository.records.map(_.path) should contain theSameElementsAs expectedRelativePaths
  }
}

object SchedulerServiceTest {
  // Instant can have nano-second precision, marshalling keeps only mills
  val now: Instant = Instant.now().truncatedTo(ChronoUnit.MILLIS)
  val lastWeek = now.minus(7, ChronoUnit.DAYS)
  val yesterday = now.minus(1, ChronoUnit.DAYS)
  val tomorrow = now.plus(1, ChronoUnit.DAYS)
  val nextWeek: Instant = now.plus(7, ChronoUnit.DAYS)

  val musicRelativePath = RelativePath(Seq("music"))
  val repositoryRoot = LocalFileLocation(AbsolutePath(UnixPathBase, Nil)) ++ musicRelativePath

  val aliceLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("alice"))
  val bobLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("bob"))
  val charlieLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("charlie"))
  val dannyLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("danny"))
  val eveLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("eve"))

  val configuration = SchedulerServiceConfigurationModule(SchedulerConfiguration(Path.of("data")))

  def repositoryRecord(fakeFile: FakeDirectory, relativePath: RelativePath, lastIndexed: Instant): RepositoryRecord =
    RepositoryRecord(
      relativePath,
      Track(RandomStringUtils.randomAlphabetic(6), fakeFile.location.path, fakeFile.modifyTime, randomTag()),
      lastIndexed)

  def randomTag() =
    Tag(
      RandomStringUtils.randomAlphabetic(6),
      RandomStringUtils.randomAlphabetic(6),
      RandomStringUtils.randomAlphabetic(6),
      Some(1950 + RandomUtils.nextInt(0, 70)),
      Some(RandomUtils.nextInt(1, 13)),
      Some(RandomUtils.nextInt(13, 22)),
      Some(1),
      Some(1)
    )
}
