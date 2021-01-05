package com.hydrangea.repository

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.inject.Guice
import com.hydrangea.file.{
  AbsolutePath,
  FakeDirectory,
  FakeFile,
  FakeFileSystemService,
  FakeRegularFile,
  LocalFileLocation,
  RelativePath,
  UnixPathBase
}
import com.hydrangea.music.track.{Tag, Track}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.apache.commons.lang3.{RandomStringUtils, RandomUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MusicRepositorySchedulerTest extends AnyFlatSpec {
  import com.hydrangea.repository.MusicRepositorySchedulerTest._

  "Music Repository Scheduler" should "create schedules" in {
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val alice1Record = repositoryRecord(aliceDirectory, alice1RelativePath, lastWeek)
    val alice2Record = repositoryRecord(aliceDirectory, alice2RelativePath, lastWeek)
    val repository: MusicRepository[LocalFileLocation] =
      MusicRepository(repositoryRoot, List(alice1Record, alice2Record))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val alice1Record = repositoryRecord(aliceDirectory, alice1RelativePath, lastWeek)
    val alice2Record = repositoryRecord(aliceDirectory, alice2RelativePath, lastWeek)
    val repository: MusicRepository[LocalFileLocation] =
      MusicRepository(repositoryRoot, List(alice1Record, alice2Record))

    val currentAliceRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(aliceLocation, Some(lastWeek), lastWeek)
    val currentBobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(currentAliceRecord, currentBobRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord, charlieRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord, charlieRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(now), lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, lastWeek)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(yesterday), now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, Some(lastWeek), now)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord, charlieRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
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
    val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(files))

    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val currentSchedule: Schedule[LocalFileLocation] = Schedule(List(aliceRecord, bobRecord))

    val scheduler: MusicRepositoryScheduler = injector.instance[MusicRepositoryScheduler]
    val (takenSchedule, remainingSchedule) = scheduler.splitSchedule(currentSchedule, 10, 0)
    takenSchedule.records should contain theSameElementsAs Seq(aliceRecord, bobRecord)
    remainingSchedule.records.isEmpty should equal(true)
  }

  "Schedule" should "merge last indexed" in {
    val recentlyIndexedRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(aliceLocation, Some(now), now)
    val oldIndexedRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(aliceLocation, Some(yesterday), now)
    val unindexedRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)

    Schedule(List(unindexedRecord))
      .merge(List(oldIndexedRecord))
      .records should contain theSameElementsInOrderAs List(oldIndexedRecord)

    Schedule(List(oldIndexedRecord))
      .merge(List(recentlyIndexedRecord))
      .records should contain theSameElementsInOrderAs List(recentlyIndexedRecord)

    Schedule(List(oldIndexedRecord))
      .merge(List(unindexedRecord))
      .records should contain theSameElementsInOrderAs List(oldIndexedRecord)

    Schedule(List(recentlyIndexedRecord))
      .merge(List(oldIndexedRecord))
      .records should contain theSameElementsInOrderAs List(recentlyIndexedRecord)
  }

  it should "merge most recent update time" in {
    val oldRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, lastWeek)
    val newRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, None, now)

    val updatedSchedule: Schedule[LocalFileLocation] = Schedule(List(oldRecord)).merge(List(newRecord))
    updatedSchedule.records should contain theSameElementsInOrderAs List(newRecord)

    val unchangedSchedule: Schedule[LocalFileLocation] = Schedule(List(newRecord)).merge(List(oldRecord))
    unchangedSchedule.records should contain theSameElementsInOrderAs List(newRecord)
  }

  it should "merge new records" in {
    val aliceRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(aliceLocation, Some(now), lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)

    val charlieRecord: ScheduleRecord[LocalFileLocation] =
      ScheduleRecord(charlieLocation, Some(now), lastWeek)
    val dannyRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(dannyLocation, None, now)

    val mergedSchedule: Schedule[LocalFileLocation] =
      Schedule(List(aliceRecord, bobRecord)).merge(List(charlieRecord, dannyRecord))
    mergedSchedule.records should contain theSameElementsAs List(aliceRecord, bobRecord, charlieRecord, dannyRecord)
  }

  it should "merge records" in {
    val aliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(now), lastWeek)
    val bobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, None, now)
    val charlieRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(charlieLocation, Some(now), lastWeek)

    val newAliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(tomorrow), now)
    val newBobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, Some(yesterday), yesterday)

    val mergedSchedule: Schedule[LocalFileLocation] =
      Schedule(List(aliceRecord, bobRecord, charlieRecord)).merge(List(newAliceRecord, newBobRecord))
    val expectedAliceRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(aliceLocation, Some(tomorrow), now)
    val expectedBobRecord: ScheduleRecord[LocalFileLocation] = ScheduleRecord(bobLocation, Some(yesterday), now)

    mergedSchedule.records should contain theSameElementsAs List(expectedAliceRecord, expectedBobRecord, charlieRecord)
  }
}

object MusicRepositorySchedulerTest {
  val now = Instant.now()
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
