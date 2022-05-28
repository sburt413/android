package com.hydrangea.repository.schedule

import com.hydrangea.file.{AbsolutePath, LocalFileLocation, RelativePath, UnixPathBase}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.time.Instant
import java.time.temporal.ChronoUnit

class ScheduleTest extends AnyFlatSpec {
  import com.hydrangea.repository.schedule.ScheduleTest._

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

object ScheduleTest {
  // Instant can have nano-second precision, marshalling keeps only mills
  val now: Instant = Instant.now().truncatedTo(ChronoUnit.MILLIS)
  val lastWeek = now.minus(7, ChronoUnit.DAYS)
  val yesterday = now.minus(1, ChronoUnit.DAYS)
  val tomorrow = now.plus(1, ChronoUnit.DAYS)

  val musicRelativePath = RelativePath(Seq("music"))
  val repositoryRoot = LocalFileLocation(AbsolutePath(UnixPathBase, Nil)) ++ musicRelativePath

  val aliceLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("alice"))
  val bobLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("bob"))
  val charlieLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("charlie"))
  val dannyLocation: LocalFileLocation = repositoryRoot ++ RelativePath(Seq("danny"))
}
