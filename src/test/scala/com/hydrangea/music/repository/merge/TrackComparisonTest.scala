package com.hydrangea.music.repository.merge

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.hydrangea.file.AbsolutePath
import com.hydrangea.music.track.{Tag, Track}
import com.hydrangea.repository.RepositoryRecord
import com.hydrangea.repository.merge.{
  DuplicateTrack,
  TrackAdded,
  TrackComparison,
  TrackConflict,
  TrackMatch,
  TrackRemoved
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TrackComparisonTest extends AnyFlatSpec {
  import TrackComparisonTest._

  "TrackConflict" should "determine types of conflicts" in {
    val tagConflictTrack: Track = aliceTrack.copy(tag = aliceTag.copy("Alice Track 2"))
    val tagConflictRecord: RepositoryRecord = RepositoryRecord(repositoryRoot, tagConflictTrack)

    val contentConflictTrack: Track = aliceTrack.copy(hash = "aaaa")
    val contentConflictRecord: RepositoryRecord = RepositoryRecord(repositoryRoot, contentConflictTrack)

    val tagAndContentConflictTrack: Track = aliceTrack.copy(hash = "aaaa", tag = aliceTag.copy("Alice Track 2"))
    val tagAndContentConflictRecord: RepositoryRecord = RepositoryRecord(repositoryRoot, tagAndContentConflictTrack)

    // Not a true conflict
    TrackConflict(aliceRecord, aliceRecord).conflictingTags should equal(false)
    TrackConflict(aliceRecord, aliceRecord).conflictingContent should equal(false)

    val tagConflict: TrackConflict = TrackConflict(aliceRecord, tagConflictRecord)
    tagConflict.conflictingTags should equal(true)
    tagConflict.conflictingContent should equal(false)
    tagConflict.isSymmetricWith(TrackConflict(tagConflictRecord, aliceRecord)) should equal(true)
    tagConflict.isSymmetricWith(TrackConflict(bobRecord, aliceRecord)) should equal(false)

    val contentConflict: TrackConflict = TrackConflict(aliceRecord, contentConflictRecord)
    contentConflict.conflictingTags should equal(false)
    contentConflict.conflictingContent should equal(true)
    contentConflict.isSymmetricWith(TrackConflict(contentConflictRecord, aliceRecord)) should equal(true)
    contentConflict.isSymmetricWith(TrackConflict(bobRecord, aliceRecord)) should equal(false)

    val tagAndContentConflict: TrackConflict = TrackConflict(aliceRecord, tagAndContentConflictRecord)
    tagAndContentConflict.conflictingTags should equal(true)
    tagAndContentConflict.conflictingContent should equal(true)
    tagAndContentConflict.isSymmetricWith(TrackConflict(tagAndContentConflictRecord, aliceRecord)) should equal(true)
    tagAndContentConflict.isSymmetricWith(TrackConflict(bobRecord, aliceRecord)) should equal(false)
  }

  "DuplicateTrack" should "determine types of duplicates" in {
    val duplicateContentTrack: Track =
      aliceTrack.copy(path = AbsolutePath.unixPath("/alice2"), tag = aliceTag.copy("Alice Track 2"))
    val duplicateContentRecord = RepositoryRecord(repositoryRoot, duplicateContentTrack)

    val duplicateTagTrack: Track = aliceTrack.copy(path = AbsolutePath.unixPath("/alice2"), hash = "aaaa")
    val duplicateTagRecord = RepositoryRecord(repositoryRoot, duplicateTagTrack)

    val unrelatedTrack: Track =
      aliceTrack.copy(path = AbsolutePath.unixPath("/alice2"), hash = "aaaa", tag = aliceTag.copy("Alice Track 2"))
    val unrelatedRecord = RepositoryRecord(repositoryRoot, unrelatedTrack)

    DuplicateTrack(aliceRecord, aliceRecord).duplicateContent should equal(true)
    DuplicateTrack(aliceRecord, aliceRecord).duplicateTags should equal(true)

    val duplicateContent: DuplicateTrack = DuplicateTrack(aliceRecord, duplicateContentRecord)
    duplicateContent.duplicateTags should equal(false)
    duplicateContent.duplicateContent should equal(true)
    duplicateContent.isSymmetricWith(DuplicateTrack(duplicateContentRecord, aliceRecord)) should equal(true)
    duplicateContent.isSymmetricWith(DuplicateTrack(bobRecord, aliceRecord)) should equal(false)

    val duplicateTag: DuplicateTrack = DuplicateTrack(aliceRecord, duplicateTagRecord)
    duplicateTag.duplicateTags should equal(true)
    duplicateTag.duplicateContent should equal(false)
    duplicateTag.isSymmetricWith(DuplicateTrack(duplicateTagRecord, aliceRecord)) should equal(true)
    duplicateTag.isSymmetricWith(DuplicateTrack(bobRecord, aliceRecord)) should equal(false)

    val unrelatedDuplicate: DuplicateTrack = DuplicateTrack(aliceRecord, unrelatedRecord)
    unrelatedDuplicate.duplicateTags should equal(false)
    unrelatedDuplicate.duplicateContent should equal(false)
    unrelatedDuplicate.isSymmetricWith(DuplicateTrack(unrelatedRecord, aliceRecord)) should equal(true)
    unrelatedDuplicate.isSymmetricWith(DuplicateTrack(bobRecord, aliceRecord)) should equal(false)
  }

  "TrackComparison" should "compare tracks with empty sets" in {
    val lhsAddComparison: Set[TrackComparison] = TrackComparison.compare(Set(aliceRecord), Set.empty)
    lhsAddComparison should equal(Set(TrackAdded(aliceRecord)))

    val lhsAddComparisons: Set[TrackComparison] = TrackComparison.compare(Set(aliceRecord, bobRecord), Set.empty)
    lhsAddComparisons should equal(Set(TrackAdded(aliceRecord), TrackAdded(bobRecord)))

    val rhsAddComparison: Set[TrackComparison] = TrackComparison.compare(Set.empty, Set(aliceRecord))
    rhsAddComparison should equal(Set(TrackRemoved(aliceRecord)))

    val rhsAddComparisons: Set[TrackComparison] = TrackComparison.compare(Set.empty, Set(aliceRecord, bobRecord))
    rhsAddComparisons should equal(Set(TrackRemoved(aliceRecord), TrackRemoved(bobRecord)))
  }

  it should "compare different tracks" in {
    val actual: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceRecord, bobRecord), Set(charlieRecord, dannyRecord))
    val expected: Set[TrackComparison] =
      (Set(aliceRecord, bobRecord).map(TrackAdded) ++ Set(charlieRecord, dannyRecord).map(TrackRemoved)).toSet
    actual should equal(expected)
  }

  it should "recognize matches" in {
    val actual: Set[TrackComparison] = TrackComparison.compare(Set(aliceRecord, bobRecord), Set(aliceRecord, bobRecord))
    val expected: Set[TrackComparison] = Set(aliceRecord, bobRecord).map(TrackMatch)
    actual should equal(expected)
  }

  it should "recognize conflict in tags" in {
    val aliceTrack2: Track = aliceTrack.copy(tag = aliceTag.copy(title = "Alice Track 2"))
    val aliceRecord2 = RepositoryRecord(repositoryRoot, aliceTrack2)

    val bobTrack2: Track = bobTrack.copy(tag = bobTag.copy(title = "Bob Track 2"))
    val bobRecord2 = RepositoryRecord(repositoryRoot, bobTrack2)

    val actual: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceRecord, bobRecord), Set(aliceRecord2, bobRecord2))
    val expected: Set[TrackComparison] =
      Set(TrackConflict(aliceRecord, aliceRecord2), TrackConflict(bobRecord, bobRecord2))

    assert(actual.forall({
      case conflict: TrackConflict => conflict.conflictingTags && !conflict.conflictingContent
      case _                       => false
    }))
    actual should equal(expected)
  }

  it should "recognize conflicts in content" in {
    val aliceTrack2: Track = aliceTrack.copy(hash = "aaaa")
    val aliceRecord2 = RepositoryRecord(repositoryRoot, aliceTrack2)

    val bobTrack2: Track = bobTrack.copy(hash = "bbbb")
    val bobRecord2 = RepositoryRecord(repositoryRoot, bobTrack2)

    val actual: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceRecord, bobRecord), Set(aliceRecord2, bobRecord2))
    val expected: Set[TrackComparison] =
      Set(TrackConflict(aliceRecord, aliceRecord2), TrackConflict(bobRecord, bobRecord2))

    assert(actual.forall({
      case conflict: TrackConflict => !conflict.conflictingTags && conflict.conflictingContent
      case _                       => false
    }))
    actual should equal(expected)
  }

  it should "recognize conflicts in tags and content" in {
    val aliceTrack2: Track = aliceTrack.copy(hash = "aaaa", tag = aliceTag.copy(title = "Alice Track 2"))
    val aliceRecord2 = RepositoryRecord(repositoryRoot, aliceTrack2)

    val bobTrack2: Track = bobTrack.copy(hash = "bbbb", tag = bobTag.copy(title = "Bob Track 2"))
    val bobRecord2 = RepositoryRecord(repositoryRoot, bobTrack2)

    val actual: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceRecord, bobRecord), Set(aliceRecord2, bobRecord2))
    val expected: Set[TrackComparison] =
      Set(TrackConflict(aliceRecord, aliceRecord2), TrackConflict(bobRecord, bobRecord2))

    assert(actual.forall({
      case conflict: TrackConflict => conflict.conflictingTags && conflict.conflictingContent
      case _                       => false
    }))
    actual should equal(expected)
  }

  it should "recognize duplicates" in {
    val aliceTrackDuplicate: Track = aliceTrack.copy(path = AbsolutePath.unixPath("/alice2"))
    val aliceDuplicateRecord = RepositoryRecord(repositoryRoot, aliceTrackDuplicate)

    val bobTrackDuplicate: Track = bobTrack.copy(path = AbsolutePath.unixPath("/bob2"))
    val bobDuplicateRecord = RepositoryRecord(repositoryRoot, bobTrackDuplicate)

    val actual: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceRecord, bobRecord), Set(aliceDuplicateRecord, bobDuplicateRecord))
    val expected: Set[TrackComparison] =
      Set(DuplicateTrack(aliceRecord, aliceDuplicateRecord), DuplicateTrack(bobRecord, bobDuplicateRecord))

    actual should equal(expected)
  }

  it should "differentiate added, matching, conflicted and duplicate tracks" in {
    val charlieTrack2: Track = charlieTrack.copy(tag = charlieTag.copy(title = "Charlie Track 2"))
    val charlieRecord2 = RepositoryRecord(repositoryRoot, charlieTrack2)

    val dannyTrackDuplicate: Track = dannyTrack.copy(path = AbsolutePath.unixPath("/danny2"))
    val dannyDuplicateRecord = RepositoryRecord(repositoryRoot, dannyTrackDuplicate)

    val comparisons: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceRecord, bobRecord, charlieRecord, dannyRecord),
                              Set(bobRecord, charlieRecord2, dannyDuplicateRecord))

    comparisons should have size 4
    comparisons should contain(TrackAdded(aliceRecord))
    comparisons should contain(TrackMatch(bobRecord))
    comparisons should contain(TrackConflict(charlieRecord, charlieRecord2))
    comparisons should contain(DuplicateTrack(dannyRecord, dannyDuplicateRecord))
  }
}

object TrackComparisonTest {
  private val now: Instant = Instant.now()
  private val yesterday: Instant = now.minus(1, ChronoUnit.DAYS)
  private val tomorrow: Instant = now.plus(1, ChronoUnit.DAYS)

  val repositoryRoot = AbsolutePath.unixPath("/")

  val aliceTag = Tag("Alice Title", "Alice Track", "Alice", Some(2000), Some(1), Some(10), Some(1), Some(2))
  val aliceTrack = Track("1111", AbsolutePath.unixPath("/alice"), now, aliceTag)
  val aliceRecord = RepositoryRecord(repositoryRoot, aliceTrack)

  val bobTag = Tag("Bob Title", "Bob Track", "Bob", Some(2001), Some(2), Some(20), Some(2), Some(2))
  val bobTrack = Track("2222", AbsolutePath.unixPath("/bob"), yesterday, bobTag)
  val bobRecord = RepositoryRecord(repositoryRoot, bobTrack)

  val charlieTag = Tag("Charlie Title", "Charlie Track", "Charlie", Some(2002), Some(3), Some(10), Some(1), Some(1))
  val charlieTrack = Track("3333", AbsolutePath.unixPath("/charlie"), tomorrow, charlieTag)
  val charlieRecord = RepositoryRecord(repositoryRoot, charlieTrack)

  val dannyTag = Tag("Danny Title", "Danny Track", "Danny", Some(2003), Some(4), Some(10), Some(1), Some(1))
  val dannyTrack = Track("4444", AbsolutePath.unixPath("/danny"), now, dannyTag)
  val dannyRecord = RepositoryRecord(repositoryRoot, dannyTrack)
}
