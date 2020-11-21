package com.hydrangea.music.track.merge

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.hydrangea.android.file.AndroidPath
import com.hydrangea.music.track.{Tag, Track, merge}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TrackComparisonTest extends AnyFlatSpec {
  import TrackComparisonTest._

  "TrackConflict" should "determine types of conflicts" in {
    val tagConflictTrack: Track = aliceTrack.copy(tag = aliceTag.copy("Alice Track 2"))
    val contentConflictTrack: Track = aliceTrack.copy(hash = "aaaa")
    val tagAndContentConflictTrack: Track = aliceTrack.copy(hash = "aaaa", tag = aliceTag.copy("Alice Track 2"))

    // Not a true conflict
    TrackConflict(aliceTrack, aliceTrack).conflictingTags should equal(false)
    merge.TrackConflict(aliceTrack, aliceTrack).conflictingContent should equal(false)

    val tagConflict: TrackConflict = merge.TrackConflict(aliceTrack, tagConflictTrack)
    tagConflict.conflictingTags should equal(true)
    tagConflict.conflictingContent should equal(false)
    tagConflict.isSymmetricWith(merge.TrackConflict(tagConflictTrack, aliceTrack)) should equal(true)
    tagConflict.isSymmetricWith(merge.TrackConflict(bobTrack, aliceTrack)) should equal(false)

    val contentConflict: TrackConflict = merge.TrackConflict(aliceTrack, contentConflictTrack)
    contentConflict.conflictingTags should equal(false)
    contentConflict.conflictingContent should equal(true)
    contentConflict.isSymmetricWith(merge.TrackConflict(contentConflictTrack, aliceTrack)) should equal(true)
    contentConflict.isSymmetricWith(merge.TrackConflict(bobTrack, aliceTrack)) should equal(false)

    val tagAndContentConflict: TrackConflict = merge.TrackConflict(aliceTrack, tagAndContentConflictTrack)
    tagAndContentConflict.conflictingTags should equal(true)
    tagAndContentConflict.conflictingContent should equal(true)
    tagAndContentConflict.isSymmetricWith(merge.TrackConflict(tagAndContentConflictTrack, aliceTrack)) should equal(
      true)
    tagAndContentConflict.isSymmetricWith(merge.TrackConflict(bobTrack, aliceTrack)) should equal(false)
  }

  "DuplicateTrack" should "determine types of duplicates" in {
    val duplicateContentTrack: Track =
      aliceTrack.copy(path = AndroidPath("/alice2"), tag = aliceTag.copy("Alice Track 2"))
    val duplicateTagTrack: Track = aliceTrack.copy(path = AndroidPath("/alice2"), hash = "aaaa")
    val unrelatedTrack: Track =
      aliceTrack.copy(path = AndroidPath("/alice2"), hash = "aaaa", tag = aliceTag.copy("Alice Track 2"))

    // Not a true duplicate
    DuplicateTrack(aliceTrack, aliceTrack).duplicateContent should equal(true)
    merge.DuplicateTrack(aliceTrack, aliceTrack).duplicateTags should equal(true)

    val duplicateContent: DuplicateTrack = merge.DuplicateTrack(aliceTrack, duplicateContentTrack)
    duplicateContent.duplicateTags should equal(false)
    duplicateContent.duplicateContent should equal(true)
    duplicateContent.isSymmetricWith(merge.DuplicateTrack(duplicateContentTrack, aliceTrack)) should equal(true)
    duplicateContent.isSymmetricWith(merge.DuplicateTrack(bobTrack, aliceTrack)) should equal(false)

    val duplicateTag: DuplicateTrack = merge.DuplicateTrack(aliceTrack, duplicateTagTrack)
    duplicateTag.duplicateTags should equal(true)
    duplicateTag.duplicateContent should equal(false)
    duplicateTag.isSymmetricWith(merge.DuplicateTrack(duplicateTagTrack, aliceTrack)) should equal(true)
    duplicateTag.isSymmetricWith(merge.DuplicateTrack(bobTrack, aliceTrack)) should equal(false)

    val unrelatedDuplicate: DuplicateTrack = merge.DuplicateTrack(aliceTrack, unrelatedTrack)
    unrelatedDuplicate.duplicateTags should equal(false)
    unrelatedDuplicate.duplicateContent should equal(false)
    unrelatedDuplicate.isSymmetricWith(merge.DuplicateTrack(unrelatedTrack, aliceTrack)) should equal(true)
    unrelatedDuplicate.isSymmetricWith(merge.DuplicateTrack(bobTrack, aliceTrack)) should equal(false)
  }

  "TrackComparison" should "compare tracks with empty sets" in {
    val lhsAddComparison: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack), Set.empty)
    lhsAddComparison should equal(Set(TrackAdded(aliceTrack)))

    val lhsAddComparisons: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack), Set.empty)
    lhsAddComparisons should equal(Set(merge.TrackAdded(aliceTrack), merge.TrackAdded(bobTrack)))

    val rhsAddComparison: Set[TrackComparison] = TrackComparison.compare(Set.empty, Set(aliceTrack))
    rhsAddComparison should equal(Set(TrackRemoved(aliceTrack)))

    val rhsAddComparisons: Set[TrackComparison] = TrackComparison.compare(Set.empty, Set(aliceTrack, bobTrack))
    rhsAddComparisons should equal(Set(merge.TrackRemoved(aliceTrack), merge.TrackRemoved(bobTrack)))
  }

  it should "compare different tracks" in {
    val actual: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack), Set(charlieTrack, dannyTrack))
    val expected: Set[TrackComparison] =
      Set(aliceTrack, bobTrack).map(TrackAdded) ++ Set(charlieTrack, dannyTrack).map(TrackRemoved)
    actual should equal(expected)
  }

  it should "recognize matches" in {
    val actual: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack), Set(aliceTrack, bobTrack))
    val expected: Set[TrackComparison] = Set(aliceTrack, bobTrack).map(TrackMatch)
    actual should equal(expected)
  }

  it should "recognize conflict in tags" in {
    val aliceTrack2: Track = aliceTrack.copy(tag = aliceTag.copy(title = "Alice Track 2"))
    val bobTrack2: Track = bobTrack.copy(tag = bobTag.copy(title = "Bob Track 2"))
    val actual: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack), Set(aliceTrack2, bobTrack2))
    val expected: Set[TrackComparison] =
      Set(merge.TrackConflict(aliceTrack, aliceTrack2), merge.TrackConflict(bobTrack, bobTrack2))

    assert(actual.forall({
      case conflict: TrackConflict => conflict.conflictingTags && !conflict.conflictingContent
      case _                       => false
    }))
    actual should equal(expected)
  }

  it should "recognize conflicts in content" in {
    val aliceTrack2: Track = aliceTrack.copy(hash = "aaaa")
    val bobTrack2: Track = bobTrack.copy(hash = "bbbb")

    val actual: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack), Set(aliceTrack2, bobTrack2))
    val expected: Set[TrackComparison] =
      Set(merge.TrackConflict(aliceTrack, aliceTrack2), merge.TrackConflict(bobTrack, bobTrack2))

    assert(actual.forall({
      case conflict: TrackConflict => !conflict.conflictingTags && conflict.conflictingContent
      case _                       => false
    }))
    actual should equal(expected)
  }

  it should "recognize conflicts in tags and content" in {
    val aliceTrack2: Track = aliceTrack.copy(hash = "aaaa", tag = aliceTag.copy(title = "Alice Track 2"))
    val bobTrack2: Track = bobTrack.copy(hash = "bbbb", tag = bobTag.copy(title = "Bob Track 2"))

    val actual: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack), Set(aliceTrack2, bobTrack2))
    val expected: Set[TrackComparison] =
      Set(merge.TrackConflict(aliceTrack, aliceTrack2), merge.TrackConflict(bobTrack, bobTrack2))

    assert(actual.forall({
      case conflict: TrackConflict => conflict.conflictingTags && conflict.conflictingContent
      case _                       => false
    }))
    actual should equal(expected)
  }

  it should "recognize duplicates" in {
    val aliceTrackDuplicate: Track = aliceTrack.copy(path = AndroidPath("/alice2"))
    val bobTrackDuplicate: Track = bobTrack.copy(path = AndroidPath("/bob2"))

    val actual: Set[TrackComparison] =
      TrackComparison.compare(Set(aliceTrack, bobTrack), Set(aliceTrackDuplicate, bobTrackDuplicate))
    val expected: Set[TrackComparison] =
      Set(merge.DuplicateTrack(aliceTrack, aliceTrackDuplicate), merge.DuplicateTrack(bobTrack, bobTrackDuplicate))

    actual should equal(expected)
  }

  it should "differentiate added, matching, conflicted and duplicate tracks" in {
    val charlieTrack2: Track = charlieTrack.copy(tag = charlieTag.copy(title = "Charlie Track 2"))
    val dannyTrackDuplicate: Track = dannyTrack.copy(path = AndroidPath("/danny2"))
    val comparisons: Set[TrackComparison] = TrackComparison.compare(Set(aliceTrack, bobTrack, charlieTrack, dannyTrack),
                                                                    Set(bobTrack, charlieTrack2, dannyTrackDuplicate))

    comparisons should have size 4
    comparisons should contain(merge.TrackAdded(aliceTrack))
    comparisons should contain(merge.TrackMatch(bobTrack))
    comparisons should contain(merge.TrackConflict(charlieTrack, charlieTrack2))
    comparisons should contain(merge.DuplicateTrack(dannyTrack, dannyTrackDuplicate))
  }
}

object TrackComparisonTest {
  private val now: Instant = Instant.now()
  private val yesterday: Instant = now.minus(1, ChronoUnit.DAYS)
  private val tomorrow: Instant = now.plus(1, ChronoUnit.DAYS)

  val aliceTag = Tag("Alice Title", "Alice Track", "Alice", Some(2000), Some(1), Some(10), Some(1), Some(2))
  val aliceTrack = Track("1111", AndroidPath("/alice"), now, aliceTag)

  val bobTag = Tag("Bob Title", "Bob Track", "Bob", Some(2001), Some(2), Some(20), Some(2), Some(2))
  val bobTrack = Track("2222", AndroidPath("/bob"), yesterday, bobTag)

  val charlieTag = Tag("Charlie Title", "Charlie Track", "Charlie", Some(2002), Some(3), Some(10), Some(1), Some(1))
  val charlieTrack = Track("3333", AndroidPath("/charlie"), tomorrow, charlieTag)

  val dannyTag = Tag("Danny Title", "Danny Track", "Danny", Some(2003), Some(4), Some(10), Some(1), Some(1))
  val dannyTrack = Track("4444", AndroidPath("/danny"), now, dannyTag)
}
