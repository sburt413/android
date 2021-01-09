package com.hydrangea.repository.merge

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.hydrangea.file.AbsolutePath
import com.hydrangea.music.track.{Tag, Track}
import com.hydrangea.repository.{RepositoryRecord, merge}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

// should have matcher doesn't look as good without Symbol
class RepositoryMergeTest extends AnyFlatSpec {
  import RepositoryMergeTest._

  "RepositoryMerge" should "track resolved and unresolved resolutions" in {
    val added = TrackAdded(aliceRecord)
    val removed = TrackRemoved(bobRecord)

    val initialMerge = merge.RepositoryMerge(Set(added, removed), Set.empty[TrackComparisonResolution])
    initialMerge.comparisons should equal(Set(added, removed))
    initialMerge.resolutions should be(Symbol("empty"))
    initialMerge.unresolvedComparisons should be(Set(added, removed))
    initialMerge.resolved should be(false)

    val ignoreAdded = Ignore(manualResolution = false, added)
    val addedResolved: RepositoryMerge = initialMerge.resolve(ignoreAdded)
    addedResolved.comparisons should equal(Set(added, removed))
    addedResolved.resolutions should be(Set(ignoreAdded))
    addedResolved.unresolvedComparisons should be(Set(removed))
    addedResolved.resolved should be(false)

    val removeRemoved = RemoveTrack(manualResolution = true, removed)
    val allResolved: RepositoryMerge = addedResolved.resolve(removeRemoved)
    allResolved.comparisons should equal(Set(added, removed))
    allResolved.resolutions should be(Set(ignoreAdded, removeRemoved))
    allResolved.unresolvedComparisons should be(Set.empty)
    allResolved.resolved should be(true)
  }

  it should "use a track match strategy" in {
    val matched = TrackMatch(aliceRecord)

    val ignoreMatched =
      RepositoryMergeStrategy(matchStrategy = comparison => Some(Ignore(manualResolution = false, comparison)))
    val ignoreMerge = RepositoryMerge(Set(matched), ignoreMatched)
    ignoreMerge.comparisons should equal(Set(matched))
    ignoreMerge.resolutions should equal(Set(Ignore(manualResolution = false, matched)))
    ignoreMerge.unresolvedComparisons should be(Symbol("empty"))
    ignoreMerge.resolved should be(true)

    val unrelated = TrackAdded(bobRecord)
    val unresolvedMerge = RepositoryMerge(Set(matched, unrelated), ignoreMatched)
    unresolvedMerge.comparisons should equal(Set(matched, unrelated))
    unresolvedMerge.resolutions should equal(Set(Ignore(manualResolution = false, matched)))
    unresolvedMerge.unresolvedComparisons should be(Set(unrelated))
    unresolvedMerge.resolved should be(false)

    val noResolution = RepositoryMergeStrategy(matchStrategy = _ => None)
    val noResolutionMerge = RepositoryMerge(Set(matched), noResolution)
    noResolutionMerge.comparisons should equal(Set(matched))
    noResolutionMerge.resolutions should be(Symbol("empty"))
    noResolutionMerge.unresolvedComparisons should be(Set(matched))
    noResolutionMerge.resolved should be(false)
  }

  it should "use a track added strategy" in {
    val added = TrackAdded(aliceRecord)

    val ignoreAdded =
      RepositoryMergeStrategy(addedStrategy = comparison => Some(Ignore(manualResolution = false, comparison)))
    val ignoreMerge = RepositoryMerge(Set(added), ignoreAdded)
    ignoreMerge.comparisons should equal(Set(added))
    ignoreMerge.resolutions should equal(Set(Ignore(manualResolution = false, added)))
    ignoreMerge.unresolvedComparisons should be(Symbol("empty"))
    ignoreMerge.resolved should be(true)

    val createAdded =
      RepositoryMergeStrategy(addedStrategy = comparison => Some(CreateTrack(manualResolution = false, comparison)))
    val createMerge = RepositoryMerge(Set(added), createAdded)
    createMerge.comparisons should equal(Set(added))
    createMerge.resolutions should equal(Set(CreateTrack(manualResolution = false, added)))
    createMerge.unresolvedComparisons should be(Symbol("empty"))
    createMerge.resolved should be(true)

    val unrelated = TrackMatch(bobRecord)
    val unresolvedMerge = RepositoryMerge(Set(added, unrelated), createAdded)
    unresolvedMerge.comparisons should equal(Set(added, unrelated))
    unresolvedMerge.resolutions should equal(Set(CreateTrack(manualResolution = false, added)))
    unresolvedMerge.unresolvedComparisons should be(Set(unrelated))
    unresolvedMerge.resolved should be(false)

    val noResolution = RepositoryMergeStrategy(addedStrategy = _ => None)
    val noResolutionMerge = RepositoryMerge(Set(added), noResolution)
    noResolutionMerge.comparisons should equal(Set(added))
    noResolutionMerge.resolutions should be(Symbol("empty"))
    noResolutionMerge.unresolvedComparisons should be(Set(added))
    noResolutionMerge.resolved should be(false)
  }

  it should "use a track removed strategy" in {
    val removed = TrackRemoved(aliceRecord)

    val ignoreRemoved =
      RepositoryMergeStrategy(removedStrategy = comparison => Some(Ignore(manualResolution = false, comparison)))
    val ignoreMerge = RepositoryMerge(Set(removed), ignoreRemoved)
    ignoreMerge.comparisons should equal(Set(removed))
    ignoreMerge.resolutions should equal(Set(Ignore(manualResolution = false, removed)))
    ignoreMerge.unresolvedComparisons should be(Symbol("empty"))
    ignoreMerge.resolved should be(true)

    val removeRemoved =
      RepositoryMergeStrategy(removedStrategy = comparison => Some(RemoveTrack(manualResolution = false, comparison)))
    val createMerge = RepositoryMerge(Set(removed), removeRemoved)
    createMerge.comparisons should equal(Set(removed))
    createMerge.resolutions should equal(Set(RemoveTrack(manualResolution = false, removed)))
    createMerge.unresolvedComparisons should be(Symbol("empty"))
    createMerge.resolved should be(true)

    val unrelated = TrackMatch(bobRecord)
    val unresolvedMerge = RepositoryMerge(Set(removed, unrelated), removeRemoved)
    unresolvedMerge.comparisons should equal(Set(removed, unrelated))
    unresolvedMerge.resolutions should equal(Set(RemoveTrack(manualResolution = false, removed)))
    unresolvedMerge.unresolvedComparisons should be(Set(unrelated))
    unresolvedMerge.resolved should be(false)

    val noResolution = RepositoryMergeStrategy(removedStrategy = _ => None)
    val noResolutionMerge = RepositoryMerge(Set(removed), noResolution)
    noResolutionMerge.comparisons should equal(Set(removed))
    noResolutionMerge.resolutions should be(Symbol("empty"))
    noResolutionMerge.unresolvedComparisons should be(Set(removed))
    noResolutionMerge.resolved should be(false)
  }

  it should "use a track conflicted strategy" in {
    val alternateAliceTrack: Track = aliceTrack.copy(hash = "aaaa", tag = aliceTag.copy(title = "Alice Track 2"))
    val alternateAliceRecord = RepositoryRecord(repositoryRoot, alternateAliceTrack)
    val conflict = TrackConflict(aliceRecord, alternateAliceRecord)

    val ignoreConflict =
      RepositoryMergeStrategy(conflictStrategy = comparison => Some(Ignore(manualResolution = false, comparison)))
    val ignoreMerge = RepositoryMerge(Set(conflict), ignoreConflict)
    ignoreMerge.comparisons should equal(Set(conflict))
    ignoreMerge.resolutions should equal(Set(Ignore(manualResolution = false, conflict)))
    ignoreMerge.unresolvedComparisons should be(Symbol("empty"))
    ignoreMerge.resolved should be(true)

    val acceptSourceStrategy =
      RepositoryMergeStrategy(
        conflictStrategy = comparison => Some(AcceptSourceChanges(manualResolution = false, comparison)))
    val acceptSourceMerge = RepositoryMerge(Set(conflict), acceptSourceStrategy)
    acceptSourceMerge.comparisons should equal(Set(conflict))
    acceptSourceMerge.resolutions should equal(Set(AcceptSourceChanges(manualResolution = false, conflict)))
    acceptSourceMerge.unresolvedComparisons should be(Symbol("empty"))
    acceptSourceMerge.resolved should be(true)

    val unrelated = TrackMatch(bobRecord)
    val unresolvedMerge = RepositoryMerge(Set(conflict, unrelated), acceptSourceStrategy)
    unresolvedMerge.comparisons should equal(Set(conflict, unrelated))
    unresolvedMerge.resolutions should equal(Set(AcceptSourceChanges(manualResolution = false, conflict)))
    unresolvedMerge.unresolvedComparisons should be(Set(unrelated))
    unresolvedMerge.resolved should be(false)

    val noResolution = RepositoryMergeStrategy(conflictStrategy = _ => None)
    val noResolutionMerge = RepositoryMerge(Set(conflict), noResolution)
    noResolutionMerge.comparisons should equal(Set(conflict))
    noResolutionMerge.resolutions should be(Symbol("empty"))
    noResolutionMerge.unresolvedComparisons should be(Set(conflict))
    noResolutionMerge.resolved should be(false)
  }

  it should "use a track duplicate strategy" in {
    val duplicateAliceTrack: Track = aliceTrack.copy(path = AbsolutePath.unixPath("/alice2"))
    val duplicateAliceRecord = RepositoryRecord(repositoryRoot, duplicateAliceTrack)
    val duplicate = DuplicateTrack(aliceRecord, duplicateAliceRecord)

    val ignoreDuplicate =
      RepositoryMergeStrategy(duplicateStrategy = comparison => Some(Ignore(manualResolution = false, comparison)))
    val ignoreMerge = RepositoryMerge(Set(duplicate), ignoreDuplicate)
    ignoreMerge.comparisons should equal(Set(duplicate))
    ignoreMerge.resolutions should equal(Set(Ignore(manualResolution = false, duplicate)))
    ignoreMerge.unresolvedComparisons should be(Symbol("empty"))
    ignoreMerge.resolved should be(true)

    val removeTrackStrategy =
      RepositoryMergeStrategy(duplicateStrategy = comparison => Some(RemoveTrack(manualResolution = false, comparison)))
    val removeTrackMerge = RepositoryMerge(Set(duplicate), removeTrackStrategy)
    removeTrackMerge.comparisons should equal(Set(duplicate))
    removeTrackMerge.resolutions should equal(Set(RemoveTrack(manualResolution = false, duplicate)))
    removeTrackMerge.unresolvedComparisons should be(Symbol("empty"))
    removeTrackMerge.resolved should be(true)

    val unrelated = TrackMatch(bobRecord)
    val unresolvedMerge = RepositoryMerge(Set(duplicate, unrelated), removeTrackStrategy)
    unresolvedMerge.comparisons should equal(Set(duplicate, unrelated))
    unresolvedMerge.resolutions should equal(Set(RemoveTrack(manualResolution = false, duplicate)))
    unresolvedMerge.unresolvedComparisons should be(Set(unrelated))
    unresolvedMerge.resolved should be(false)

    val noResolution = RepositoryMergeStrategy(duplicateStrategy = _ => None)
    val noResolutionMerge = RepositoryMerge(Set(duplicate), noResolution)
    noResolutionMerge.comparisons should equal(Set(duplicate))
    noResolutionMerge.resolutions should be(Symbol("empty"))
    noResolutionMerge.unresolvedComparisons should be(Set(duplicate))
    noResolutionMerge.resolved should be(false)
  }

  it should "resolve with multiple strategies" in {
    // All strategies, same strategy multiple times
    val aliceMatch = TrackMatch(aliceRecord)
    val charlieAdded = TrackAdded(charlieRecord)
    val dannyRemoved = TrackRemoved(dannyRecord)

    val alternateAliceTrack: Track = aliceTrack.copy(hash = "aaaa", tag = aliceTag.copy(title = "Alice Track 2"))
    val alternateAliceRecord = RepositoryRecord(repositoryRoot, alternateAliceTrack)
    val aliceConflict = TrackConflict(aliceRecord, alternateAliceRecord)

    val duplicateBobTrack: Track = bobTrack.copy(path = AbsolutePath.unixPath("/bob2"))
    val duplicateBobRecord = RepositoryRecord(repositoryRoot, duplicateBobTrack)
    val bobDuplicate = DuplicateTrack(aliceRecord, duplicateBobRecord)

    val eveAdded = TrackAdded(eveRecord)

    val strategy =
      RepositoryMergeStrategy(
        matchStrategy = comparison => Some(Ignore(manualResolution = false, comparison)),
        addedStrategy = comparison => Some(CreateTrack(manualResolution = false, comparison)),
        removedStrategy = comparison => Some(RemoveTrack(manualResolution = false, comparison)),
        conflictStrategy = comparison => Some(AcceptSourceChanges(manualResolution = false, comparison)),
        duplicateStrategy = comparison => Some(RemoveTrack(manualResolution = false, comparison))
      )
    val merge =
      RepositoryMerge(Set(aliceMatch, charlieAdded, dannyRemoved, aliceConflict, bobDuplicate, eveAdded), strategy)
    merge.comparisons should equal(Set(aliceMatch, charlieAdded, dannyRemoved, aliceConflict, bobDuplicate, eveAdded))
    val expectedResolutions: Set[TrackComparisonResolution] =
      Set(
        Ignore(manualResolution = false, aliceMatch),
        CreateTrack(manualResolution = false, charlieAdded),
        RemoveTrack(manualResolution = false, dannyRemoved),
        AcceptSourceChanges(manualResolution = false, aliceConflict),
        RemoveTrack(manualResolution = false, bobDuplicate),
        CreateTrack(manualResolution = false, eveAdded),
      )
    merge.resolutions should be(expectedResolutions)
    merge.unresolvedComparisons should be(Symbol("empty"))
    merge.resolved should be(true)
  }
}

object RepositoryMergeTest {
  private val now: Instant = Instant.now()
  private val yesterday: Instant = now.minus(1, ChronoUnit.DAYS)
  private val tomorrow: Instant = now.plus(1, ChronoUnit.DAYS)

  private val repositoryRoot = AbsolutePath.unixPath("/")

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

  val eveTag = Tag("Eve Title", "Eve Track", "Eve", Some(2004), Some(5), Some(12), Some(1), Some(1))
  val eveTrack = Track("5555", AbsolutePath.unixPath("/eve"), now, eveTag)
  val eveRecord = RepositoryRecord(repositoryRoot, eveTrack)
}
