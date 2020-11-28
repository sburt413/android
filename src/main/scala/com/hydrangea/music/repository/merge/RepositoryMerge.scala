package com.hydrangea.music.repository.merge

import com.hydrangea.music.track.Track
import com.hydrangea.repository.merge.{RepositoryMergeStrategy, TrackComparison}

case class RepositoryMerge(comparisons: Set[TrackComparison], resolutions: Set[TrackComparisonResolution]) {
  val unresolvedComparisons: Set[TrackComparison] = comparisons.removedAll(resolutions.map(_.comparison))
  val resolved: Boolean = unresolvedComparisons.isEmpty

  def resolve(resolution: TrackComparisonResolution): RepositoryMerge =
    resolve(Set(resolution))

  def resolve(resolutions: Set[TrackComparisonResolution]): RepositoryMerge =
    copy(comparisons, this.resolutions ++ resolutions)
}

object RepositoryMerge {
  def apply(sourceTracks: Set[Track],
            destinationTracks: Set[Track],
            mergeStrategy: RepositoryMergeStrategy): RepositoryMerge = {
    val comparisons: Set[TrackComparison] = TrackComparison.compare(sourceTracks, destinationTracks)
    RepositoryMerge(comparisons, mergeStrategy)
  }

  def apply(comparisons: Set[TrackComparison], mergeStrategy: RepositoryMergeStrategy): RepositoryMerge = {
    val automaticResolutions: Set[TrackComparisonResolution] = mergeStrategy.resolve(comparisons)
    RepositoryMerge(comparisons, automaticResolutions)
  }
}
