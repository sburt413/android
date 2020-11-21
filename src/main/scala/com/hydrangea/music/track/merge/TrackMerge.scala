package com.hydrangea.music.track.merge

import com.hydrangea.music.track.Track

case class TrackMerge(comparisons: Set[TrackComparison], resolutions: Set[TrackComparisonResolution]) {
  val unresolvedComparisons: Set[TrackComparison] = comparisons.removedAll(resolutions.map(_.comparison))
  val resolved: Boolean = unresolvedComparisons.isEmpty

  def resolve(resolution: TrackComparisonResolution): TrackMerge =
    resolve(Set(resolution))

  def resolve(resolutions: Set[TrackComparisonResolution]): TrackMerge =
    copy(comparisons, this.resolutions ++ resolutions)
}

object TrackMerge {
  def apply(sourceTracks: Set[Track], destinationTracks: Set[Track], mergeStrategy: TrackMergeStrategy): TrackMerge = {
    val comparisons: Set[TrackComparison] = TrackComparison.compare(sourceTracks, destinationTracks)
    TrackMerge(comparisons, mergeStrategy)
  }

  def apply(comparisons: Set[TrackComparison], mergeStrategy: TrackMergeStrategy): TrackMerge = {
    val automaticResolutions: Set[TrackComparisonResolution] = mergeStrategy.resolve(comparisons)
    TrackMerge(comparisons, automaticResolutions)
  }
}
