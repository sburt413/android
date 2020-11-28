package com.hydrangea.music.repository.merge

import com.hydrangea.repository.merge.{TrackAdded, TrackComparison, TrackConflict}

sealed trait TrackComparisonResolution {
  def manualResolution: Boolean
  def comparison: TrackComparison
}

case class Ignore(manualResolution: Boolean, comparison: TrackComparison) extends TrackComparisonResolution

case class CreateTrack(manualResolution: Boolean, comparison: TrackAdded) extends TrackComparisonResolution

case class RemoveTrack(manualResolution: Boolean, comparison: TrackComparison) extends TrackComparisonResolution

case class AcceptSourceChanges(manualResolution: Boolean, comparison: TrackConflict) extends TrackComparisonResolution
