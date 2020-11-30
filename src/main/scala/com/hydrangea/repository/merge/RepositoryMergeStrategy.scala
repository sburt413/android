package com.hydrangea.repository.merge

trait RepositoryMergeStrategy {
  def matchStrategy(trackMatch: TrackMatch): Option[TrackComparisonResolution] = None
  def addedStrategy(added: TrackAdded): Option[TrackComparisonResolution] = None
  def removedStrategy(removed: TrackRemoved): Option[TrackComparisonResolution] = None
  def conflictStrategy(conflict: TrackConflict): Option[TrackComparisonResolution] = None
  def duplicateStrategy(duplicate: DuplicateTrack): Option[TrackComparisonResolution] = None

  def resolve(comparison: TrackComparison): Option[TrackComparisonResolution] =
    comparison match {
      case trackMatch: TrackMatch         => matchStrategy(trackMatch)
      case added: TrackAdded              => addedStrategy(added)
      case removed: TrackRemoved          => removedStrategy(removed)
      case conflict: TrackConflict        => conflictStrategy(conflict)
      case duplicateTrack: DuplicateTrack => duplicateStrategy(duplicateTrack)
    }

  def resolve(comparisons: Set[TrackComparison]): Set[TrackComparisonResolution] =
    comparisons.flatMap(resolve)
}

object RepositoryMergeStrategy {
  def apply(matchStrategy: TrackMatch => Option[TrackComparisonResolution] = _ => None,
            addedStrategy: TrackAdded => Option[TrackComparisonResolution] = _ => None,
            removedStrategy: TrackRemoved => Option[TrackComparisonResolution] = _ => None,
            conflictStrategy: TrackConflict => Option[TrackComparisonResolution] = _ => None,
            duplicateStrategy: DuplicateTrack => Option[TrackComparisonResolution] = _ => None) =
    BasicRepositoryMergeStrategy(matchStrategy, addedStrategy, removedStrategy, conflictStrategy, duplicateStrategy)

  private def ignoreMatch(trackMatch: TrackMatch): Option[TrackComparisonResolution] =
    Some(Ignore(manualResolution = false, trackMatch))
  private def acceptAddition(trackAdded: TrackAdded): Option[TrackComparisonResolution] =
    Some(CreateTrack(manualResolution = false, trackAdded))
  private def ignoreDuplicates(duplicateTrack: DuplicateTrack): Option[TrackComparisonResolution] =
    Some(Ignore(manualResolution = false, duplicateTrack))

  val IgnoreMatches = RepositoryMergeStrategy(matchStrategy = ignoreMatch)
  val IgnoreMatchesAcceptAdditions = IgnoreMatches.copy(addedStrategy = acceptAddition)
  val BasicStrategy = IgnoreMatchesAcceptAdditions.copy(duplicateStrategy = ignoreDuplicates)
}

case class BasicRepositoryMergeStrategy(
    matchStrategy: TrackMatch => Option[TrackComparisonResolution] = _ => None,
    addedStrategy: TrackAdded => Option[TrackComparisonResolution] = _ => None,
    removedStrategy: TrackRemoved => Option[TrackComparisonResolution] = _ => None,
    conflictStrategy: TrackConflict => Option[TrackComparisonResolution] = _ => None,
    duplicateStrategy: DuplicateTrack => Option[TrackComparisonResolution] = _ => None)
    extends RepositoryMergeStrategy {
  override def matchStrategy(trackMatch: TrackMatch): Option[TrackComparisonResolution] =
    matchStrategy.apply(trackMatch)

  override def addedStrategy(added: TrackAdded): Option[TrackComparisonResolution] = addedStrategy.apply(added)

  override def removedStrategy(removed: TrackRemoved): Option[TrackComparisonResolution] =
    removedStrategy.apply(removed)

  override def conflictStrategy(conflict: TrackConflict): Option[TrackComparisonResolution] =
    conflictStrategy.apply(conflict)

  override def duplicateStrategy(duplicate: DuplicateTrack): Option[TrackComparisonResolution] =
    duplicateStrategy.apply(duplicate)
}
