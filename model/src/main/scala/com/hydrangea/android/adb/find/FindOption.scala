package com.hydrangea.android.adb.find

import java.time.Instant

import com.hydrangea.file.AbsolutePath

sealed trait FindOption {
  def param: String
  def value: String
}

object FindOption {
  def secondsSince(since: Instant): Long = Instant.now().getEpochSecond - since.getEpochSecond
}

case class ByName(name: String) extends FindOption {
  val param: String = "-name"
  val value: String = name.replace("*", "\\*")
}

case class ByPath(path: AbsolutePath) extends FindOption {
  val param = "-path"
  val value: String = path.commandLine.replace("*", "\\*")
}

case class ByCreateDate(secondsAgo: Long) extends FindOption {
  val param = "-ctime"
  val value: String = "$-{secondsAgo}s"
}

object ByCreateDate {
  def apply(since: Instant): ByCreateDate = ByCreateDate(FindOption.secondsSince(since))
}

case class ByModifyDate(secondsAgo: Long) extends FindOption {
  val param: String = "-mtime"
  val value: String = "-${secondsAgo}s"
}

object ByModifyDate {
  def apply(since: Instant): ByModifyDate = ByModifyDate(FindOption.secondsSince(since))
}

case class FindDepth(maxDepth: Int) extends FindOption {
  val param: String = "-maxdepth"
  val value: String = maxDepth.toString
}

object ForRegularFiles extends FindOption {
  val param: String = "-type"
  val value: String = "f"
}

object ForDirectories extends FindOption {
  val param: String = "-type"
  val value: String = "d"
}
