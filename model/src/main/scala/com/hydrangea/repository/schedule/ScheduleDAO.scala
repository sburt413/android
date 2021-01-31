package com.hydrangea.repository.schedule

import java.nio.file.Path

import argonaut.{DecodeJson, EncodeJson}
import com.google.inject.Inject
import com.hydrangea.AbstractDAO
import com.hydrangea.file.{FileLocation, FileSystemService}

class ScheduleDAO @Inject()(fileSystemService: FileSystemService) extends AbstractDAO(fileSystemService) {
  def persist[L <: FileLocation: EncodeJson: DecodeJson](repository: Schedule[L], recordFilePath: Path): Unit =
    persistObj(repository, recordFilePath)

  def load[L <: FileLocation: EncodeJson: DecodeJson](recordFilePath: Path): Option[Schedule[L]] =
    loadObj[Schedule[L]](recordFilePath)
}
