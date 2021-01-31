package com.hydrangea.repository

import java.nio.file.Path

import argonaut._
import com.google.inject.Inject
import com.hydrangea.AbstractDAO
import com.hydrangea.file.{FileLocation, FileSystemService}

class MusicRepositoryDAO @Inject()(fileSystemService: FileSystemService) extends AbstractDAO(fileSystemService) {
  def persist[L <: FileLocation: EncodeJson: DecodeJson](repository: MusicRepository[L], recordFilePath: Path): Unit =
    persistObj(repository, recordFilePath)

  def load[L <: FileLocation: EncodeJson: DecodeJson](recordFilePath: Path): Option[MusicRepository[L]] =
    loadObj[MusicRepository[L]](recordFilePath)
}
