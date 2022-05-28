package com.hydrangea.repository

import argonaut._
import com.google.inject.Inject
import com.hydrangea.AbstractDAO
import com.hydrangea.file.{FileLocation, FileSystemService}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path

class MusicRepositoryDAO @Inject()(fileSystemService: FileSystemService) extends AbstractDAO(fileSystemService) {
  import MusicRepositoryDAO.logger

  def persist[L <: FileLocation: EncodeJson: DecodeJson](repository: MusicRepository[L], recordFilePath: Path): Unit = {
    logger.info(s"Writing MusicRepository to $recordFilePath")
    persistObj(repository, recordFilePath)
  }

  def load[L <: FileLocation: EncodeJson: DecodeJson](recordFilePath: Path): Option[MusicRepository[L]] =
    loadObj[MusicRepository[L]](recordFilePath)
}

object MusicRepositoryDAO {
  private[repository] val logger: Logger = LoggerFactory.getLogger(MusicRepositoryDAO.getClass)
}
