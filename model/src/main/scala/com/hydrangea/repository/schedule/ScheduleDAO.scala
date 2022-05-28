package com.hydrangea.repository.schedule

import argonaut.{DecodeJson, EncodeJson}
import com.google.inject.Inject
import com.hydrangea.AbstractDAO
import com.hydrangea.file.{FileLocation, FileSystemService}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path

class ScheduleDAO @Inject()(fileSystemService: FileSystemService) extends AbstractDAO(fileSystemService) {
  import ScheduleDAO.logger

  def persist[L <: FileLocation: EncodeJson: DecodeJson](repository: Schedule[L], recordFilePath: Path): Unit = {
    logger.info(s"Writing ScheduleDAO to $recordFilePath")
    persistObj(repository, recordFilePath)
  }

  def load[L <: FileLocation: EncodeJson: DecodeJson](recordFilePath: Path): Option[Schedule[L]] =
    loadObj[Schedule[L]](recordFilePath)
}

object ScheduleDAO {
  private[repository] val logger: Logger = LoggerFactory.getLogger(ScheduleDAO.getClass)
}
