package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.file.{DefaultFileSystemServiceModule, LocalFileLocation}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import com.hydrangea.repository.schedule.{Schedule, SchedulerService}
import com.hydrangea.repository.{MusicRepository, MusicRepositoryService}
import com.hydrangea.{Configuration, TypeSafeConfigurationModule}
import net.codingwell.scalaguice.InjectorExtensions._

object UpdateRepositorySchedule extends App {
  val injector =
    Guice.createInjector(DefaultCLIProcessFactoryModule, TypeSafeConfigurationModule, DefaultFileSystemServiceModule)

  val config: Configuration = injector.instance[Configuration]
  val repositoryLocation: LocalFileLocation = config.localRepositoryLocation

  val musicRepositoryService: MusicRepositoryService = injector.instance[MusicRepositoryService]
  val scheduler: SchedulerService = injector.instance[SchedulerService]

  val repository: MusicRepository[LocalFileLocation] =
    musicRepositoryService
      .loadRepository(repositoryLocation)
      .getOrElse({
        val newRepository: MusicRepository[LocalFileLocation] = MusicRepository(repositoryLocation, Nil)
        musicRepositoryService.writeRepository(newRepository)
        newRepository
      })

  val schedule: Schedule[LocalFileLocation] =
    scheduler.loadSchedule(repository).getOrElse(scheduler.createSchedule(repository))
  val updatedSchedule: Schedule[LocalFileLocation] = scheduler.updateSchedule(repository, schedule)
  println(s"Updated schedule: $updatedSchedule")
  scheduler.writeSchedule(repository, updatedSchedule)
}
