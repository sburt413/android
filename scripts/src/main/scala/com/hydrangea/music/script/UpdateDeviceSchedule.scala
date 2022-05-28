package com.hydrangea.music.script

import com.google.inject.Guice
import com.hydrangea.file.{AndroidLocation, DefaultFileSystemServiceModule}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import com.hydrangea.repository.schedule.{Schedule, SchedulerService}
import com.hydrangea.repository.{MusicRepository, MusicRepositoryService}
import com.hydrangea.{Configuration, TypeSafeConfigurationModule}
import net.codingwell.scalaguice.InjectorExtensions._

object UpdateDeviceSchedule extends App {
  val injector =
    Guice.createInjector(DefaultCLIProcessFactoryModule, TypeSafeConfigurationModule, DefaultFileSystemServiceModule)

  val config: Configuration = injector.instance[Configuration]
  val remoteRepositoryLocation: AndroidLocation = config.androidRepositoryLocation

  val musicRepositoryService: MusicRepositoryService = injector.instance[MusicRepositoryService]
  val scheduler: SchedulerService = injector.instance[SchedulerService]

  val repository: MusicRepository[AndroidLocation] =
    musicRepositoryService
      .loadRepository(remoteRepositoryLocation)
      .getOrElse({
        val newRepository: MusicRepository[AndroidLocation] = MusicRepository(remoteRepositoryLocation, Nil)
        musicRepositoryService.writeRepository(newRepository)
        newRepository
      })

  val schedule: Schedule[AndroidLocation] =
    scheduler.loadSchedule(repository).getOrElse(scheduler.createSchedule(repository))
  val updatedSchedule: Schedule[AndroidLocation] = scheduler.updateSchedule(repository, schedule)
  println(s"Updated schedule: $updatedSchedule")
  scheduler.writeSchedule(repository, updatedSchedule)
}
