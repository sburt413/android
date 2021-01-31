package com.hydrangea.repository

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.inject.Guice
import com.hydrangea.file.{AbsolutePath, FakeFileSystemService, LocalFileLocation, RelativePath, UnixPathBase}
import com.hydrangea.music.track.{Tag, Track}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import com.hydrangea.{ConfigurationModule, ConfigurationValue}
import net.codingwell.scalaguice.InjectorExtensions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MusicRepositoryServiceTest extends AnyFlatSpec {
  import MusicRepositoryServiceTest._

  val injector =
    Guice.createInjector(DefaultCLIProcessFactoryModule, ConfigurationModule(config), FakeFileSystemService.module(Nil))

  "Music Repository Service" should "persist repositories" in {
    val location: LocalFileLocation = LocalFileLocation(AbsolutePath(UnixPathBase, Seq("music")))
    val repository: MusicRepository[LocalFileLocation] = MusicRepository(location, List(aliceRecord, bobRecord))

    val musicRepositoryService: MusicRepositoryService = injector.instance[MusicRepositoryService]
    musicRepositoryService.writeRepository(repository)

    val loadedRepository: MusicRepository[LocalFileLocation] =
      musicRepositoryService.loadRepository(location).getOrElse(fail("Could not load Repository"))

    loadedRepository should equal(repository)
  }
}

object MusicRepositoryServiceTest {
  private val config: ConfigurationValue = ConfigurationValue(Path.of("test-output"))

  // Instant can have nano-second precision, marshalling keeps only mills
  private val now: Instant = Instant.now().truncatedTo(ChronoUnit.MILLIS)
  private val yesterday: Instant = now.minus(1, ChronoUnit.DAYS)
  private val tomorrow: Instant = now.plus(1, ChronoUnit.DAYS)

  private val repositoryRoot = AbsolutePath.unixPath(Seq("music"))

  val aliceRelativePath = RelativePath(Seq("alice", "Alice Track"))
  val aliceTag = Tag("Alice Title", "Alice Track", "Alice", Some(2000), Some(1), Some(10), Some(1), Some(2))
  val aliceTrack = Track("1111", repositoryRoot ++ aliceRelativePath, now, aliceTag)
  val aliceRecord = RepositoryRecord(repositoryRoot, aliceTrack, tomorrow)

  val bobRelativePath = RelativePath(Seq("bob", "Bob Track"))
  val bobTag = Tag("Bob Title", "Bob Track", "Bob", Some(2001), Some(2), Some(20), Some(2), Some(2))
  val bobTrack = Track("2222", repositoryRoot ++ bobRelativePath, yesterday, bobTag)
  val bobRecord = RepositoryRecord(repositoryRoot, bobTrack, tomorrow)
}
