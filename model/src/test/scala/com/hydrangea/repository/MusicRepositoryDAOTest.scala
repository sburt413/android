package com.hydrangea.repository

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.inject.Guice
import com.hydrangea.file.{AbsolutePath, DefaultFileSystemServiceModule, LocalFileLocation, RelativePath, UnixPathBase}
import com.hydrangea.music.track.{Tag, Track}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MusicRepositoryDAOTest extends AnyFlatSpec {
  import MusicRepositoryDAOTest._

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, DefaultFileSystemServiceModule)

  "Music Repository Service" should "persist and load repository records" in {
    val location: LocalFileLocation = LocalFileLocation(AbsolutePath(UnixPathBase, Seq("music")))
    val repository: MusicRepository[LocalFileLocation] = MusicRepository(location, List(aliceRecord, bobRecord))

    val musicRepositoryService: MusicRepositoryDAO = injector.instance[MusicRepositoryDAO]
    val testFilePath: Path = Path.of("test.json")
    musicRepositoryService.persist(repository, testFilePath)

    val reloadedRepository: MusicRepository[LocalFileLocation] =
      musicRepositoryService.load[LocalFileLocation](testFilePath).getOrElse(fail("Cannot load repository from file."))

    reloadedRepository should equal(repository)
  }
}

object MusicRepositoryDAOTest {
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
