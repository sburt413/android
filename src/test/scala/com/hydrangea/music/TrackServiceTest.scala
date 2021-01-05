package com.hydrangea.music

import java.io.ByteArrayOutputStream
import java.time.Instant

import com.google.inject.Guice
import com.hydrangea.file.{
  AbsolutePath,
  FakeFileSystemService,
  FakeRegularFile,
  LocalFileLocation,
  LocalRegularFileData,
  UnixPathBase
}
import com.hydrangea.music.track.{Track, TrackService}
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TrackServiceTest extends AnyFlatSpec {

  "TrackService" should "generate tracks from files" in {
    val fileContent: ByteArrayOutputStream = new ByteArrayOutputStream()
    IOUtils.copy(getClass.getResourceAsStream("/audio/sample1.mp3"), fileContent)

    val path: AbsolutePath = AbsolutePath(UnixPathBase, Seq("music", "sample1.mp3"))
    val location: LocalFileLocation = LocalFileLocation(path)
    val modifyTime: Instant = Instant.now()
    val fileDefinition: FakeRegularFile = FakeRegularFile(location, modifyTime, fileContent.toByteArray)

    val injector =
      Guice.createInjector(DefaultCLIProcessFactoryModule, FakeFileSystemService.module(Seq(fileDefinition)))
    val trackService: TrackService = injector.instance[TrackService]

    val file = LocalRegularFileData(location, modifyTime)
    val track: Track = trackService.readTrack(file)

    track.hash should equal("4b874410bbda68c322ca08d1925e10dc25f8fb36")
    track.path should equal(path)
    track.lastModified should equal(modifyTime)

    track.tag.title should equal("Sample 1")
    track.tag.album should equal("Hydrangea Sample")
    track.tag.artist should equal("Hydrangea")
    track.tag.year should contain(2021)
    track.tag.trackNumber should contain(1)
    track.tag.trackCount should contain(8)
    track.tag.discNumber should contain(1)
    track.tag.discCount should contain(1)
  }
}
