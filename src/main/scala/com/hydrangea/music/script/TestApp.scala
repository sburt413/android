package com.hydrangea.music.script

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import com.hydrangea.android.adb.{ADBService, Device}
import com.hydrangea.file.FileData._
import com.hydrangea.file.{AbsolutePath, AndroidRegularFileData, FileData, LocalRegularFileData}
import com.hydrangea.music.tagger.TikaTagger
import com.hydrangea.music.track.{Tag, Track, TrackService}
import com.hydrangea.process.DefaultCLIProcessFactory

// TODO
object TestApp {

//  import com.hydrangea.file.FilePath._

  val adbService: ADBService = ADBService(DefaultCLIProcessFactory.instance)
  val device: Device = adbService.firstDevice
  val tagger: TikaTagger = TikaTagger(DefaultCLIProcessFactory.instance)

  private val musicDirectoryPath = "/storage/0123-4567/Music"
  private val devinTownsendDirectoryPath = "/storage/0123-4567/Music/Devin Townsend"
  private val acceleratedEvolution = "/storage/0123-4567/Test/Accelerated Evolution"
  private val byAThreadAddicted =
    "/storage/0123-4567/Music/Devin Townsend/By a Thread - Live in London 2011 (incl Encores) [Explicit]"
  private val aPerfectCircle = "/storage/0123-4567/Music/A Perfect Circle"
  private val merDeNoms = aPerfectCircle + "/Mer de Noms"
  private val generationAxeDirectoryPath = "/storage/0123-4567/Music/Generation Axe"
  private val whippingPostPath =
    "/storage/0123-4567/Music/Generation Axe/The Guitars That Destroyed The World_ Li/06 Whipping Post [Live].mp3"

  def main(args: Array[String]): Unit = {
//    val repositoryDirectory: WindowsDirectory =
//      Configuration.repositoryDirectory.toLocalDirectory
//        .getOrElse(throw new IllegalStateException("Repository directory does not exist."))
//
//    RepositoryLibraryService.createRepositoryIndex(Repository(repositoryDirectory))
    val apcPath: Path = Paths.get("Z:\\A Perfect Circle\\Mer de Noms")
    Files.walkFileTree(
      apcPath,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          println(s"Visiting: $file")
          if (file.getFileName.toString.endsWith(".mp3")) {
            val fileData: LocalRegularFileData =
              file.toLocalRegularFileData.getOrElse(throw new IllegalArgumentException("Not a regular file"))
            val track: Track = TrackService(DefaultCLIProcessFactory.instance).getLocalTrack(fileData)
            println(s"Record for path ($file): $track")
          }

          FileVisitResult.CONTINUE
        }
      }
    )

  }

  def tag() = {
    adbService.withCommandLine(device) { commandLine =>
      //      val path: AndroidPath = "/storage/0123-4567/Test/Addicted/01-03- Bend It Like Bender!.mp3".toAndroidPath

      //      val toEval = s"'ls -la ${path.raw.replace(" ", "\\ ").replace("!", "\\!")}'"
      //      val cmd = "\"" + "eval " + toEval + "\""
      //      val (_, output, _) = commandLine.execOutCmdAndParse(cmd)
      //      println(s"OUTPUT: ${output.mkString("\n")}")

      println(s"Extracting MP3 tags.")
      val mp3Files: Seq[AndroidRegularFileData] =
        commandLine
          .listRecursive(AbsolutePath.unixPath(merDeNoms))
          .collect({
            case f: AndroidRegularFileData => f
          })
          .filter(FileData.mp3Filter)

      val tags: Seq[Tag] = mp3Files.map(file => tagger.tag(file.location))
      println(s"Extracted Tags:\n${tags.mkString("\n")}")

      //      val depthCharge: AndroidRegularFile =
      //        commandLine.stat("/storage/0123-4567/Test/DepthCharge.mp3".toAndroidPath).get.to[AndroidRegularFile].get
      //      val record: TrackRecord = TikaAndroidTagger.tag(commandLine, depthCharge)
      //      val record: TrackRecord = TikaAndroidTagger.tag2(commandLine, depthCharge)
      //      println(s"Final record: $record")

      //commandLine.transferViaBase64(depthCharge.path, out)
      //      mp3Files.map(file => {
      //        val fileDataStream: InputStream = commandLine.transferViaBase64Tunnel(file.path)
      //        val out: Path = Files.createTempFile("adb", ".dat")
      //        val outStream = new FileOutputStream(out.toFile)
      //        IOUtils.copy(fileDataStream, outStream)
      //        fileDataStream.close()
      //        outStream.close()
      //
      //        println(s"Wrote to temp file: $out")
      //      })
    }

    //    val writtenRecords: Seq[TrackRecord] =
    //      device.withCommandLine() { commandline =>
    //        val source: AndroidDirectory =
    //          commandline
    //            .statOrThrow(devinTownsendDirectoryPath)
    //            .castOrThrow[AndroidDirectory]
    //
    //        LibraryService.updateIndex(device, source, forceOverwrite = true)
    //      }
    //
    //    println(s"Wrote records:\n${writtenRecords.mkString("\n")}")
    //
    //    val results: Seq[(Id, TrackRecord)] = IndexService.query(device, ArtistQuery(writtenRecords.head.tag.artist), 1000)
    //    println(s"Found results:n${results.mkString("\n")}")
  }
}
