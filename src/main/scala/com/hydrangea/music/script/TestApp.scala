package com.hydrangea.music.script

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import com.hydrangea.android.adb.{ADB, Device}
import com.hydrangea.android.file.VirtualPath._
import com.hydrangea.android.file._
import com.hydrangea.music.TrackRecord
import com.hydrangea.music.tagger.TikaTagger

object TestApp {
  val adbDirectory: WindowsDirectory = {
    val path: Path = Paths.get("D:\\adb")
    Files.createDirectories(path)
    WindowsDirectory(path)
  }

  private val device: Device = ADB.firstDevice

  private val musicDirectoryPath: AndroidPath = "/storage/0123-4567/Music".toAndroidPath
  private val devinTownsendDirectoryPath: AndroidPath = "/storage/0123-4567/Music/Devin Townsend".toAndroidPath
  private val acceleratedEvolution: AndroidPath = "/storage/0123-4567/Test/Accelerated Evolution".toAndroidPath
  private val byAThreadAddicted: AndroidPath =
    "/storage/0123-4567/Music/Devin Townsend/By a Thread - Live in London 2011 (incl Encores) [Explicit]".toAndroidPath
  private val aPerfectCircle: AndroidPath =
    "/storage/0123-4567/Music/A Perfect Circle".toAndroidPath
  private val merDeNoms: AndroidPath = aPerfectCircle :+ "Mer de Noms"
  private val generationAxeDirectoryPath: AndroidPath = "/storage/0123-4567/Music/Generation Axe".toAndroidPath
  private val whippingPostPath: AndroidPath =
    "/storage/0123-4567/Music/Generation Axe/The Guitars That Destroyed The World_ Li/06 Whipping Post [Live].mp3".toAndroidPath

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
            val record: TrackRecord = TikaTagger.tag(file)
            println(s"Record for path ($file): $record")
          }

          FileVisitResult.CONTINUE
        }
      }
    )

  }

  def tag() = {
    device.withCommandLine() { commandLine =>
      //      val path: AndroidPath = "/storage/0123-4567/Test/Addicted/01-03- Bend It Like Bender!.mp3".toAndroidPath

      //      val toEval = s"'ls -la ${path.raw.replace(" ", "\\ ").replace("!", "\\!")}'"
      //      val cmd = "\"" + "eval " + toEval + "\""
      //      val (_, output, _) = commandLine.execOutCmdAndParse(cmd)
      //      println(s"OUTPUT: ${output.mkString("\n")}")

      println(s"Extracting MP3 tags.")
      val mp3Files: Seq[AndroidRegularFile] =
        commandLine
          .listRecursive(merDeNoms)
          .collect({
            case f: AndroidRegularFile => f
          })
          .filter(VirtualFile.mp3Filter)

      val records: Seq[TrackRecord] = mp3Files.map(TikaTagger.tag(commandLine, _))
      println(s"Extracted Records:\n${records.mkString("\n")}")

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
