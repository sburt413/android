package com.hydrangea.music.script

import java.io.File

import com.hydrangea.android.adb.CommandlineHelper._
import com.hydrangea.android.adb.{ADB, Device}
import com.hydrangea.android.file.VirtualPath._
import com.hydrangea.android.file._
import com.hydrangea.music.library.TrackRecord
import com.hydrangea.music.library.index.{ArtistQuery, IndexService}
import com.mpatric.mp3agic.{ID3v2, Mp3File}
import IndexService._

object TestApp {
  val adbDirectory: LocalDirectory = {
    val file: File = new File("D:\\adb")
    file.mkdirs()
    LocalDirectory(file)
  }

  private val device: Device = ADB.firstDevice

  private val musicDirectoryPath: AndroidPath = "/storage/0123-4567/Music".toAndroidPath
  private val generationAxeDirectoryPath: AndroidPath = "/storage/0123-4567/Music/Generation Axe".toAndroidPath
  private val whippingPostPath: AndroidPath =
    "/storage/0123-4567/Music/Generation Axe/The Guitars That Destroyed The World_ Li/06 Whipping Post [Live].mp3".toAndroidPath

  def main(args: Array[String]): Unit = {
    val records: Seq[TrackRecord] = copyGenerationAxe()

    IndexService.createIndex(device)
    records.foreach(record => {
      println(s"Indexing $record")
      IndexService.put(device, record)
    })

    println("Done indexing")

    val results: Seq[(Id, TrackRecord)] = IndexService.query(device, ArtistQuery("Tosin"))
    println(s"Found results: $results")
  }

  def printListings(): Unit = {
    val listing: Seq[RemoteFile] = device.commandline().scan(musicDirectoryPath)
    listing.foreach(println)
  }

  def copyGenerationAxe(): Seq[TrackRecord] = {
    val source: AndroidDirectory =
      device
        .commandline()
        .stat(generationAxeDirectoryPath)
        .flatMap({
          case dir: AndroidDirectory => Some(dir)
          case _                     => None
        })
        .getOrElse(throw new IllegalStateException("File Not Found: " + generationAxeDirectoryPath))

    val copied: Map[AndroidPath, WindowsPath] = device.commandline().pull(source, adbDirectory)
    copied
      .filter(_._2.raw.endsWith(".mp3"))
      .map({
        case (src, dest) =>
          val sourceSum: String = device.commandline().sha1sum(src)

          val destFile: LocalFile =
            LocalFileSystem.read(dest).getOrElse(throw new IllegalStateException(s"Did not copy file: $dest"))
          val mp3File = new Mp3File(destFile.toJavaFile)
          val tag: ID3v2 = mp3File.getId3v2Tag
          println(src + s" ($sourceSum) => " + dest + s" (${tag.getArtist} / ${tag.getTitle})")

          TrackRecord(sourceSum, destFile, tag)
      })
      .toSeq
  }

  def sha1(path: AndroidPath): Unit = {
    println(device.commandline().sha1sum(path))
  }
}
