package com.hydrangea.android.adb.ls

import java.text.SimpleDateFormat
import java.time.Instant

import com.hydrangea.android.file.{AndroidPath, VirtualPath}
import org.slf4j.Logger

import scala.annotation.tailrec

object LsParser {
  private val logger: Logger = org.slf4j.LoggerFactory.getLogger(LsParser.getClass)

  // [2020-05-09]T[15:51]
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

  //  $ adb -d shell ls -Rpla --full-time /storage/0123-4567/Music/Toska/
  //  /storage/0123-4567/Music/Toska/:
  //  total 640
  //  drwxrwx--x   5 root sdcard_rw 131072 2018-11-02 08:16:10.000000000 -0400 ./
  //  drwxrwx--x 304 root sdcard_rw 131072 2020-04-26 09:22:38.000000000 -0400 ../
  //  drwxrwx--x   2 root sdcard_rw 131072 2018-11-02 08:16:16.000000000 -0400 Fire\ by\ the\ Silos/
  //  drwxrwx--x   2 root sdcard_rw 131072 2018-05-01 15:22:52.000000000 -0400 Ode\ to\ the\ Author/
  //

  // (subpath, fileName, modified time)
  def parseDirectories(output: Seq[String]): Seq[(AndroidPath, String, Instant)] = {
    @tailrec
    def split(directoryListings: Seq[(AndroidPath, String, Instant)],
              unprocessed: Seq[String]): Seq[(AndroidPath, String, Instant)] = {
      if (unprocessed.isEmpty) {
        directoryListings
      } else {
        // Find the block of entries related to the first directory lists
        val splitIndex: Int = unprocessed.indexWhere(_.isEmpty)
        val (directoryListing, remaining) =
          if (splitIndex == -1) {
            (unprocessed, Nil)
          } else {
            val (listing, everythingElse) = unprocessed.splitAt(splitIndex)
            (listing, everythingElse.tail) // Drop the empty line leading off the list
          }

        val (header, listing) = directoryListing.splitAt(2)
        val subpath: AndroidPath = AndroidPath(header.head.dropRight(1)) // Drop the final ':' from the directory path
        val directoryEntries: Seq[(String, Instant)] = parseDirectory(listing)
        val processedDirectory: Seq[(AndroidPath, String, Instant)] = directoryEntries.map({
          case (fileName, lastModified) => (subpath, fileName, lastModified)
        })

        split(directoryListings ++ processedDirectory, remaining)
      }
    }

    split(Nil, output)
  }

  def parseDirectory(output: Seq[String]): Seq[(String, Instant)] = {
    // Ignore first column containing total
    val entries: Seq[String] = output.tail
    entries
      .flatMap(parseFile)
      .filter({
        case (pathString, _) =>
          val path: AndroidPath = AndroidPath(pathString)
          !VirtualPath.isCurrentDirectory(path) && !VirtualPath.isParentDirectory(path)
      })
  }

  def parseFile(entry: String): Option[(String, Instant)] = {
    Option(entry)
      .filterNot(_.matches("total\\s+\\d+"))
      .map(line => {
        // Permissions, Number, Owner, Group, Size, Modify Date, Modify Time, File Name
        val columns: Seq[String] = line.split("\\s+", 9).toSeq
        logger.trace(s"Parsing $columns")
        val fileName: String = columns(8)
        // 2019-02-11 22:05:37.121626843 -0500
        // Truncate to millisecond precision
        val dateString: String = columns(5) + 'T' + columns(6).substring(0, 12) + columns(7)
        val modifyTime: Instant = dateFormat.parse(dateString).toInstant
        (fileName, modifyTime)
      })
  }
}
