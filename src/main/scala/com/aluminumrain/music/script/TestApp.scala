package com.aluminumrain.music.script

import com.aluminumrain.android.adb.ADB
import com.aluminumrain.android.adb.ShellHelper._
import com.aluminumrain.android.file.VirtualPath._
import com.aluminumrain.android.file.{VirtualFile, VirtualPath}

object TestApp {
  private val musicDirectoryPath: VirtualPath = "/storage/0123-4567/Music".toVirtualPath
  private val generationAxeDirectoryPath: VirtualPath = "/storage/0123-4567/Music/Generation Axe".toVirtualPath

  def main(args: Array[String]): Unit = {
    val listing: Seq[VirtualFile] = ADB.usb.scan(musicDirectoryPath)
    listing.foreach(println)
  }
}
