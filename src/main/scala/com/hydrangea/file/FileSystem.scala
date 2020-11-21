package com.hydrangea.file

trait FileSystem[P <: AbsolutePath] {}

case class WindowsFileSystem(computerName: String) extends FileSystem[WindowsPath2]

case class NetworkFileSystem(host: String) extends FileSystem[NetworkPath]

case class AndroidFileSystem(serial: String) extends FileSystem[UnixPath]
