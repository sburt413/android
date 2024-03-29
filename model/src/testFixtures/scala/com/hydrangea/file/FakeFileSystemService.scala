package com.hydrangea.file

import java.io.ByteArrayInputStream
import java.time.Instant

import com.google.inject.AbstractModule
import com.hydrangea.DisjunctionOps._
import com.hydrangea.file.FileSystemService.ReadLambda
import net.codingwell.scalaguice.ScalaModule
import scalaz.Disjunction

import scala.collection.mutable
import scala.reflect.ClassTag

case class FakeFileSystemService(fakeFileSystem: FakeFileSystem) extends FileSystemService {
  override def read[A](location: FileLocation)(readerFn: ReadLambda[A]): Disjunction[String, A] = {
    fakeFileSystem.getOrThrow(location) match {
      case _: FakeDirectory                => s"Fake file for location ($location) is a directory.".toLeftDisjunction
      case FakeRegularFile(_, _, contents) => readerFn(new ByteArrayInputStream(contents)).toRightDisjunction
    }
  }

  override def write(location: FileLocation, data: Array[Byte]): Unit =
    fakeFileSystem.put(location, FakeRegularFile(location, Instant.now(), data))

  override def list(location: FileLocation): Seq[FileData] =
    fakeFileSystem.filesByLocation.toSeq.flatMap({
      case (fileLocation, file) => Some(file.toFileData).filter(_ => fileLocation.parentLocation.equals(location))
    })

  override def scan(location: FileLocation): Seq[FileData] =
    fakeFileSystem.filesByLocation.toSeq.flatMap({
      case (fileLocation, file) => Some(file.toFileData).filter(_ => fileLocation.startsWith(location))
    })

  override def ancestorLocations[L <: FileLocation](location: L)(implicit classTag: ClassTag[L]): Seq[L] =
    scan(location).map(_.location).flatMap(_.to[L])

  // These fake methods do _not_ need to take advantage of the filesystem
  override def regularFileCount(location: FileLocation): Int =
    scan(location).count({
      case _: RegularFileData => true
      case _                  => false
    })

  override def mostRecentUpdate(location: FileLocation): Option[Instant] =
    scan(location).map(_.modifyTime).maxOption
}

object FakeFileSystemService {
  def apply(files: Seq[FakeFile]): FakeFileSystemService = {
    val fileByLocation: mutable.Map[FileLocation, FakeFile] = files.map(file => file.location -> file).to(mutable.Map)
    FakeFileSystemService(FakeFileSystem(fileByLocation))
  }

  def module(files: Seq[FakeFile]): AbstractModule = new AbstractModule with ScalaModule {
    override def configure(): Unit = {
      bind[FileSystemService].toInstance(FakeFileSystemService(files))
    }
  }
}

case class FakeFileSystem(filesByLocation: mutable.Map[FileLocation, FakeFile]) {
  def put(location: FileLocation, fakeFile: FakeFile): Unit =
    filesByLocation.put(location, fakeFile)

  def getOrThrow(location: FileLocation): FakeFile =
    filesByLocation.getOrElse(location, throw new IllegalStateException(s"No file fakes for location $location"))
}

sealed trait FakeFile {
  def location: FileLocation
  def modifyTime: Instant

  def toFileData: FileData =
    (location, this) match {
      case (local: LocalFileLocation, _: FakeDirectory)   => LocalDirectoryData(local, this.modifyTime)
      case (local: LocalFileLocation, _: FakeRegularFile) => LocalRegularFileData(local, this.modifyTime)
      case (android: AndroidLocation, _: FakeDirectory)   => AndroidDirectoryData(android, this.modifyTime)
      case (android: AndroidLocation, _: FakeRegularFile) => AndroidRegularFileData(android, this.modifyTime)
    }

  def asFileDataOrThrow[F <: FileData](implicit classTag: ClassTag[F]): F =
    this.toFileData match {
      case f: F => f
      case d    => throw new IllegalStateException(s"$d is not a ${classTag.runtimeClass.getName}")
    }
}

case class FakeDirectory(location: FileLocation, modifyTime: Instant) extends FakeFile
case class FakeRegularFile(location: FileLocation, modifyTime: Instant, contents: Array[Byte] = Nil.toArray)
    extends FakeFile
