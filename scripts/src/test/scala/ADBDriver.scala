import java.io.InputStream

import com.google.inject.Guice
import com.hydrangea.android.Device
import com.hydrangea.android.adb.{ADBCommandLine, ADBService}
import com.hydrangea.file.{
  AbsolutePath,
  AndroidFileData,
  AndroidLocation,
  DefaultFileSystemServiceModule,
  FileSystemService
}
import com.hydrangea.music.tagger.TikaTagger
import com.hydrangea.music.track.Tag
import com.hydrangea.process.DefaultCLIProcessFactoryModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec

class ADBDriver extends AnyFlatSpec {
  import com.hydrangea.file.FilePath._

  val injector = Guice.createInjector(DefaultCLIProcessFactoryModule, DefaultFileSystemServiceModule)

  "Driver" should "run" in {
    val filePath: AbsolutePath =
      "/storage/0123-4567/Music/Whitesnake/The Best Of Whitesnake/02 - Still Of The Night.mp3".toAbsolutePath.getOrElse(
        throw new IllegalStateException("Illegal Path"))

    val adbService = injector.instance[ADBService]
    val fileSystemService: FileSystemService = injector.instance[FileSystemService]
    val tagger: TikaTagger = injector.instance[TikaTagger]

    val device: Device = adbService.firstDevice

    val location = AndroidLocation(device, filePath)
    val tag: Tag = tagger.tag(location)
    println(s"Parsed tag: $tag")

    val srcPath: AbsolutePath = filePath.raw.toUnixPath
    val start: Long = System.currentTimeMillis()
    val srcLocation: AndroidLocation = AndroidLocation(device, srcPath)
    fileSystemService.copyFromDevice(srcLocation, "F:\\output.mp3".toLocalWindowsPath)
    System.out.println("Copied in " + (System.currentTimeMillis() - start) + "ms")
  }

  it should "list a directory" in {
    val adbService = injector.instance[ADBService]
    val device: Device = adbService.firstDevice
    val listing: Seq[AndroidFileData] =
      adbService.commandLine(device).list("/storage/self/primary/test/dir-1".toUnixPath)
    println(listing.mkString("\n"))
  }

  "Driver" should "scan music" in {
    val filePath: AbsolutePath =
      "/storage/0123-4567/Music".toAbsolutePath.getOrElse(throw new IllegalStateException("Illegal Path"))

    val adbService = injector.instance[ADBService]
    val device: Device = adbService.firstDevice
    val files = adbService.commandLine(device, charset = ADBCommandLine.UTF_8).scan(filePath)
    println(s"Files: \n${files.mkString("\n")}")
  }
}

class Copier(inputStream: InputStream) extends Thread {
  private var bytes: Array[Byte] = _

  override def run(): Unit = {
    bytes = IOUtils.toByteArray(inputStream)
    inputStream.close()
  }

  def getBytes: Array[Byte] = bytes
}
