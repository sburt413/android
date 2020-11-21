package com.hydrangea.file

import com.hydrangea.file.UnixPath._
import com.hydrangea.file.WindowsPath2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class FilePathTest extends AnyFlatSpec {
  import com.hydrangea.file.FilePath._

  "WindowsPath" should "be built from a String" in {
    val cWindowsPath: WindowsPath2 = "C:\\Windows\\system32".toWindows2Path
    cWindowsPath.driveLetter should equal("C")
    cWindowsPath.root should equal("C:\\")
    cWindowsPath.segments should equal(Seq("Windows", "system32"))
    cWindowsPath.raw should equal("C:\\Windows\\system32")

    val dTempPath: WindowsPath2 = "D:\\temp".toWindows2Path
    dTempPath.driveLetter should equal("D")
    dTempPath.root should equal("D:\\")
    dTempPath.segments should equal(Seq("temp"))
    dTempPath.raw should equal("D:\\temp")

    val ePath: WindowsPath2 = "E:".toWindows2Path
    ePath.driveLetter should equal("E")
    ePath.root should equal("E:\\")
    ePath.segments should be(Symbol("empty"))
    ePath.raw should equal("E:\\")
  }

  it should "append path fragments" in {
    val dTemp: WindowsPath2 = WindowsPath2("D", Seq("temp"))

    val result: WindowsPath2 = dTemp ++ RelativePath(Seq("app", "files"))
    result.driveLetter should equal("D")
    result.segments should equal(Seq("temp", "app", "files"))
  }

  it should "create relative paths" in {
    val dTempApp: WindowsPath2 = WindowsPath2("D", Seq("temp", "app"))
    val dTempAppFiles: WindowsPath2 = WindowsPath2("D", Seq("temp", "app", "files"))

    val result: RelativePath = dTempAppFiles.relativePath(dTempApp)
    result.segments should equal(Seq("files"))
  }

  it should "be rebased" in {
    val dTemp: WindowsPath2 = WindowsPath2("D", Seq("temp"))
    val dTempAppFiles: WindowsPath2 = WindowsPath2("D", Seq("temp", "app", "files"))
    val cWindows: WindowsPath2 = WindowsPath2("C", Seq("Windows"))

    val cWindowsRebase: WindowsPath2 = dTempAppFiles.rebase(dTemp, cWindows)
    cWindowsRebase.driveLetter should equal("C")
    cWindowsRebase.segments should equal(Seq("Windows", "app", "files"))

    val usrBin = UnixPath(Seq("usr", "bin"))
    val usrBinRebase: UnixPath = dTempAppFiles.rebase(dTemp, usrBin)
    usrBinRebase.segments should equal(Seq("usr", "bin", "app", "files"))
  }

  "NetworkPath" should "be built from a String" in {
    val halJupiterMission: NetworkPath = "\\\\hal9k\\Jupiter\\mission".toNetworkPath
    halJupiterMission.host should equal("hal9k")
    halJupiterMission.root should equal("\\\\hal9k\\")
    halJupiterMission.segments should equal(Seq("Jupiter", "mission"))
    halJupiterMission.raw should equal("\\\\hal9k\\Jupiter\\mission")

    val davePath: NetworkPath = "\\\\Dave".toNetworkPath
    davePath.host should equal("Dave")
    davePath.root should equal("\\\\Dave\\")
    davePath.segments should be(Symbol("empty"))
    davePath.raw should equal("\\\\Dave\\")
  }

  it should "append path fragments" in {
    val halJupiter: NetworkPath = NetworkPath("hal9k", Seq("Jupiter"))

    val result: NetworkPath = halJupiter ++ RelativePath(Seq("mission", "secret"))
    result.host should equal("hal9k")
    result.segments should equal(Seq("Jupiter", "mission", "secret"))
  }

  it should "create relative paths" in {
    val halJupiterSecretMission: NetworkPath = NetworkPath("hal9k", Seq("Jupiter", "secret", "mission"))
    val halJupiterSecret: NetworkPath = NetworkPath("hal9k", Seq("Jupiter", "secret"))

    val result: RelativePath = halJupiterSecretMission.relativePath(halJupiterSecret)
    result.segments should equal(Seq("mission"))
  }

  it should "be rebased" in {
    val halJupiter: NetworkPath = NetworkPath("hal9k", Seq("Jupiter"))
    val halJupiterSecretMission: NetworkPath = NetworkPath("hal9k", Seq("Jupiter", "secret", "mission"))
    val davePersonal: NetworkPath = NetworkPath("dave", Seq("personal"))

    val davePersonalRebase: NetworkPath = halJupiterSecretMission.rebase(halJupiter, davePersonal)
    davePersonalRebase.host should equal("dave")
    davePersonalRebase.segments should equal(Seq("personal", "secret", "mission"))

    val usrBin = UnixPath(Seq("usr", "bin"))
    val usrBinRebase: UnixPath = halJupiterSecretMission.rebase(halJupiter, usrBin)
    usrBinRebase.segments should equal(Seq("usr", "bin", "secret", "mission"))
  }

  "UnixPath" should "be built from a String" in {
    val usrBin: UnixPath = "/usr/bin".toUnixPath
    usrBin.root should equal("/")
    usrBin.segments should equal(Seq("usr", "bin"))
    usrBin.raw should equal("/usr/bin")

    val usr: UnixPath = "/usr".toUnixPath
    usrBin.root should equal("/")
    usrBin.segments should equal(Seq("usr", "bin"))
  }

  it should "append path fragments" in {
    val usrBin = UnixPath(Seq("usr", "bin"))

    val result: UnixPath = usrBin ++ RelativePath(Seq("app", "files"))
    result.segments should equal(Seq("usr", "bin", "app", "files"))
  }

  it should "create relative paths" in {
    val usrLocal = UnixPath(Seq("usr", "local"))
    val usrLocalBin = UnixPath(Seq("usr", "local", "bin"))

    val result: RelativePath = usrLocalBin.relativePath(usrLocal)
    result.segments should equal(Seq("bin"))
  }

  it should "be rebased" in {
    val usrLocal: UnixPath = UnixPath(Seq("usr", "local"))
    val usrLocalBin: UnixPath = UnixPath(Seq("usr", "local", "bin"))
    val libTomcat: UnixPath = UnixPath(Seq("lib", "tomcat"))

    val tomcatLibRebase: UnixPath = usrLocalBin.rebase(usrLocal, libTomcat)
    tomcatLibRebase.segments should equal(Seq("lib", "tomcat", "bin"))

    val cWindowsSystem32: WindowsPath2 = WindowsPath2("C", Seq("Windows", "system32"))

    val cWindowsSystem32Rebase: WindowsPath2 = usrLocalBin.rebase(usrLocal, cWindowsSystem32)
    cWindowsSystem32Rebase.driveLetter should equal("C")
    cWindowsSystem32Rebase.segments should equal(Seq("Windows", "system32", "bin"))
  }
}
