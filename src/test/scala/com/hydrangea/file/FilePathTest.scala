package com.hydrangea.file

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class FilePathTest extends AnyFlatSpec {

  "AbsolutePath" should "build paths from local Windows path Strings" in {
    val cWindowsSystem32: AbsolutePath = AbsolutePath.localWindowsPath("C:\\Windows\\system32")
    cWindowsSystem32.base should equal(LocalWindowsPathBase('C'))
    cWindowsSystem32.base.root should equal("C:\\")
    cWindowsSystem32.segments should equal(Seq("Windows", "system32"))
    cWindowsSystem32.raw should equal("C:\\Windows\\system32")
  }

  it should "build paths from Windows network path Strings" in {
    val halJupiterMission: AbsolutePath = AbsolutePath.windowsNetworkPath("\\\\hal9k\\Jupiter\\mission")
    halJupiterMission.base should equal(WindowsNetworkPathBase("hal9k"))
    halJupiterMission.base.root should equal("\\\\hal9k\\")
    halJupiterMission.segments should equal(Seq("Jupiter", "mission"))
    halJupiterMission.raw should equal("\\\\hal9k\\Jupiter\\mission")
  }

  it should "build paths from unix path Strings" in {
    val usrBin: AbsolutePath = AbsolutePath.unixPath("/usr/bin")
    usrBin.base should equal(UnixPathBase)
    usrBin.base.root should equal("/")
    usrBin.segments should equal(Seq("usr", "bin"))
    usrBin.raw should equal("/usr/bin")
  }

  it should "append relative paths" in {
    val dWindowsSystem32: AbsolutePath = AbsolutePath.localWindowsPath("D:\\Windows\\system32")
    val driversPath: AbsolutePath = dWindowsSystem32 ++ RelativePath(Seq("etc", "drivers"))
    driversPath should equal(AbsolutePath(LocalWindowsPathBase('D'), List("Windows", "system32", "etc", "drivers")))

    val halJupiterMission: AbsolutePath = AbsolutePath.windowsNetworkPath("\\\\hal9k\\Jupiter\\mission")
    val secretPath: AbsolutePath = halJupiterMission ++ RelativePath(Seq("secret"))
    secretPath should equal(AbsolutePath(WindowsNetworkPathBase("hal9k"), List("Jupiter", "mission", "secret")))

    val usr: AbsolutePath = AbsolutePath.unixPath("/usr")
    val binPath: AbsolutePath = usr ++ RelativePath(Seq("local", "bin"))
    binPath should equal(AbsolutePath(UnixPathBase, List("usr", "local", "bin")))
  }

  it should "create relative paths" in {
    val windowsSystem32: AbsolutePath = AbsolutePath.localWindowsPath("D:\\Windows\\system32")
    val windowsSystem32EtcHosts: AbsolutePath = AbsolutePath.localWindowsPath("D:\\Windows\\system32\\etc\\hosts")
    val etcHostsFragment: RelativePath = windowsSystem32EtcHosts.relativePath(windowsSystem32)
    etcHostsFragment should equal(RelativePath(Seq("etc", "hosts")))

    val halJupiterMission: AbsolutePath = AbsolutePath.windowsNetworkPath("\\\\hal9k\\Jupiter\\mission")
    val halJupiterMissionSecret: AbsolutePath = AbsolutePath.windowsNetworkPath("\\\\hal9k\\Jupiter\\mission\\secret")
    val secretFragment: RelativePath = halJupiterMissionSecret.relativePath(halJupiterMission)
    secretFragment should equal(RelativePath(Seq("secret")))

    val usrLocal: AbsolutePath = AbsolutePath.unixPath("/usr/local/bin")
    val usr: AbsolutePath = AbsolutePath.unixPath("/usr")
    val localBinFragment: RelativePath = usrLocal.relativePath(usr)
    localBinFragment should equal(RelativePath(Seq("local", "bin")))
  }

  it should "be able to be rebased" in {
    val srcWindows: AbsolutePath = AbsolutePath.localWindowsPath("D:\\Windows\\system32")
    val srcHost: AbsolutePath = AbsolutePath.localWindowsPath("D:\\Windows\\system32\\etc\\hosts")
    val destUsr: AbsolutePath = AbsolutePath.unixPath("/usr")

    val rebasedHost: AbsolutePath = srcHost.rebase(srcWindows, destUsr)
    rebasedHost should equal(AbsolutePath(UnixPathBase, List("usr", "etc", "hosts")))

    val srcUsr: AbsolutePath = AbsolutePath.unixPath("/usr")
    val srcBin: AbsolutePath = AbsolutePath.unixPath("/usr/local/bin")
    val destSystem32: AbsolutePath = AbsolutePath.localWindowsPath("E:\\Windows\\system32")

    val rebasedBin: AbsolutePath = srcBin.rebase(srcUsr, destSystem32)
    rebasedBin should equal(AbsolutePath(LocalWindowsPathBase('E'), List("Windows", "system32", "local", "bin")))
  }

  it should "escape special characters" in {
    val escaped: String = AbsolutePath.unixPath("/Music/Sound & Fury/Storm ('Unleashed' Mix)").escaped
    escaped should equal(""""/Music/Sound\ \&\ Fury/Storm\ \(\'Unleashed\'\ Mix\)"""")
  }
}
