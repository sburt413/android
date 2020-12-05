package com.hydrangea.process

import java.io.{InputStream, StringWriter}
import java.nio.charset.Charset

import org.apache.commons.io.IOUtils

class ReaderThread(inputStream: InputStream, charset: Charset) extends Thread {
  private val output = new StringWriter()

  override def run(): Unit = {
    output.write(IOUtils.toCharArray(inputStream, charset))
    inputStream.close()
  }

  def getOutput: String = output.toString
}
