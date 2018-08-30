package org.sireum.aadl.biji

import java.io.{File, BufferedWriter, FileWriter}
import org.sireum.{B}

import org.sireum.println

object NativeIO_Ext {

  def writeToFile(path: org.sireum.String, contents: org.sireum.String, overwrite: org.sireum.B): Unit = {
    val fname = new File(path.toString)

    try {
      // try building any missing subdirs
      fname.getParentFile.mkdirs

      assert(fname.getParentFile.exists)

      if (overwrite || !fname.exists) {
        val bw = new BufferedWriter(new FileWriter(fname))
        bw.write(contents.toString)
        bw.close()

        println("Wrote: " + fname)
      }
    } catch {
      case e: Throwable =>
        println("Error encountered while trying to create file: " + fname)
        println(e.getMessage)
    }
  }
}
