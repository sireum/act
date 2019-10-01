package org.sireum.hamr.act

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.StandardCopyOption

import org.sireum.B
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
        Util.addError("Error encountered while trying to create file: " + fname)
        Util.addError(e.getMessage)
    }
  }

  def copyFile(srcPath: org.sireum.String, outputPath: org.sireum.String): String = {
    val src = new File(srcPath.native)

    val outputDir = new File(outputPath.native)

    val outputFile = new File(outputDir, src.getName)

    outputFile.getParentFile.mkdirs()

    java.nio.file.Files.copy(src.toPath, outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)

    Util.addMessage(s"Wrote: ${outputFile}")

    return src.getName
  }

  def fileExists(path: org.sireum.String): B = {
    val f = new File(path.native)
    return f.exists()
  }
}
