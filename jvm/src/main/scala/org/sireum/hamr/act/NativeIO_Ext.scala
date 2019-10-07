package org.sireum.hamr.act

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.StandardCopyOption

import org.sireum.B
import org.sireum.message.Reporter

object NativeIO_Ext {

  def writeToFile(path: org.sireum.String, contents: org.sireum.String, overwrite: org.sireum.B, reporter: Reporter): Unit = {
    val fname = new File(path.toString)

    try {
      // try building any missing subdirs
      fname.getParentFile.mkdirs

      assert(fname.getParentFile.exists)

      if (overwrite || !fname.exists) {
        val bw = new BufferedWriter(new FileWriter(fname))
        bw.write(contents.toString)
        bw.close()

        reporter.info(org.sireum.None(), Util.toolName, ("Wrote: " + fname))
      }
    } catch {
      case e: Throwable =>
        reporter.error(org.sireum.None(), Util.toolName, "Error encountered while trying to create file: " + fname)
        reporter.error(org.sireum.None(), Util.toolName, e.getMessage)
    }
  }

  def copyFile(srcPath: org.sireum.String, outputPath: org.sireum.String, reporter: Reporter): String = {
    val src = new File(srcPath.native)

    val outputDir = new File(outputPath.native)

    val outputFile = new File(outputDir, src.getName)

    outputFile.getParentFile.mkdirs()

    java.nio.file.Files.copy(src.toPath, outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)

    reporter.info(org.sireum.None(), Util.toolName, s"Wrote: ${outputFile}")

    return src.getName
  }

  def fileExists(path: org.sireum.String): B = {
    val f = new File(path.native)
    return f.exists()
  }
}
