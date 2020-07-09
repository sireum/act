// #Sireum

package org.sireum.hamr.act.utils

import org.sireum._
import org.sireum.hamr.act.Util
import org.sireum.hamr.codegen.common.symbols.AadlThread

object PathUtil {
  def getComponentSourcePath(aadlThread: AadlThread): String = {
    val componentDirName = Util.getClassifier(aadlThread.component.classifier.get)
    return s"${Util.DIR_COMPONENTS}/${componentDirName}/src"
  }

  def getComponentGlueCodeFilename(aadlThread: AadlThread): String = {
    return Util.brand(Util.getClassifier(aadlThread.component.classifier.get))
  }

  def getComponentGlueCodeHeaderFilename(aadlThread: AadlThread): String = {
    return Util.genCHeaderFilename(getComponentGlueCodeFilename(aadlThread))
  }

  def getComponentGlueCodeImplementationFilename(aadlThread: AadlThread): String = {
    return Util.genCImplFilename(getComponentGlueCodeFilename(aadlThread))
  }
}
