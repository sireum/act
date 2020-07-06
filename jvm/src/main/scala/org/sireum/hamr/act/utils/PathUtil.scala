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
}
