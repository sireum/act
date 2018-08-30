// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.ops._
import org.sireum.aadl.ir

object Util {

  def getClassifier(c : ir.Classifier) : String = {
    var s = StringOps(c.name)
    var index = s.lastIndexOf(':') + 1
    s = StringOps(s.substring(index, c.name.size))
    //index = s.indexOf('.') // FIXME
    index = s.lastIndexOf('.')
    if(index >= 0) {
      return s.substring(0, index)
    } else {
      return s.s
    }
  }

  @pure def getDiscreetPropertyValue[T](properties: ISZ[ir.Property], propertyName: String): Option[T] = {
    for (p <- properties if getLastName(p.name) == propertyName) {
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    }
    return None[T]
  }


  def getLastName(n : ir.Name) : String = {
    return n.name(n.name.size - 1)
  }

  def getName(n : ir.Name) : String = {
    return st"""${(n.name, "_")}""".render
  }
}
