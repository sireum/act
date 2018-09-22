// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.ops._
import org.sireum.aadl.ir

object Util {

  val GEN_ARTIFACT_PREFIX: String = "tb"

  val MONITOR_COMP_SUFFIX: String  = "Monitor"
  val MONITOR_DIRECTORY_NAME: String = "tb_Monitors"
  val MONITOR_NOTIFICATION_TYPE: String  = "QueuedData"

  val NOTIFICATION_TYPE: String = "Notification"

  val INTERFACE_PREFIX: String  = s"${GEN_ARTIFACT_PREFIX}_Monitor"

  val PROP_ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val PROP_QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val DEFAULT_QUEUE_SIZE: Z = 1

  val CONNECTOR_SEL4_NOTIFICATION: String = "sel4Notification"
  val CONNECTOR_RPC: String = "seL4RPCCall"

  def getClassifierFullyQualified(c : ir.Classifier) : String = {
    val a = replaceAll(c.name, "::", "__")
    val b = replaceAll(a, ".", "_")
    return b
  }

  def getClassifier(c : ir.Classifier) : String = {
    var s = StringOps(c.name)
    var index = s.lastIndexOf(':') + 1
    s = StringOps(s.substring(index, c.name.size))
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

  def isMonitor(s: String) : B = {
    val ss = StringOps(s)
    return ss.startsWith(GEN_ARTIFACT_PREFIX) && ss.endsWith(MONITOR_COMP_SUFFIX)
  }

  def getMonitorName(comp: ir.Component, feature: ir.FeatureEnd) : String = {
    val cname = Util.getLastName(comp.identifier)
    val fname = Util.getLastName(feature.identifier)
    return s"${GEN_ARTIFACT_PREFIX}_${cname}_${fname}_${MONITOR_COMP_SUFFIX}"
  }

  def getTypeHeaderFileName(c: ir.Component) : String = {
    assert(c.category == ir.ComponentCategory.Process)
    val processor = getDiscreetPropertyValue[ir.ReferenceProp](c.properties, PROP_ACTUAL_PROCESSOR_BINDING)
    val procName: String = processor match {
      case Some(v) => getLastName(v.value)
      case _ => halt("No processor binding provided")
    }
    return s"${GEN_ARTIFACT_PREFIX}_${procName}_types.h"
  }

  def getInterfaceName(feature: ir.FeatureEnd): String = {
    val typeName = getClassifierFullyQualified(feature.classifier.get)

    if(feature.category == ir.FeatureCategory.DataPort) {
      return s"${INTERFACE_PREFIX}_${typeName}"
    } else {
      return s"${INTERFACE_PREFIX}_${typeName}_${getQueueSize(feature)}"
    }
  }

  def getQueueSize(f: ir.Feature): Z = {
    val x: Option[ir.UnitProp] = getDiscreetPropertyValue[ir.UnitProp](f.properties, PROP_QUEUE_SIZE)
    val v: Z = x match {
      case Some(v) =>
        R(v.value) match {
          case Some(vv) => conversions.R.toZ(vv)
          case _ => DEFAULT_QUEUE_SIZE
        }
      case _ => DEFAULT_QUEUE_SIZE
    }
    return v
  }



  def portName(f: ir.FeatureEnd, num: Option[Z]): String = {
    return s"${GEN_ARTIFACT_PREFIX}_${Util.getLastName(f.identifier)}${if(num.nonEmpty) num.get else ""}"
  }

  def portNotificationName(f: ir.FeatureEnd): String = {
    return s"${GEN_ARTIFACT_PREFIX}_${Util.getLastName(f.identifier)}_notification"
  }



  def replaceAll(s: String, f: String, replacement: String): String = {
    var split: ISZ[String] = ISZ()

    val sops = StringOps(s)
    val cms = conversions.String.toCms(s)
    val fcms = conversions.String.toCms(f)

    var start: Z = 0
    var index: Z = 0

    for(i <- 0 until cms.length.toInt) {
      if(cms(i) == fcms(index)){
        index = index + 1
      }
      if(index == fcms.size) {
        split = split :+ sops.substring(start, i - fcms.size + 1)
        start = i + 1
        index = 0
      }
    }
    if(start < cms.length) {
      split = split :+ sops.substring(start, cms.length)
    }

    return  st"""${(split, replacement)}""".render
  }

  def toLowerCase(s: String):String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map(c => COps(c).toLower))
  }
}

@datatype class Monitor (i: Instance,               // camkes monitor
                         p: Procedure,              // camkes interface
                         index: Z,                  // fan-out index
                         ci: ir.ConnectionInstance) // aadl connection