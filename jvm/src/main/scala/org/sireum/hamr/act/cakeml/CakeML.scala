// #Sireum

package org.sireum.hamr.act.cakeml

import com.sun.source.tree.CaseTree.CaseKind
import org.sireum._
import org.sireum.hamr.act.templates.CakeMLTemplate
import org.sireum.hamr.act.{Resource, Util, templates}
import org.sireum.hamr.act.utils.PathUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names, SeL4NixNamesUtil}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.ir.{Direction, ValueProp}
import org.sireum.ops.ISZOps

object CakeML {
  val PROP__CASE_PROPERTIES__COMPONENT_TYPE: String = "CASE_Properties::Component_Type"

  def requiresFFIs(aadlThread: AadlThread): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(aadlThread.component.properties, PROP__CASE_PROPERTIES__COMPONENT_TYPE) match {
      case Some(ValueProp("MONITOR")) => T
      case Some(ValueProp("FILTER")) => T
      case _ => F
    }
    return ret
  }

  def modelRequiresFFIs(symbolTable: SymbolTable): Boolean = {
    return ISZOps(symbolTable.getThreads()).exists(t => requiresFFIs(t))
  }

  def processThread(aadlThread: AadlThread,
                    basePackageName: String): Resource = {

    val names = Names(aadlThread.component, basePackageName)

    assert(requiresFFIs(aadlThread))
    val path: String = PathUtil.getComponentSourcePath(aadlThread)
    val classifierName = Util.getClassifier(aadlThread.component.classifier.get)
    val filename: String = Util.genCImplFilename(Util.brand(s"${classifierName}_ffi"))

    val apiHelperFilename: String = Util.genCHeaderFilename(SeL4NixNamesUtil.apiHelperFilename(names))

    var includes: ISZ[String] = ISZ("all.h", "camkes.h", "stdbool.h", apiHelperFilename)
    var globals: ISZ[ST] = ISZ()
    var methods: ISZ[ST] = ISZ()

    includes = includes :+ Util.genCHeaderFilename(names.cEntryPointAdapterName)

    globals = globals :+ CakeMLTemplate.entryPointGlobalVar(names.cBridgeEntryPoints, names.cEntryPointAdapterQualifiedName)
    globals = globals :+ CakeMLTemplate.thisGlobalVar(names.cComponentImpl)
    globals = globals ++ CakeMLTemplate.portIdsGlobalVars()
    globals = globals :+ CakeMLTemplate.initializedGlobalVar()

    val _includes = includes.map(m => s"#include <${m}>")

    { // initialization
      val statements: ISZ[ST] = ISZ(
        CakeMLTemplate.ffi_initializeEntryPoints(names.cBridgeEntryPoints, names.cEntryPointAdapterQualifiedName),
        CakeMLTemplate.ffi_initializeThis(names.cComponentImpl),
        CakeMLTemplate.initializePortIds(names.cBridgeEntryPoints)
      )

      methods = methods :+ CakeMLTemplate.initMethod(statements)
      methods = methods :+ CakeMLTemplate.ffi_initialization(statements)
    }

    methods = methods :+ CakeMLTemplate.checkAndReportBufferOverrun(names.cBridgeApi, names.cThisApi)

    methods = methods :+ CakeMLTemplate.dumpBuffer(names.cBridgeApi, names.cThisApi)

    methods = methods :+ CakeMLTemplate.ffi_artReceiveInput(names.cBridgeEntryPoints)

    methods = methods :+ CakeMLTemplate.ffi_artSendOutput(names.cBridgeEntryPoints)

    methods = methods ++ CakeMLTemplate.ffi_artLoggers(names.cBridgeApi, names.cThisApi)

    methods = methods ++ processPorts(aadlThread, basePackageName)

    methods = methods :+ CakeMLTemplate.postlude()

    val content: ST = st"""${(_includes, "\n")}
                          |
                          |${(globals, "\n")}
                          |
                          |${(methods, "\n\n")}
                          |"""

    val ret: Resource = Resource(
      path = s"${path}/${filename}",
      content = content,
      overwrite = T,
      makeExecutable = F)

    return ret
  }


  def processPorts(aadlThread: AadlThread, basePackageName: String): ISZ[ST] = {
    var methods: ISZ[ST] = ISZ()
    val ports = aadlThread.getFeatureEnds().filter(f => CommonUtil.isEventPort(f) || CommonUtil.isDataPort(f))
    val names = Names(aadlThread.component, basePackageName)

    methods = methods ++ ports.map(p => {
      val portName = CommonUtil.getLastName(p.identifier)

      p.direction match {
        case Direction.In =>
          val ffiName = CakeMLTemplate.ffi_getterMethodName(portName)
          val slangName = SeL4NixNamesUtil.apiHelperGetterMethodName(portName, names)
          CakeMLTemplate.ffi_get(ffiName, slangName)
        case Direction.Out =>
          val ffiName = CakeMLTemplate.ffi_setterMethodName(portName)
          val slangName = SeL4NixNamesUtil.apiHelperSetterMethodName(portName, names)
          val isDataPort = CommonUtil.isDataPort(p)
          CakeMLTemplate.ffi_send(ffiName, slangName, isDataPort)
        case x => halt(s"Not expecting direction ${x}")
      }
    })

    return methods
  }
}
