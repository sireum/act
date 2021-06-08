// #Sireum

package org.sireum.hamr.act.cakeml

import org.sireum._
import org.sireum.hamr.act.templates.CakeMLTemplate
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.{CommonUtil, Names, NixSeL4NameUtil}
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object CakeML {

  def processThread(aadlThread: AadlThread,
                    basePackageName: String,
                    symbolTable: SymbolTable,
                    aadlRoot: Option[String],
                    reporter: Reporter): ISZ[Resource] = {

    val names = Names(aadlThread.component, basePackageName)

    assert(aadlThread.isCakeMLComponent(), s"${aadlThread.identifier} is not a CakeML component")
    val path: String = PathUtil.getComponentSourcePath(aadlThread, symbolTable)
    val classifierName = Util.getClassifier(aadlThread.component.classifier.get)
    val filename: String = Util.genCImplFilename(Util.brand(s"${classifierName}_ffi"))

    val apiHelperFilename: String = Util.genCHeaderFilename(NixSeL4NameUtil.apiHelperFilename(names))

    var includes: ISZ[String] = ISZ("all.h", "camkes.h", "stdbool.h", apiHelperFilename)
    var globals: ISZ[ST] = ISZ()
    var methods: ISZ[ST] = ISZ()

    includes = includes :+ Util.genCHeaderFilename(names.cEntryPointAdapterName)

    globals = globals :+ CakeMLTemplate.entryPointGlobalVar(names.cBridgeEntryPoints, names.cEntryPointAdapterQualifiedName)
    globals = globals ++ CakeMLTemplate.portIdsGlobalVars()
    globals = globals :+ CakeMLTemplate.initializedGlobalVar()

    val _includes: ISZ[String] = includes.map((m : String) => s"#include <${m}>")

    val logInfo = NixSeL4NameUtil.apiHelperLoggerMethodName("logInfo", names.cComponentType)

    { // initialization
      val statements: ISZ[ST] = ISZ(
        CakeMLTemplate.ffi_initializeEntryPoints(names.cBridgeEntryPoints, names.cEntryPointAdapterQualifiedName),
        CakeMLTemplate.initializePortIds(names.cBridgeEntryPoints)
      )

      methods = methods :+ CakeMLTemplate.initMethod(statements, filename)
      methods = methods :+ CakeMLTemplate.ffi_initialization(statements, filename)
    }

    methods = methods :+ CakeMLTemplate.checkAndReportBufferOverrun(logInfo, filename)

    methods = methods :+ CakeMLTemplate.dumpBuffer(logInfo, filename)

    methods = methods :+ CakeMLTemplate.ffi_artReceiveInput(names.cBridgeEntryPoints, filename)

    methods = methods :+ CakeMLTemplate.ffi_artSendOutput(names.cBridgeEntryPoints, filename)

    methods = methods ++ CakeMLTemplate.ffi_artLoggers(names.cComponentType, filename)

    methods = methods ++ processPorts(aadlThread, basePackageName, filename)

    methods = methods :+ CakeMLTemplate.postlude(!symbolTable.hasVM())

    val content: ST = CakeMLTemplate.ffiTemplate(_includes, globals, methods)

    var ret: ISZ[Resource] = ISZ(Resource(
      path = s"${path}/${filename}",
      content = content,
      overwrite = T,
      makeExecutable = F))

    val sourceText: ISZ[String] = PropertyUtil.getSourceText(aadlThread.component.properties)

      if(sourceText.nonEmpty) {
        if(sourceText.size > 1) {
          val msg = s"Expecting a single entry for ${OsateProperties.PROGRAMMING_PROPERTIES__SOURCE_TEXT} but found ${sourceText.size}."
          reporter.error(aadlThread.component.identifier.pos, Util.toolName, msg)
        }
        var cand = Os.path(sourceText(0))
        if(!cand.exists && aadlRoot.nonEmpty) {
          cand = Os.path(aadlRoot.get) / sourceText(0)
        }

        if(!cand.exists) {
          val msg = s"Couldn't locate ${sourceText(0)}"
          reporter.error(aadlThread.component.identifier.pos, Util.toolName, msg)
        } else {
          val contents = cand.read
          ret = ret :+ Resource(
            path = s"${path}/${cand.name}",
            content = st"${contents}",
            overwrite = T,
            makeExecutable = F)
        }
      } else {
        val assemblyFilename = Util.brand(s"${classifierName}.S")
        ret = ret :+ Resource(
          path = s"${path}/${assemblyFilename}",
          content = CakeMLTemplate.emptyAssemblyFile(),
          overwrite = F,
          makeExecutable = F)
      }

    return ret
  }


  def processPorts(aadlThread: AadlThread, basePackageName: String, fileUri: String): ISZ[ST] = {
    var methods: ISZ[ST] = ISZ()
    val ports: ISZ[ir.FeatureEnd] = aadlThread.getFeatureEnds().filter((f: ir.FeatureEnd) => CommonUtil.isEventPort(f) || CommonUtil.isDataPort(f))
    val names = Names(aadlThread.component, basePackageName)

    val _ports: ISZ[ST] = ports.map((p: ir.FeatureEnd) => {
      val portName = CommonUtil.getLastName(p.identifier)

      p.direction match {
        case ir.Direction.In =>
          val ffiName = CakeMLTemplate.ffi_getterMethodName(portName)
          val slangName = NixSeL4NameUtil.apiHelperGetterMethodName(portName, names)
          CakeMLTemplate.ffi_get(ffiName, slangName, fileUri, p.category)
        case ir.Direction.Out =>
          val ffiName = CakeMLTemplate.ffi_setterMethodName(portName)
          val slangName = NixSeL4NameUtil.apiHelperSetterMethodName(portName, names)
          val isDataPort = CommonUtil.isDataPort(p)
          CakeMLTemplate.ffi_send(ffiName, slangName, isDataPort, fileUri)
        case x => halt(s"Not expecting direction ${x}")
      }
    })

    methods = methods ++ _ports

    return methods
  }
}
