// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.connections.{ConnectorContainer, ConnectorTemplate}
import org.sireum.hamr.act.periodic.{PacerTemplate, PeriodicDispatcherTemplate}
import org.sireum.hamr.act.templates.{CMakeTemplate, CakeMLTemplate, SlangEmbeddedTemplate, StringTemplate}
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.DirectoryUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.util.ExperimentalOptions
import org.sireum.ops.StringOps

@record class ActPrettyPrint {

  var resources : ISZ[Resource] = ISZ()
  var rootServer: String = ""
  var actContainer: Option[ActContainer] = None[ActContainer]()

  def tempEntry(destDir: String,
                container: ActContainer,
                cFiles: ISZ[String],
                cHeaderDirectories: ISZ[String],
                aadlRootDir: String,
                slangLibInstanceNames: ISZ[String],
                symbolTable: SymbolTable,
                options: ActOptions
               ): ISZ[Resource] = {

    val platform = options.platform

    rootServer = container.rootServer
    actContainer = Some(container)

    prettyPrint(container.models)

    if(ExperimentalOptions.generateDotGraphs(options.experimentalOptions)){
      val assembly = container.models(0).asInstanceOf[Assembly]
      val dot = org.sireum.hamr.act.dot.HTMLDotGenerator.dotty(assembly, F)
      add(s"graph.dot", dot)

      val dotSimple = org.sireum.hamr.act.dot.HTMLDotGenerator.dotty(assembly, T)
      add(s"graph_simple.dot", dotSimple)
    }

    var cmakeComponents: ISZ[ST] = container.cContainers.map((c: C_Container) => {
      var sourcePaths: ISZ[String] = ISZ()
      var includePaths: ISZ[String] = ISZ()

      if(c.sourceText.nonEmpty) {
        val dir = s"${Util.DIR_COMPONENTS}/${c.componentId}/"
        val rootDestDir = dir

        for(st <- c.sourceText) {
          val path = s"${aadlRootDir}/${st}"
          val p = Os.path(path)

          if(p.exists) {
            if(StringOps(st).endsWith(".c")) {
              val fname = s"${rootDestDir}/src/${p.name}"
              add(fname, st"""${p.read}""")

              sourcePaths = sourcePaths :+ fname

            }
            else if(StringOps(st).endsWith(".h")) {
              val fname = s"${rootDestDir}/includes/${p.name}"
              add(fname, st"""${p.read}""")
            }
            else {
              reporter.warn(None(), Util.toolName, s"${path} does not appear to be a valid C source file")
            }
          } else {
            reporter.warn(None(), Util.toolName, s"${path} does not exist")
          }
        }
      }

      sourcePaths = sourcePaths ++ c.cSources.map((r: Resource) => r.path) ++ c.cmakeSOURCES
      includePaths = includePaths ++ c.cIncludes.map((r: Resource) =>
        Util.getDirectory(r.path)) ++ c.cmakeINCLUDES

      val slangLib: Option[String] = getSlangLibrary(c.componentId, platform)

      val hasAux = cFiles.nonEmpty && c.componentId != PeriodicDispatcherTemplate.DISPATCH_CLASSIFIER

      CMakeTemplate.cmake_DeclareCamkesComponent(
        componentName = c.componentId,
        sources = sourcePaths,
        includes = includePaths,
        libs = c.cmakeLIBS,
        hasAux = hasAux,
        slangLib = slangLib)
    })

    for (m <- container.monitors) {
      m match {
        case i: TB_Monitor => prettyPrint(ISZ(i.interface))
        case i: Ihor_Monitor => prettyPrint(ISZ(i.interfaceReceiver, i.interfaceSender))
      }

      val slangLib: Option[String] = if(platform == ActPlatform.SeL4) Some(Util.SlangTypeLibrary) else None()

      cmakeComponents = cmakeComponents :+ CMakeTemplate.cmake_DeclareCamkesComponent(
        componentName = m.i.component.name,
        sources = ISZ(m.cimplementation.path),
        includes = ISZ(Util.getDirectory(m.cinclude.path), Util.getTypeIncludesPath()),
        libs = ISZ(),
        hasAux = F,
        slangLib = slangLib)

      add(s"${m.cimplementation.path}", m.cimplementation.content)
      add(s"${m.cinclude.path}", m.cinclude.content)
      add(s"${m.cinclude.path}", m.cinclude.content)
    }

    if(container.samplingPorts.nonEmpty) {

      val seqNumFile = s"${Util.getTypeRootPath()}/includes/${Util.genCHeaderFilename(StringTemplate.SeqNumName)}"
      add(seqNumFile, StringTemplate.seqNumHeader())

      for (spi <- container.samplingPorts) {
        val header = StringTemplate.samplingPortHeader(spi)
        val impl = StringTemplate.samplingPortImpl(spi)

        add(spi.headerPath, header)
        add(spi.implPath, impl)
      }
    }

    var cmakeEntries: ISZ[ST] = ISZ()

    if(actContainer.get.requiresTimeServer) {
      cmakeEntries = cmakeEntries :+ st"includeGlobalComponents()"
    }

    if(Util.hamrIntegration(platform) && slangLibInstanceNames.nonEmpty) {
      for(slangLib <- slangLibInstanceNames){
        val path = s"$${CMAKE_CURRENT_LIST_DIR}/${DirectoryUtil.DIR_SLANG_LIBRARIES}/${slangLib}"
        cmakeEntries = cmakeEntries :+ CMakeTemplate.cmake_add_subdirectory(path)
      }
    }

    // add type library after hamr libs as it will depend on the former if sel4 platform
    cmakeEntries = cmakeEntries :+ CMakeTemplate.cmake_addSubDir_TypeLibrary()

    // hmm, not sure when this should be added
    if(symbolTable.hasVM()) {
      cmakeEntries = cmakeEntries :+ CMakeTemplate.cmake_addSubDir_VM()
    }

    var auxResources: ISZ[Resource] = ISZ()

    if(container.connectors.nonEmpty) {

      cmakeEntries = cmakeEntries :+ st"""# add path to connector templates
                                         |CAmkESAddTemplatesPath($${CMAKE_CURRENT_LIST_DIR}/templates)"""

      for(conn <- container.connectors) {
        val connTemplate: ConnectorTemplate = conn.connectorTemplate
        cmakeEntries = cmakeEntries :+ st"""DeclareCAmkESConnector(${conn.connectorName}
                                           |  FROM ${connTemplate.fromTemplateName}
                                           |  FROM_LIBS SB_Type_Library
                                           |  TO ${connTemplate.toTemplateName}
                                           |  TO_LIBS SB_Type_Library
                                           |)"""

        if(connTemplate.fromTemplate.nonEmpty) {
          auxResources = auxResources :+ Resource(
            path = s"templates/${connTemplate.fromTemplateName}",
            content = connTemplate.fromTemplate.get,
            overwrite = T,
            makeExecutable = F)
        }

        if(connTemplate.toTemplate.nonEmpty) {
          auxResources = auxResources :+ Resource(
            path = s"templates/${connTemplate.toTemplateName}",
            content = connTemplate.toTemplate.get,
            overwrite = T,
            makeExecutable = F)
        }
      }
    }

    if(cFiles.nonEmpty) {
      cmakeEntries = cmakeEntries :+ CMakeTemplate.cmakeAuxSources(cFiles, cHeaderDirectories)
    }

    cmakeEntries = cmakeEntries ++ cmakeComponents

    var preludes: ISZ[ST] = ISZ(CMakeTemplate.cmake_add_definitions(ISZ("CAMKES")))

    if(symbolTable.hasCakeMLComponents()) {
      val filename = "CMake_CakeMlOptions.cmake"
      add(filename, CMakeTemplate.cmake_add_options(CakeMLTemplate.CAKEML_OPTIONS))
      preludes = preludes :+ CMakeTemplate.include(s"$${CMAKE_CURRENT_LIST_DIR}/${filename}")
    }

    if(platform == ActPlatform.SeL4) {
      val filename= "CMake_TranspilerOptions.cmake"
      add(filename, CMakeTemplate.cmake_add_options(SlangEmbeddedTemplate.TRANSPILER_OPTIONS))
      preludes = preludes :+ CMakeTemplate.include(s"$${CMAKE_CURRENT_LIST_DIR}/${filename}")
    }

    val cmakelist = CMakeTemplate.cmakeLists(container.rootServer, cmakeEntries, preludes)

    add("CMakeLists.txt", cmakelist)


    val c: ISZ[Resource] = container.cContainers.flatMap((x: C_Container) => x.cSources ++ x.cIncludes)
    auxResources = auxResources ++ container.auxFiles


    addExeResource(s"${PathUtil.DIR_BIN}/run-camkes.sh", StringTemplate.runCamkesScript(symbolTable.hasVM()))

    // add dest dir to path
    val ret: ISZ[Resource] = (resources ++ c ++ auxResources).map((o: Resource) => Resource(
      path = s"${destDir}/${o.path}",
      content = o.content,
      overwrite = o.overwrite,
      makeExecutable = o.makeExecutable
    ))

    return ret
  }

  def prettyPrint(objs: ISZ[ASTObject]): Unit = {
    for(a <- objs) {
      visit(a)
    }
  }

  def visit(a: ASTObject) : Option[ST] = {
    a match {
      case o: Assembly => visitAssembly(o)
      case o: Procedure => visitProcedure(o)
      case _ =>
        reporter.error(None(), Util.toolName, s"Not handling: ast object ${a}")
    }
    return None()
  }

  def visitAssembly(a: Assembly) : Option[ST] = {
    var children: ISZ[ST] = ISZ()

    val comp = visitComposition(a.composition)

    val preprocessorIncludes = actContainer.get.globalPreprocessorIncludes.map((m: String) => s"#include ${m}")

    var imports = Set.empty[String] ++ actContainer.get.globalImports.map((m: String) => s"import ${m};")

    imports = imports ++ resources.map((o: Resource) => s"""import "${o.path}";""")

    val connectors: ISZ[ST] = actContainer.get.connectors.map((c: ConnectorContainer) => {
      val conn = c.assemblyEntry

      val srcWithThreads: Option[ST] = if(conn.from_threads >= 0) Some(st" with ${conn.from_threads} threads") else None()
      val dstWithThreads: Option[ST] = if(conn.to_threads >= 0) Some(st" with ${conn.to_threads} threads") else None()

      val fromType: Option[ST] = if(conn.from_template.nonEmpty) Some(st""" template "${conn.from_template.get}"""") else None()
      val toType: Option[ST] = if(conn.from_template.nonEmpty) Some(st""" template "${conn.to_template.get}"""") else None()

      val attributes: Option[ST] = if(conn.attributes.nonEmpty) {
        val i = conn.attributes.map((m: ast.Attribute) => st"attribute ${m.typ} ${m.name} = ${m.value};")
        Some(st"${(i, "\n")}")
      } else { None() }

      st"""connector ${conn.name} {
          |  from ${conn.from_type.name}${fromType}${srcWithThreads};
          |  to ${conn.to_type.name}${toType}${dstWithThreads};
          |  ${attributes}
          |}
          |"""
    })

    val st =
      st"""${StringTemplate.doNotEditComment()}
          |
          |${(preprocessorIncludes, "\n")}
          |${(imports.elements, "\n")}
          |
          |${(connectors, "\n")}
          |assembly {
          |  ${comp.get}
          |
          |  configuration {
          |    ${(a.configuration, "\n")}
          |
          |    ${(a.configurationMacros, "\n")}
          |  }
          |}
          |"""

    add(s"${rootServer}.camkes", st)

    return None()
  }

  def visitComposition(o: Composition) : Option[ST] = {
    assert(o.groups.isEmpty)
    assert(o.exports.isEmpty)

    var instances: ISZ[ST] = ISZ()
    var connections: ISZ[ST] = ISZ()

    for(i <- o.instances) {
      visitCamkesComponent(i.component)
      instances = instances :+ st"""component ${i.component.name} ${i.name};"""
    }

    if(actContainer.get.requiresTimeServer) {
      instances = instances :+ st"component ${PeriodicDispatcherTemplate.TIMER_SERVER_CLASSIFIER} ${PeriodicDispatcherTemplate.TIMER_INSTANCE};"
    }

    for(c <- o.connections) {
      val froms = c.from_ends.map((m: ast.ConnectionEnd) => st"from ${m.component}.${m.end}")
      val tos = c.to_ends.map((m: ast.ConnectionEnd) => st"to ${m.component}.${m.end}")

      connections = connections :+
        st"""connection ${c.connectionType} ${c.name}(${(froms, ", ")}, ${(tos, ", ")});"""
    }


    val st =
      st"""composition {
          |  ${(instances, "\n")}
          |
          |  ${(connections, "\n")}
          |  ${(o.externalEntities, "\n")}
          |}"""

    /*
    println("--------")
    println(st.render)
    println("--------")
    */

    return Some(st)
  }

  def visitCamkesComponent(component: CamkesComponent): Option[ST] = {
    val ret: Option[ST] = component match {
      case c: Component => {
        var name = c.name

        val st =
          st"""${(c.imports.map((i: String) => s"import ${i};"), "\n")}
              |${(c.preprocessorIncludes.map((i: String) => s"#include ${i}"), "\n")}
              |component ${name} {
              |  ${(c.includes.map((i: String) => s"include ${i};"), "\n")}
              |  ${if(c.control) "control;" else ""}
              |  ${(c.provides.map((p: Provides) => StringTemplate.provides(p)), "\n")}
              |  ${(c.uses.map((u: Uses) => StringTemplate.uses(u)), "\n")}
              |  ${(c.emits.map((e: Emits) => StringTemplate.emits(e)), "\n")}
              |  ${(c.consumes.map((c: Consumes) => StringTemplate.consumes(c)), "\n")}
              |  ${(c.dataports.map((d: Dataport) => StringTemplate.dataport(d)), "\n")}
              |  ${(c.binarySemaphores.map((b: BinarySemaphore) => s"has binary_semaphore ${b.name};"), "\n")}
              |  ${(c.semaphores.map((b: Semaphore) => s"has semaphore ${b.name};"), "\n")}
              |  ${(c.mutexes.map((m: Mutex) => s"has mutex ${m.name};"), "\n")}
              |  ${(c.externalEntities.map((s: String) => s), "\n")}
              |}
              |"""

        if(Util.isMonitor(name)) {
          add(s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${name}/${name}.camkes", st)
        } else {
          add(s"${Util.DIR_COMPONENTS}/${name}/${name}.camkes", st)
        }
        None()
      }
      case c: LibraryComponent => None()
    }
    return ret
  }

  def visitProcedure(o: Procedure): Option[ST] = {
    var methods: ISZ[ST] = ISZ()
    for(m <- o.methods) {
      methods = methods :+ visitMethod(m).get
    }

    val st =
      st"""procedure ${o.name} {
          |  ${(o.includes.map((i: String) => s"include ${i};"), "\n")}
          |  ${(methods, "\n")}
          |};"""

    add(s"${Util.DIR_INTERFACES}/${o.name}.idl4", st)

    return None()
  }

  def visitMethod(o: Method) : Option[ST] = {
    var params: ISZ[ST] = ISZ()
    for(p <- o.parameters) {
      val dir: String = p.direction match {
        case Direction.In => "in"
        case Direction.Out => "out"
        case Direction.Refin => "refin"
      }
      params = params :+ st"""${dir} ${p.typ} ${p.name}"""
    }

    val retType: String = o.returnType match {
      case Some(r) => r
      case _ => "void"
    }

    val st = st"""${retType} ${o.name}(${(params, ",")});"""
    return Some(st)
  }

  def add(path: String, content: ST) : Unit = {
    resources = resources :+ Util.createResource(path, content, T)
  }

  def addExeResource(path: String, content: ST) : Unit = {
    resources = resources :+ Util.createExeResource(path, content, T)
  }

  def getSlangLibrary(componentName: String, platform: ActPlatform.Type): Option[String] = {
    val libName: String =
      if(componentName == PacerTemplate.PACER_COMPONENT_TYPE ||
        componentName == PeriodicDispatcherTemplate.DISPATCH_CLASSIFIER) {
        Util.SlangTypeLibrary
      } else {
        componentName
      }
    return if(platform == ActPlatform.SeL4) Some(libName) else None()
  }
}
