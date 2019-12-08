// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.hamr.act.ast.{ASTObject, Assembly, BinarySemaphore, Composition, Consumes, Dataport, Direction, Emits, Instance, Method, Mutex, Procedure, Provides, Semaphore, Uses}
import org.sireum.message.Reporter
import org.sireum.ops.StringOps
import org.sireum.hamr.act.Util.reporter

@record class ActPrettyPrint {

  var resources : ISZ[Resource] = ISZ()
  var rootServer: String = ""
  var actContainer: Option[ActContainer] = None[ActContainer]()

  def tempEntry(destDir: String,
                container: ActContainer,
                cFiles: ISZ[String],
                cHeaderDirectories: ISZ[String],
                aadlRootDir: String,
                hamrIncludeDirs: ISZ[String],
                hamrStaticLib: Option[String],
                platform: ActPlatform.Type
               ): ISZ[Resource] = {

    rootServer = container.rootServer
    actContainer = Some(container)

    prettyPrint(container.models)

    var cmakeComponents: ISZ[ST] = container.cContainers.map((c: C_Container) => {
      var sourcePaths: ISZ[String] = ISZ()
      var includePaths: ISZ[String] = ISZ()

      if(c.sourceText.nonEmpty) {
        val dir = s"components/${c.component}/"
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

      sourcePaths = sourcePaths ++ c.cSources.map((r: Resource) => r.path) ++ c.externalCSources
      includePaths = includePaths ++ c.cIncludes.map((r: Resource) => StringUtil.getDirectory(r.path)) ++ c.externalCIncludeDirs :+ Util.DIR_INCLUDES
      
      StringTemplate.cmakeComponent(c.component, sourcePaths, includePaths, cFiles.nonEmpty,
        Util.hamrIntegration(platform) && hamrIncludeDirs.nonEmpty,
        Util.hamrIntegration(platform) && hamrStaticLib.nonEmpty)
    })

    container.monitors.foreach(m => {
      m match {
        case i: TB_Monitor => prettyPrint(ISZ(i.interface))
        case i: Ihor_Monitor => prettyPrint(ISZ(i.interfaceReceiver, i.interfaceSender))
      }

      cmakeComponents = cmakeComponents :+ StringTemplate.cmakeComponent(m.i.component.name,
        ISZ(m.cimplementation.path), ISZ(StringUtil.getDirectory(m.cinclude.path), Util.DIR_INCLUDES), F, F, F)

      add(s"${m.cimplementation.path}", m.cimplementation.content)
      add(s"${m.cinclude.path}", m.cinclude.content)
      add(s"${m.cinclude.path}", m.cinclude.content)
    })

    if(container.samplingPorts.nonEmpty) {
      
      val seqNumFile = s"${Util.DIR_SAMPLING_PORTS}/${StringTemplate.SeqNumName}.h"
      add(seqNumFile, StringTemplate.seqNumHeader())
      
      for (spi <- container.samplingPorts) {
        val header = StringTemplate.samplingPortHeader(spi)
        val impl = StringTemplate.samplingPortImpl(spi)
        val fname = spi.name

        add(spi.headerPath, header)
        add(spi.implPath, impl)
      }
    }
        
    var cmakeEntries: ISZ[ST] = ISZ()

    if(actContainer.get.requiresTimeServer) {
      cmakeEntries = cmakeEntries :+ st"includeGlobalComponents()"
    }
    
    if(Util.hamrIntegration(platform) && hamrStaticLib.nonEmpty) {
      cmakeEntries = cmakeEntries :+ StringTemplate.cmakeHamrExecuteProcess() :+
        StringTemplate.cmakeHamrLib(hamrStaticLib.get)
    }

    if(Util.hamrIntegration(platform) && hamrIncludeDirs.nonEmpty) {
      cmakeEntries = cmakeEntries :+ StringTemplate.cmakeHamrIncludes(hamrIncludeDirs)
    }

    if(container.connectors.nonEmpty) {
      cmakeEntries = cmakeEntries :+
        st"""# add path to connector templates
            |CAmkESAddTemplatesPath(../../../../components/templates/)
            |"""
    }

    if(cFiles.nonEmpty) {
      cmakeEntries = cmakeEntries :+ StringTemplate.cmakeAuxSources(cFiles, cHeaderDirectories)
    }

    cmakeEntries = cmakeEntries ++ cmakeComponents

    val cmakelist = StringTemplate.cmakeLists(Util.CMAKE_VERSION, container.rootServer, cmakeEntries)

    add(s"CMakeLists.txt", cmakelist)

    
    val c: ISZ[Resource] = container.cContainers.flatMap((x: C_Container) => x.cSources ++ x.cIncludes)
    val auxResources: ISZ[Resource] = container.auxFiles

    val ret = (resources ++ c ++ auxResources).map((o: Resource) => Util.createResource(s"${destDir}/${o.path}", o.content, T))

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

    var imports = actContainer.get.globalImports.map((m: String) => st"import ${m};")
    
    imports = imports ++ resources.map((o: Resource) => st"""import "${o.path}";""")
    
    val connectors: ISZ[ST] = actContainer.get.connectors.map(c => {
      val fromType: ST = if(c.from_template.nonEmpty) st"""template "${c.from_template.get}"""" else st"""TODO"""
      val toType: ST = if(c.from_template.nonEmpty) st"""template "${c.to_template.get}"""" else st"""TODO"""
      st"""connector ${c.name} {
          |  from ${c.from_type.name} ${fromType};
          |  to ${c.to_type.name} ${toType};
          |}
          |"""
    })

    val st =
      st"""${(imports, "\n")}
          |
          |${(connectors, "\n")}
          |assembly {
          |  ${comp.get}
          |
          |  configuration {
          |    ${a.configuration}
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
      visitInstance(i)
      instances = instances :+ st"""component ${i.component.name} ${i.name};"""
    }
    
    if(actContainer.get.requiresTimeServer) {
      instances = instances :+ st"component ${TimerUtil.TIMER_SERVER_CLASSIFIER} ${TimerUtil.TIMER_INSTANCE};"
    }

    for(c <- o.connections) {
      val from = s"${c.from_ends(0).component}.${c.from_ends(0).end}"
      val to = s"${c.to_ends(0).component}.${c.to_ends(0).end}"

      connections = connections :+
        st"""connection ${c.connectionType} ${c.name}(from ${from}, to ${to});"""
    }

    val st =
      st"""composition {
          |  ${(instances, "\n")}
          |
          |  ${(connections, "\n")}
          |}"""

    return Some(st)
  }

  def visitInstance(i: Instance): Option[ST] = {
    var name = i.component.name

    val st =
      st"""${(i.component.imports.map((i: String) => s"import ${i};"), "\n")}
         |
          |component ${name} {
          |  ${(i.component.includes.map((i: String) => s"include ${i};"), "\n")}
          |  ${if(i.component.control) "control;" else ""}
          |  ${(i.component.provides.map((p: Provides) => s"provides ${p.typ} ${p.name};"), "\n")}
          |  ${(i.component.uses.map((u: Uses) => s"uses ${u.typ} ${u.name};"), "\n")}
          |  ${(i.component.emits.map((e: Emits) => s"emits ${e.typ} ${e.name};"), "\n")}
          |  ${(i.component.consumes.map((c: Consumes) => s"consumes ${c.typ} ${c.name};"), "\n")}
          |  ${(i.component.dataports.map((d: Dataport) => s"dataport ${d.typ} ${d.name};"), "\n")}
          |  ${(i.component.binarySemaphores.map((b: BinarySemaphore) => s"has binary_semaphore ${b.name};"), "\n")}
          |  ${(i.component.semaphores.map((b: Semaphore) => s"has semaphore ${b.name};"), "\n")}
          |  ${(i.component.mutexes.map((m: Mutex) => s"has mutex ${m.name};"), "\n")}
          |}
          """

    if(Util.isMonitor(name)) {
      add(s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${name}/${name}.camkes", st)
    } else {
      add(s"${Util.DIR_COMPONENTS}/${name}/${name}.camkes", st)
    }

    return None()
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
}
