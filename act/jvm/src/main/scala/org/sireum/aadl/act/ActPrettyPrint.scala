// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.aadl.act.ast._

@record class BijiPrettyPrint() {

  var out : ISZ[(String, ST)] = ISZ()
  var rootServer: String = ""
  var actContainer: Option[ActContainer] = None[ActContainer]()

  def tempEntry(destDir: String, container: ActContainer,
                cFiles: ISZ[String],
                aadlRootDir: String
               ): ISZ[(String, ST)] = {
    rootServer = container.rootServer
    actContainer = Some(container)

    prettyPrint(container.models)

    val c = container.cContainers.flatMap((x: C_Container) => x.cSources ++ x.cIncludes).map((x: Resource) => (x.path, x.contents))
    val aux = container.auxFiles.map((x: Resource) => (x.path, x.contents))


    var cmakeComponents: ISZ[ST] = container.cContainers.map((c: C_Container) => {
      var sources: ISZ[String] = ISZ()
      var includes: ISZ[String] = ISZ()

      if(c.sourceText.nonEmpty) {
        val dir = s"components/${c.component}/"
        val rootDestDir = s"${destDir}/${dir}"

        for(st <- c.sourceText) {
          val path = s"${aadlRootDir}/${st}"
          if(NativeIO.fileExists(path)) {

            if(StringUtil.endsWith(st, ".c")) {
              val fname = NativeIO.copyFile(path, s"${rootDestDir}/src")
              sources = sources :+ s"${dir}/src/${fname}"
            } else if(StringUtil.endsWith(st, ".h")) {
              val fname = NativeIO.copyFile(path, s"${rootDestDir}/includes")
              includes = includes :+ s"${dir}/includes/${fname}"
            } else {
              Util.addWarning(s"${path} does not appear to be a valid C source file")
            }
          } else {
            Util.addWarning(s"${path} does not exist")
          }
        }
      }

      sources = sources ++ c.cSources.map((r: Resource) => r.path)
      includes = includes ++ c.cIncludes.map((r: Resource) => StringUtil.getDirectory(r.path)) :+ Util.DIR_INCLUDES
      StringTemplate.cmakeComponent(c.component, sources, includes, cFiles.nonEmpty)
    })

    container.monitors.foreach(m => {
      prettyPrint(ISZ(m.writer, m.interface))

      cmakeComponents = cmakeComponents :+ StringTemplate.cmakeComponent(m.i.component.name,
        ISZ(m.cimplementation.path), ISZ(StringUtil.getDirectory(m.cinclude.path), Util.DIR_INCLUDES), cFiles.nonEmpty)

      NativeIO.writeToFile(s"${destDir}/${m.cimplementation.path}", m.cimplementation.contents.render, T)
      NativeIO.writeToFile(s"${destDir}/${m.cinclude.path}", m.cinclude.contents.render, T)
      NativeIO.writeToFile(s"${destDir}/${m.cinclude.path}", m.cinclude.contents.render, T)
    })


    val auxCFiles: ISZ[ST] = cFiles.map(c => StringTemplate.auxTemplate(c))
    val cmakelist = StringTemplate.cmakeList(container.rootServer, container.rootServer, cmakeComponents, Util.CMAKE_VERSION,
      auxCFiles, container.connectors.nonEmpty)
    NativeIO.writeToFile(s"${destDir}/CMakeLists.txt", cmakelist.render, T)

    (out ++ c ++ aux).foreach(o => NativeIO.writeToFile(s"${destDir}/${o._1}", o._2.render, T))
    return out
  }

  def prettyPrint(objs: ISZ[ASTObject]): ISZ[(String, ST)] = {
    for(a <- objs) {
      visit(a)
    }
    return out
  }

  def visit(a: ASTObject) : Option[ST] = {
    a match {
      case o: Assembly => visitAssembly(o)
      case o: Procedure => visitProcedure(o)
      case _ =>
        Util.addError(s"Not handling: ast object ${a}")
    }
    return None()
  }

  def visitAssembly(a: Assembly) : Option[ST] = {
    var children: ISZ[ST] = ISZ()

    val comp = visitComposition(a.composition)

    val imports = out.map((o: (String, ST)) => st"""import "${o._1}";""")

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
      st"""import <std_connector.camkes>;
          |
          |${(imports, "\n")}
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
          |  ${(i.component.binarySemaphores.map((b: BinarySemaphore) => s"has binary_semaphore ${b.name};"), "\n")}
          |  ${(i.component.semaphores.map((b: Semaphore) => s"has semaphore ${b.name};"), "\n")}
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

  def add(s: String, st: ST) : Unit = {
    val p = (s, st)
    out = out :+ p
  }
}


@ext object NativeIO {
  def writeToFile(path: String, contents: String, overwrite: B): Unit = $
  def copyFile(srcPath: org.sireum.String, outputPath: org.sireum.String): String = $
  def fileExists(path: String): B = $
}