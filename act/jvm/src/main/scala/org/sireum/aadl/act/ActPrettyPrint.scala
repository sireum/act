// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.aadl.act.ast._

@record class BijiPrettyPrint() {

  var out : ISZ[(String, ST)] = ISZ()
  var rootServer: String = ""

  def tempEntry(destDir: String, container: ActContainer): ISZ[(String, ST)] = {
    rootServer = container.rootServer

    prettyPrint(container.models)

    val c = container.cContainers.flatMap((x: C_Container) => x.cSources ++ x.cIncludes).map((x: Resource) => (x.path, x.contents))
    val aux = container.auxFiles.map((x: Resource) => (x.path, x.contents))

    var components: ISZ[ST] = container.cContainers.map((c: C_Container) => {
      val sources = c.cSources.map((r: Resource) => r.path)
      val includes = c.cIncludes.map((r: Resource) => StringUtil.getDirectory(r.path)) :+ Util.DIR_INCLUDES
      StringTemplate.cmakeComponent(c.component, sources, includes)
    })

    container.monitors.foreach(m => {
      prettyPrint(ISZ(m.writer, m.interface))

      components = components :+ StringTemplate.cmakeComponent(m.i.component.name,
        ISZ(m.cimplementation.path), ISZ(StringUtil.getDirectory(m.cinclude.path), Util.DIR_INCLUDES))

      NativeIO.writeToFile(s"${destDir}/${m.cimplementation.path}", m.cimplementation.contents.render, T)
      NativeIO.writeToFile(s"${destDir}/${m.cinclude.path}", m.cinclude.contents.render, T)
      NativeIO.writeToFile(s"${destDir}/${m.cinclude.path}", m.cinclude.contents.render, T)
    })

    val cmakelist = StringTemplate.cmakeList(container.rootServer, container.rootServer, components, "3.7.2")
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
        println(s"Not handling: ${a}")
    }
    return None()
  }

  def visitAssembly(a: Assembly) : Option[ST] = {
    var imports: ISZ[ST] = ISZ()
    var children: ISZ[ST] = ISZ()

    val comp = visitComposition(a.composition)

    imports = imports ++ out.map((o: (String, ST)) => st"""import "${o._1}";""")

    val st =
      st"""import <std_connector.camkes>;
          |
          |${(imports, "\n")}
          |
          |connector seL4TimeServer {
          |  from Procedures template "seL4TimeServer-from.template.c";
          |  to Procedure template "seL4TimeServer-to.template.c";
          |}
          |
          |connector seL4GlobalAsynchCallback {
          |  from Events template "seL4GlobalAsynchCallback-from.template.c";
          |  to Event template "seL4GlobalAsynchCallback-to.template.c";
          |}
          |
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
        st"""connection ${c.connector.name} ${c.name}(from ${from}, to ${to});"""
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
}