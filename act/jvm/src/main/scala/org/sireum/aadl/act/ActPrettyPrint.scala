// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.aadl.act.ast._

@record class BijiPrettyPrint() {

  var out : ISZ[(String, ST)] = ISZ()

  def tempEntry(destDir: String, container: ActContainer): ISZ[(String, ST)] = {
    prettyPrint(container.models)

    (out ++ container.auxFiles).foreach(o => NativeIO.writeToFile(s"${destDir}/${o._1}", o._2.render, T))

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
          |assembly {
          |  ${comp.get}
          |}
          |"""

    add("assembly.camkes", st)

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