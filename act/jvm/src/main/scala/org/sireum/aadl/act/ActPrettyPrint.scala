// #Sireum

package org.sireum.aadl.act

import org.sireum._

@record class BijiPrettyPrint() {

  val dirComponents: String = "components"
  val dirInterfaces: String = "interfaces"

  var out : ISZ[(String, ST)] = ISZ()

  def tempEntry(destDir: String, objs: ISZ[ASTObject]): ISZ[(String, ST)] = {
    prettyPrint(objs)


    for(o <- out) {
      NativeIO.writeToFile(s"${destDir}/${o._1}", o._2.render, T)
    }

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

    for(o <- out) {
      imports = imports :+ st"""import "${o._1}";"""
    }

    val st =
      st"""import <std_connector.camkes>;
          |
          |${(imports, "\n")}
          |
          |assembly {
          |  ${comp.get}
          |}
          |"""

    add("cakeml-filter.camkes", st)

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
        st"""connection seL4RPCCall ${c.name}(from ${from}, to ${to});"""
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
      st"""component ${name} {
          |  ${(i.component.includes.map(i => s"include ${i};"), "\n")}
          |  ${if(i.component.control) "control;" else ""}
          |  ${(i.component.provides.map(p => s"provides ${p.typ} ${p.name};"), "\n")}
          |  ${(i.component.uses.map(u => s"uses ${u.typ} ${u.name};"), "\n")}
          |  ${(i.component.emits.map(e => s"emits ${e.typ} ${e.name};"), "\n")}
          |  ${(i.component.consumes.map(c => s"consumes ${c.typ} ${c.name};"), "\n")}
          |  ${(i.component.binarySemaphores.map(b => s"has binary_semaphore ${b.name};"), "\n")}
          |}
          """

    if(Util.isMonitor(name)) {
      add(s"${dirComponents}/${Util.MONITOR_DIRECTORY_NAME}/${name}/${name}.camkes", st)
    } else {
      add(s"${dirComponents}/${name}/${name}.camkes", st)
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
          |  ${(o.includes.map(i => s"include ${i};"), "\n")}
          |  ${(methods, "\n")}
          |};"""

    add(s"${dirInterfaces}/${o.name}.idl4", st)

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