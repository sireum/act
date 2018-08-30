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
    var provides : ISZ[ST] = ISZ()
    var uses: ISZ[ST] = ISZ()

    for(p <- i.component.provides) {
      provides = provides :+ st"""provides ${p.procedure} ${p.name};"""
    }

    for(p <- i.component.uses) {
      uses = uses :+ st"""uses ${p.procedure} ${p.name};"""
    }

    val st =
      st"""component ${name} {
          |  ${if(i.component.control) "control;" else ""}
          |  ${(provides, "\n")}
          |  ${(uses, "\n")}
          |}
          """

    add(s"${dirComponents}/${name}/${name}.camkes", st)

    return None()
  }

  def visitProcedure(o: Procedure): Option[ST] = {
    var methods: ISZ[ST] = ISZ()
    for(m <- o.methods) {
      methods = methods :+ visitMethod(m).get
    }

    val st =
      st"""procedure ${o.name} {
          |  ${(methods, "\n")}
          |};"""

    add(s"${dirInterfaces}/${o.name}.idl4", st)

    return None()
  }

  def visitMethod(o: Method) : Option[ST] = {
    var params: ISZ[ST] = ISZ()
    for(p<- o.parameters) {
      val dir: String = if(p.direction == Direction.In) "in" else "out"
      params = params :+ st"""${dir} ${p.typ} ${p.name}"""
    }

    val st = st"""void ${o.name}(${(params, ",")});"""
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