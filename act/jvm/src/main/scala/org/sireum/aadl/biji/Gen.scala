// #Sireum

package org.sireum.aadl.biji

import org.sireum._
import org.sireum.aadl.ir

import scala.meta.internal.semanticdb3.Scala.Descriptor

@record class Gen() {

  var componentMap: HashMap[String, ir.Component] = HashMap.empty
  var astObjects: ISZ[ASTObject] = ISZ()

  def gen(m : ir.Aadl) : ISZ[ASTObject] = {

    def r(c: ir.Component) : Unit = {
      val name = Util.getName(c.identifier)
      assert(!componentMap.contains(name))
      componentMap = componentMap + (name ~> c)
      c.subComponents.foreach(sc => r(sc))
    }
    m.components.foreach(c => r(c))

    m.components.foreach(c => gen(c))

    return astObjects
  }

  def gen(c: ir.Component) : Unit = {
    c.category match {
      case ir.ComponentCategory.System =>
        c.subComponents.foreach(sc => gen(sc))
      case ir.ComponentCategory.Process =>
        val g = genContainer(c)
        astObjects = astObjects :+ Assembly("", g)
      case ir.ComponentCategory.Thread =>
        genThread(c)
      case _ =>
        c.subComponents.foreach(sc => gen(sc))
    }
  }

  def genContainer(c : ir.Component) : Composition = {
    assert(c.category == ir.ComponentCategory.Process)

    var instances: ISZ[Instance] = ISZ()
    for(sc <- c.subComponents) {
      sc.category match {
        case ir.ComponentCategory.Thread =>
          val name = Util.getLastName(sc.identifier)
          instances = instances :+
            Instance(address_space =  "",
              name = name,
              component = genThread(sc))
        case ir.ComponentCategory.Subprogram =>
          var params: ISZ[Parameter] = ISZ()
          for(f <- sc.features) {
            val fend = f.asInstanceOf[ir.FeatureEnd]
            params = params :+ Parameter(array = F,
              direction = if(fend.direction == ir.Direction.In) Direction.In else Direction.Out,
              name = Util.getLastName(f.identifier),
              typ = Util.getClassifier(fend.classifier.get))
          }
          val method = Method(name = Util.getLastName(sc.identifier),
            parameters = params)

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(method))
        case _ =>
          halt(s"genContainer: Not handling: ${sc}")
      }
    }

    var connections: ISZ[Connection] = ISZ()
    for(conn <- c.connectionInstances) {
      val conn_name = Util.getLastName(conn.connectionRefs(0).name)
      val connector =
        Connector(
        from_hardware = F,
        from_multiple = F,
        from_threads = 0,
        from_type = "",
        name = "",
        to_hardware = F,
        to_multiple = F,
        to_threads = 0,
        to_type = ""
      )

      val from_ends: ISZ[ConnectionEnd] = ISZ(ConnectionEnd(
        isFrom = T,
        component = Util.getLastName(conn.src.component),
        end = Util.getLastName(conn.src.feature.get)))

      val to_ends: ISZ[ConnectionEnd] = ISZ(ConnectionEnd(
        isFrom = F,
        component = Util.getLastName(conn.dst.component),
        end = Util.getLastName(conn.dst.feature.get)))

      val con = Connection(name = conn_name,
        connector = connector,
        from_ends = from_ends,
        to_ends = to_ends
      )
      connections = connections :+ con
    }

    return Composition(
      groups = ISZ(),
      exports = ISZ(),
      instances = instances,
      connections = connections
    )
  }

  def genThread(c : ir.Component) : Component = {
    assert(c.category == ir.ComponentCategory.Thread)
    assert(c.subComponents.isEmpty)
    assert(c.connectionInstances.isEmpty)

    //
    // val id = Util.getLastName(c.identifier)
    val id = Util.getClassifier(c.classifier.get)
    val path = Util.getName(c.identifier)
    var provides: ISZ[Provides] = ISZ()
    var uses: ISZ[Uses] = ISZ()

    //c.properties
    //c.classifier

    for(f <- c.features){
      val fend = f.asInstanceOf[ir.FeatureEnd]
      val name = Util.getLastName(f.identifier)
      val proc = Util.getClassifier(fend.classifier.get)
      f.category match {
        case ir.FeatureCategory.SubprogramAccess =>
          val kind: Option[ir.ValueProp] = Util.getDiscreetPropertyValue[ir.ValueProp](f.properties, "AccessType")
          kind match {
            case Some(v) =>
              if(v.value == "requires") {
                uses = uses :+ Uses(name = name,
                  optional = F,
                  procedure = proc)
              } else if (v.value == "provides") {
                provides = provides :+ Provides(name = name,
                  procedure = proc)
              } else {
                halt(s"Unexpected: ${v}")
              }
            case _ =>
              halt("Unexpected")
          }
        case _ =>
          halt(s"Not handling ${f.category}")
      }
    }


    return Component(
      control = uses.nonEmpty,
      hardware = F,
      name = id,

      mutexes = ISZ(),
      binarySimaphores = ISZ(),
      semaphores = ISZ(),

      dataports = ISZ(),
      emits = ISZ(),
      uses = uses,
      consumes = ISZ(),
      provides = provides,
      includes = ISZ(),
      attributes = ISZ()
    )
  }
}
