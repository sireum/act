// #Sireum

package org.sireum.hamr.act.dot

import org.sireum._
import org.sireum.hamr.act.ast._

@enum object EntityType{
  'dataport
  
  'emits
  'consumes
  
  'uses
  'provides
}
  
@enum object ConnectionType {
  'seL4Notification
  'seL4RPCCall
  'seL4SharedData
  
  'seL4TimeServer
  'seL4GlobalAsynchCallback

  'seL4VMDTBPassthrough
  'seL4SharedDataWithCaps
}

object HTMLDotGenerator {
  val red: String = "red"
  val blue: String = "blue"
  val orange: String = "orange"
  val green: String = "green"
  val brown: String = "brown"
  val yellow: String = "yellow"
  val pink: String = "pink"
  val unknown: String = "grey"
  
  val entityMap: Map[EntityType.Type, String] = Map.empty[EntityType.Type, String](ISZ(
    (EntityType.dataport, red),

    (EntityType.emits, blue),
    (EntityType.consumes, blue),

    (EntityType.provides, orange),
    (EntityType.uses, orange)
  ))
  
  val connMap: Map[ConnectionType.Type, String] = Map.empty[ConnectionType.Type, String](ISZ(
    (ConnectionType.seL4SharedData, red),

    (ConnectionType.seL4Notification, blue),

    (ConnectionType.seL4RPCCall, orange),

    (ConnectionType.seL4TimeServer, green),

    (ConnectionType.seL4GlobalAsynchCallback, brown),

    (ConnectionType.seL4VMDTBPassthrough, yellow),

    (ConnectionType.seL4SharedDataWithCaps, pink)
  ))

  def wrapColor(str: String, color: String): ST = {
    return st"""<FONT COLOR="${color}">${str}</FONT>"""
  }

  def dotty(model: Assembly, simpleDot: B): ST = {

    var nodes: ISZ[ST] = model.composition.instances.map(i => {
      val name = i.name
      val comp = i.component
      
      var portNames: ISZ[ST] = ISZ()

      def row(id:String, typ: String, entityTyp: EntityType.Type): ST = {
        val color = entityMap.getOrElse(entityTyp, unknown)
        val txt: String =
          if(simpleDot) s"<B>${id}</B>"
          else s"${entityTyp} ${typ} <B>${id}</B>"

        val c1 = wrapColor(txt, color)
        return st"""<TR><TD PORT="${id}">${c1}</TD></TR>"""
      }

      portNames = portNames :+ st"<TR><TD><B>${name}</B></TD></TR>"

      comp match {
        case c: Component =>
          portNames = portNames ++ c.dataports.map((d : Dataport) => row(d.name, d.typ, EntityType.dataport))
          portNames = portNames ++ c.emits.map((d : Emits) => row(d.name, d.typ, EntityType.emits))
          portNames = portNames ++ c.uses.map((d : Uses) => row(d.name, d.typ, EntityType.uses))
          portNames = portNames ++ c.consumes.map((d : Consumes) => row(d.name, d.typ, EntityType.consumes))
          portNames = portNames ++ c.provides.map((d : Provides) => row(d.name, d.typ, EntityType.provides))
        case _ =>
      }

      st"""${name} [
           |  label=<
           |    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
           |      ${(portNames, "\n")}"
           |    </TABLE>
           |  >
           |  shape=plaintext
           |];"""
    })
    
    val conns: ISZ[ST] = model.composition.connections.flatMap(conn => {
      var accum: ISZ[ST] = ISZ()

      var fromIndex = z"0"
      for(from <- conn.from_ends) {
        var toIndex = z"0"
        for(to <- conn.to_ends) {

          val color: String = ConnectionType.byName(conn.connectionType) match {
            case Some(connType) => connMap.getOrElse(connType, unknown)
            case _ => unknown
          }

          accum = accum :+
          st""""${from.component}":${from.end} -> "${to.component}":${to.end} [
              |  color="${color}"
              |  //label = "${conn.connectionType}"
              |  id = ${conn.name}

              |];"""

          toIndex = toIndex + 1
        }
        fromIndex = fromIndex + 1
      }
      accum
    })

    val connTypeRows: ISZ[ST] = connMap.entries.map(e => {
      val c1 = wrapColor(e._1.name, e._2)
      st"""<TR><TD>${c1}</TD><TD BGCOLOR="${e._2}">${e._2}</TD></TR>"""
    })

    nodes = nodes :+ st"""connectiontypekey [
                         |  label=<
                         |   <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
                         |     <TR><TD COLSPAN="2"><B>Key: Connection Types</B></TD></TR>
                         |     ${(connTypeRows, "\n")}
                         |   </TABLE>
                         |  >
                         |  shape=plaintext
                         |];"""

    val interfaceRows: ISZ[ST] = entityMap.entries.map(e => {
      val c1 = wrapColor(e._1.name, e._2)
      st"""<TR><TD>${c1}</TD><TD BGCOLOR="${e._2}">${e._2}</TD></TR>"""
    })
    
    nodes = nodes :+ st"""interfacetypekey [
                         |  label=<
                         |   <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
                         |     <TR><TD COLSPAN="2"><B>Key: Interface Types</B></TD></TR>
                         |     ${(interfaceRows, "\n")}
                         |   </TABLE>
                         |  >
                         |  shape=plaintext
                         |];"""
    
    var st: ST = st"""digraph g {
                     |graph [
                     |  overlap = false,
                     |  rankdir = "LR"
                     |];
                     |node [
                     |  fontsize = "16",
                     |  shape = "ellipse"
                     |];
                     |edge [
                     |];
                     |
                     |${(nodes, "\n\n")}
                     |
                     |${(conns, "\n\n")}
                     |
                     |}"""
    return st
  }
}
