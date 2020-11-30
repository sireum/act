// #Sireum

package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, SymbolTable}
import org.sireum.hamr.ir

object SharedDataUtil {

  def buildSharedData(symbolTable: SymbolTable): HashMap[String, SharedData] = {
    var sharedData: HashMap[String, SharedData] = HashMap.empty

    for(aadlProcess <- symbolTable.getProcesses()) {
      val dataComponents: ISZ[AadlComponent] = aadlProcess.subComponents.filter(p => p.component.category == ir.ComponentCategory.Data)

      for(dc <- dataComponents) {
        val sc = dc.component
        Util.getCamkesOwnerThread(sc.properties) match {
          case Some(owner) =>
            val threads = symbolTable.airComponentMap.values.filter(f => f.category == ir.ComponentCategory.Thread)
            val thread = threads.filter(f => f.classifier.get.name == owner || Util.getClassifier(f.classifier.get) == owner)

            if (thread.nonEmpty) {
              val theOwner = thread(0)
              if (thread.size > 1) {
                reporter.warn(None(), Util.toolName, s"Found multiple matches for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property: ${owner}")
              }
              if (Util.getClassifier(theOwner.classifier.get) == owner) {
                reporter.warn(None(), Util.toolName, s"Fully qualified name '${theOwner.classifier.get.name}' should be used for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property")
              }

              val subcomponentId = st"${(ops.ISZOps(sc.identifier.name).tail, "_")}".render

              sharedData = sharedData +
                (CommonUtil.getName(sc.identifier) ~> SharedData(theOwner, None[ir.FeatureAccess](), sc.classifier.get, subcomponentId))

            } else {
              reporter.error(None(), Util.toolName,
                st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread}:  Could not locate component '${owner}'.  Please use one of the following:
                    |  ${(threads.map((m: ir.Component) => m.classifier.get.name), "\n")}""".render)
            }
          case _ =>
            reporter.error(None(), Util.toolName, st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property missing""".render)
        }
      }
    }

    val handledConnections: ISZ[ir.ConnectionInstance] = symbolTable.outConnections.values.flatMap(f =>
      f.filter(p => Connections.isHandledConnection(p, symbolTable)))

    val dataConnections = handledConnections.filter(f => f.kind == ir.ConnectionKind.Access).filter(
      f => symbolTable.airFeatureMap.get(CommonUtil.getName(f.dst.feature.get)).get.category == ir.FeatureCategory.DataAccess)

    for(conn <- dataConnections) {
      val srcComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.src.component)).get

      if(srcComp.category != ir.ComponentCategory.Data) {
        reporter.error(None(), Util.toolName, s"${CommonUtil.getLastName(conn.src.component)} is not a data component")
      }

      val (dataKey, dstFeatureName) = Connections.getPortConnectionNames(conn, symbolTable)

      sharedData.get(dataKey) match {
        case Some(sd) =>
          val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.dst.component)).get
          val ownerId = CommonUtil.getName(sd.owner.identifier)
          val dstId = CommonUtil.getName(dstComp.identifier)

          if(ownerId == dstId) {
            val _f = dstComp.features.filter(f => CommonUtil.getName(f.identifier) == CommonUtil.getName(conn.dst.feature.get))
            if(_f.size != 1) {
              reporter.error(None(), Util.toolName, s"There are ${_f.size} matching features for ${CommonUtil.getName(conn.dst.feature.get)}, expecting only 1.")
            } else if(!_f(0).isInstanceOf[ir.FeatureAccess]) {
              reporter.error(None(), Util.toolName, s"${CommonUtil.getName(conn.dst.feature.get)} is not a FeatureAccess.")
            } else {
              // add the owner's feature
              sharedData = sharedData + (dataKey ~> SharedData(sd.owner, Some(_f(0).asInstanceOf[ir.FeatureAccess]), sd.typ, sd.subcomponentId))
            }
          }
        case _ =>
          reporter.error(None(), Util.toolName, s"Could not find data subcomponent: ${dataKey}")
      }
    }

    return sharedData
  }

}
