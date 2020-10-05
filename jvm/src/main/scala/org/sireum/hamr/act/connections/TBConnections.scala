// #Sireum
package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.{ActOptions, ActPlatform, Counter, Ihor_Monitor, Monitor, QueueObject, Sel4ConnectorTypes, SharedData, TB_Monitor, Util, ast}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir
import org.sireum.message.Reporter

import org.sireum.hamr.codegen.common.types.{TypeUtil => CommonTypeUtil}

@record class TBConnections(monitors: HashSMap[String, Monitor],
                          sharedData: HashMap[String, SharedData],
                          srcQueues: Map[String, Map[String, QueueObject]],
                          symbolTable: SymbolTable,
                          aadlTypes: AadlTypes,
                          actOptions: ActOptions,
                          reporter: Reporter) {

  val platform: ActPlatform.Type = actOptions.platform

  var camkesConfiguration: ISZ[ST] = ISZ()

  var connections: ISZ[ast.Connection] = ISZ()

  def processConnections(c: ir.Component, connectionCounter: Counter): (ISZ[ast.Connection], ISZ[ST]) = {
    assert(platform == ActPlatform.SeL4_TB)

    val handledConns = c.connectionInstances.filter(conn => Connections.isHandledConnection(conn, symbolTable))
    val unhandledConns = c.connectionInstances.filter(conn => Connections.isHandledConnection(conn, symbolTable))

    val missingFeatures = sharedData.values.filter((f: SharedData) => f.ownerFeature.isEmpty)
    if(missingFeatures.nonEmpty) {
      reporter.error(None(), Util.toolName, s"Could not find the owner for the following data subcomponents: ${(missingFeatures.map((f: SharedData) => f.subcomponentId), ", ")}")
    }

    for(conn <- handledConns) {
      val dstPath: String = CommonUtil.getName(conn.dst.feature.get)
      val fdst: ir.Feature = symbolTable.airFeatureMap.get(dstPath).get

      val dstFeatureName: String = CommonUtil.getLastName(conn.dst.feature.get)

      conn.kind match {
        case ir.ConnectionKind.Port => {
          fdst.category match {

            case ir.FeatureCategory.DataPort => {
              connections = connections ++ createDataConnection(connectionCounter, conn)
            }
            case ir.FeatureCategory.EventDataPort => {
              connections = connections ++ createDataConnection(connectionCounter, conn)
            }
            case ir.FeatureCategory.EventPort => {
              // monitor
              connections = connections ++ createDataConnection_Ihor(connectionCounter, conn)
            }
            case _ => halt(s"not expecting ${fdst.category}")
          }
        }
        case ir.ConnectionKind.Access => {
          def accessComponentId(name: ir.Name): String = {
            return st"${(ops.ISZOps(name.name).tail, "_")}".render
          }

          val srcComponentId: String = accessComponentId(conn.src.component)
          val dstComponentId: String = accessComponentId(conn.dst.component)

          fdst.category match {
            case ir.FeatureCategory.SubprogramAccess =>
              val srcFeature = CommonUtil.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(
                connectionCounter = connectionCounter,
                srcComponent = srcComponentId,
                srcFeature = srcFeature,
                dstComponent = dstComponentId,
                dstFeature = dstFeatureName)
            case ir.FeatureCategory.SubprogramAccessGroup =>
              val srcFeature = CommonUtil.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(
                connectionCounter = connectionCounter,
                srcComponent = srcComponentId,
                srcFeature = srcFeature,
                dstComponent = dstComponentId,
                dstFeature = dstFeatureName)

            case ir.FeatureCategory.DataAccess =>
              val sd = sharedData.get(CommonUtil.getName(conn.src.component)).get
              val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.dst.component)).get

              val ownerId = accessComponentId(sd.owner.identifier)
              val dstId = accessComponentId(dstComp.identifier)

              if (ownerId != dstId) {
                connections = connections :+ Connections.createConnection(
                  connectionCounter = connectionCounter,
                  connectionType = Sel4ConnectorTypes.seL4SharedData,
                  srcComponent = dstComponentId,
                  srcFeature = dstFeatureName,
                  dstComponent = accessComponentId(sd.owner.identifier),
                  dstFeature = CommonUtil.getLastName(sd.ownerFeature.get.identifier)
                )
              } else {
                // Ignore connection to the owner component
              }
            case _ => halt(s"not expecting ${fdst.category}")
          }
        }

        case _ => halt(s"not expecting ${conn.kind}")
      }
    }

    return (connections, camkesConfiguration)
  }



  def createSeL4GlobalAsynchConnection(connectionCounter: Counter,
                                       srcComponent: String, srcFeature: String,
                                       dstComponent: String, dstFeature: String): ast.Connection = {
    return Connections.createConnection(
      connectionCounter,
      Sel4ConnectorTypes.seL4GlobalAsynch,
      srcComponent, srcFeature,
      dstComponent, dstFeature)
  }

  def createSeL4NotificationConnection(connectionCounter: Counter,
                                       srcComponent: String, srcFeature: String,
                                       dstComponent: String, dstFeature: String): ast.Connection = {
    return Connections.createConnection(
      connectionCounter,
      Sel4ConnectorTypes.seL4Notification,
      srcComponent, srcFeature,
      dstComponent, dstFeature)
  }

  def createRPCConnection(connectionCounter: Counter,
                          srcComponent: String, srcFeature: String,
                          dstComponent: String, dstFeature: String) : ast.Connection = {
    return Connections.createConnection(
      connectionCounter,
      Sel4ConnectorTypes.seL4RPCCall,
      srcComponent, srcFeature,
      dstComponent, dstFeature)
  }

  def createSharedDataCounterConnection(connectionCounter: Counter,
                                        conn: ir.ConnectionInstance) : ast.Connection = {
    val srcComponent = CommonUtil.getLastName(conn.src.component)
    val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get

    val dstComponent = CommonUtil.getLastName(conn.dst.component)
    val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

    val srcFeatureName = Util.getEventSBCounterName(CommonUtil.getLastName(srcFeature.identifier))
    val dstFeatureName = Util.getEventSBCounterName(CommonUtil.getLastName(dstFeature.identifier))

    camkesConfiguration = camkesConfiguration :+ st"""${srcComponent}.${srcFeatureName}_access = "W";"""
    camkesConfiguration = camkesConfiguration :+ st"""${dstComponent}.${dstFeatureName}_access = "R";"""

    return Connections.createConnection(
      connectionCounter,
      Sel4ConnectorTypes.seL4SharedData,
      srcComponent, srcFeatureName,
      dstComponent, dstFeatureName
    )
  }

  def createSharedDataConnection(connectionCounter: Counter,
                                 conn: ir.ConnectionInstance) : ast.Connection = {
    val srcComponent = CommonUtil.getLastName(conn.src.component)
    val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get

    val dstComponent = CommonUtil.getLastName(conn.dst.component)
    val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

    val srcFeatureName = Util.brand(CommonUtil.getLastName(srcFeature.identifier))
    val dstFeatureName = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

    return Connections.createConnection(
      connectionCounter,
      Sel4ConnectorTypes.seL4SharedData,
      srcComponent, srcFeatureName,
      dstComponent, dstFeatureName
    )
  }

  def createDataConnection(connectionCounter: Counter,
                           conn: ir.ConnectionInstance) : ISZ[ast.Connection] = {
    val monitor = Monitors.getMonitorForConnectionInstance(conn, monitors).get.asInstanceOf[TB_Monitor]

    val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
    val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

    val srcCamkesComponentId: String = Util.getCamkesComponentIdentifier(srcAadlThread, symbolTable)
    val dstCamkesComponentId: String = Util.getCamkesComponentIdentifier(dstAadlThread, symbolTable)

    val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get
    val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

    val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
    val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

    var ret: ISZ[ast.Connection] = ISZ()

    // rpc src to mon
    ret = ret :+ createRPCConnection(
      connectionCounter = connectionCounter,
      srcComponent = srcCamkesComponentId,
      srcFeature = srcFeatureName,
      dstComponent = monitor.i.name,
      dstFeature = monitor.providesVarName)

    // rpc mon to dst
    ret = ret :+ createRPCConnection(
      connectionCounter = connectionCounter,
      srcComponent = dstCamkesComponentId,
      srcFeature = dstFeatureName,
      dstComponent = monitor.i.name,
      dstFeature = monitor.providesVarName)

    // notification monsig to dst
    return ret :+ createSeL4NotificationConnection(
      connectionCounter = connectionCounter,
      srcComponent = monitor.i.name,
      srcFeature = "monsig",
      dstComponent = dstCamkesComponentId,
      dstFeature = Util.genSeL4NotificationName(dstFeature, T)
    )
  }

  def createDataConnection_Ihor(connectionCounter: Counter,
                                conn: ir.ConnectionInstance) :ISZ[ast.Connection] = {
    val monitor = Monitors.getMonitorForConnectionInstance(conn, monitors).get.asInstanceOf[Ihor_Monitor]

    val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
    val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

    val srcCamkesComponentId: String = Util.getCamkesComponentIdentifier(srcAadlThread, symbolTable)
    val dstCamkesComponentId: String = Util.getCamkesComponentIdentifier(dstAadlThread, symbolTable)

    val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get
    val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

    val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
    val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

    var ret: ISZ[ast.Connection] = ISZ()

    // rpc src to mon
    ret = ret :+ createRPCConnection(
      connectionCounter = connectionCounter,
      srcComponent = srcCamkesComponentId,
      srcFeature = srcFeatureName,
      dstComponent = monitor.i.name,
      dstFeature = monitor.providesSenderVarName)

    // rpc mon to dst
    ret = ret :+ createRPCConnection(
      connectionCounter = connectionCounter,
      srcComponent = dstCamkesComponentId,
      srcFeature = dstFeatureName,
      dstComponent = monitor.i.name,
      dstFeature = monitor.providesReceiverVarName)

    // notification monsig to dst
    return ret :+ createSeL4NotificationConnection(
      connectionCounter = connectionCounter,
      srcComponent = monitor.i.name,
      srcFeature = "monsig",
      dstComponent = dstCamkesComponentId,
      dstFeature = Util.genSeL4NotificationName(dstFeature, T)
    )
  }
}
