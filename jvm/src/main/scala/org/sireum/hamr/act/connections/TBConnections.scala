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

      val srcComponentName: String = CommonUtil.getLastName(conn.src.component)

      val dstComponentName: String = CommonUtil.getLastName(conn.dst.component)
      val dstFeatureName: String = CommonUtil.getLastName(conn.dst.feature.get)

      conn.kind match {
        case ir.ConnectionKind.Port => {
          fdst.category match {

            case ir.FeatureCategory.DataPort => {

              def handleDataPort_TB_Connection(): Unit = {
                connections = connections ++ createDataConnection(connectionCounter, conn)
              }

              def handleDataPort_SB_Connection(): Unit = {
                val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
                val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

                val srcToVM = srcAadlThread.toVirtualMachine(symbolTable)
                val dstToVM = dstAadlThread.toVirtualMachine(symbolTable)

                val srcComponentId: String = Util.getThreadIdentifier(srcAadlThread, symbolTable)
                val dstComponentId: String = Util.getThreadIdentifier(dstAadlThread, symbolTable)

                val srcFeature: ir.Feature = symbolTable.getFeatureFromName(conn.src.feature.get)
                val dstFeature: ir.Feature = symbolTable.getFeatureFromName(conn.dst.feature.get)

                { // dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcFeatureQueueName: String = Util.brand(CommonUtil.getLastName(srcFeature.identifier))

                  val dstFeatureQueueName: String = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

                  connections = connections :+ Connections.createConnection(
                    connectionCounter,
                    queueConnectorType,
                    srcComponentId, srcFeatureQueueName,
                    dstComponentId, dstFeatureQueueName
                  )

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(aadlTypes) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    val connectionName = Util.getConnectionName(connectionCounter.count)
                    camkesConfiguration = camkesConfiguration :+
                      st"""${connectionName}.size = ${size};"""
                  }
                }
              }

              platform match {
                case ActPlatform.SeL4_TB => handleDataPort_TB_Connection()
                case ActPlatform.SeL4_Only => handleDataPort_SB_Connection()
                case ActPlatform.SeL4 => handleDataPort_SB_Connection()
              }
            }

            case ir.FeatureCategory.EventDataPort => {

              def eventDataPort_SB_Connection_Profile(): Unit = {
                // sel4 notification plus shared queue

                val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
                val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

                val srcToVM = srcAadlThread.toVirtualMachine(symbolTable)
                val dstToVM = dstAadlThread.toVirtualMachine(symbolTable)

                val srcCamkesComponentId: String = Util.getThreadIdentifier(srcAadlThread, symbolTable)
                val dstCamkesComponentId: String = Util.getThreadIdentifier(dstAadlThread, symbolTable)

                val srcPath: String = CommonUtil.getName(conn.src.feature.get)

                val srcFeature: ir.Feature = symbolTable.getFeatureFromName(conn.src.feature.get)
                val dstFeature: ir.Feature = symbolTable.getFeatureFromName(conn.dst.feature.get)

                val queueSize: Z = srcQueues.get(srcPath).get.get(dstPath).get.queueSize

                { // Notification connection
                  val srcCamkesFeatureName = Util.genSeL4NotificationQueueName(srcFeature, queueSize)
                  val dstCamkesFeatureName = Util.genSeL4NotificationName(fdst, T)

                  val notificationConnectorType: Sel4ConnectorTypes.Type =
                    if (dstToVM) Sel4ConnectorTypes.seL4GlobalAsynch
                    else Sel4ConnectorTypes.seL4Notification

                  connections = connections :+ Connections.createConnection(
                    connectionCounter,
                    notificationConnectorType,
                    srcCamkesComponentId, srcCamkesFeatureName,
                    dstCamkesComponentId, dstCamkesFeatureName)
                }

                { // Queue dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcCamkesFeatureQueueName: String = Util.getEventDataSBQueueSrcFeatureName(CommonUtil.getLastName(srcFeature.identifier), queueSize)

                  val dstCamkesFeatureQueueName: String = Util.getEventDataSBQueueDestFeatureName(CommonUtil.getLastName(dstFeature.identifier))

                  connections = connections :+ Connections.createConnection(
                    connectionCounter,
                    queueConnectorType,
                    srcCamkesComponentId, srcCamkesFeatureQueueName,
                    dstCamkesComponentId, dstCamkesFeatureQueueName
                  )

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(aadlTypes) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    val connectionName = Util.getConnectionName(connectionCounter.count)
                    camkesConfiguration = camkesConfiguration :+
                      st"""${connectionName}.size = ${size};"""
                  }

                  camkesConfiguration = camkesConfiguration :+ st"""${srcCamkesComponentId}.${srcCamkesFeatureQueueName}_access = "W";"""
                  camkesConfiguration = camkesConfiguration :+ st"""${dstCamkesComponentId}.${dstCamkesFeatureQueueName}_access = "R";"""
                }
              }

              platform match {
                case ActPlatform.SeL4_TB => connections = connections ++ createDataConnection(connectionCounter, conn)
                case ActPlatform.SeL4 => eventDataPort_SB_Connection_Profile()
                case ActPlatform.SeL4_Only => eventDataPort_SB_Connection_Profile()
              }
            }

            case ir.FeatureCategory.EventPort => {

              def eventPort_TB_Connection_Profile(): Unit = {
                // monitor
                connections = connections ++ createDataConnection_Ihor(connectionCounter, conn)
              }

              def eventPort_SB_Connection_Profile(): Unit = {
                // sel4 notification plus shared counter
                val srcFeature = CommonUtil.getLastName(conn.src.feature.get)

                connections = connections :+ createSeL4NotificationConnection(
                  connectionCounter,
                  srcComponentName, Util.brand(srcFeature),
                  dstComponentName, Util.brand(dstFeatureName))

                connections = connections :+ createSharedDataCounterConnection(connectionCounter, conn)
              }

              platform match {
                case ActPlatform.SeL4_TB => eventPort_TB_Connection_Profile()
                case ActPlatform.SeL4 => eventPort_SB_Connection_Profile()
                case ActPlatform.SeL4_Only => eventPort_SB_Connection_Profile()
              }
            }

            case _ => halt(s"not expecting ${fdst.category}")
          }
        }
        case ir.ConnectionKind.Access => {

          fdst.category match {
            case ir.FeatureCategory.SubprogramAccess =>
              val srcFeature = CommonUtil.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(
                connectionCounter, srcComponentName, srcFeature, dstComponentName, dstFeatureName)
            case ir.FeatureCategory.SubprogramAccessGroup =>
              val srcFeature = CommonUtil.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(
                connectionCounter, srcComponentName, srcFeature, dstComponentName, dstFeatureName)

            case ir.FeatureCategory.DataAccess =>
              val sd = sharedData.get(CommonUtil.getName(conn.src.component)).get
              val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.dst.component)).get
              val ownerId = CommonUtil.getName(sd.owner.identifier)
              val dstId = CommonUtil.getName(dstComp.identifier)

              if (ownerId != dstId) {
                connections = connections :+ Connections.createConnection(
                  connectionCounter,
                  Sel4ConnectorTypes.seL4SharedData,
                  dstComponentName, dstFeatureName,
                  CommonUtil.getLastName(sd.owner.identifier), CommonUtil.getLastName(sd.ownerFeature.get.identifier)
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

    val srcComponent = CommonUtil.getLastName(conn.src.component)
    val dstComponent = CommonUtil.getLastName(conn.dst.component)

    val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get
    val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

    val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
    val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

    var ret: ISZ[ast.Connection] = ISZ()

    // rpc src to mon
    ret = ret :+ createRPCConnection(
      connectionCounter, srcComponent, srcFeatureName, monitor.i.name, monitor.providesVarName)

    // rpc mon to dst
    ret = ret :+ createRPCConnection(
      connectionCounter, dstComponent, dstFeatureName, monitor.i.name, monitor.providesVarName)

    // notification monsig to dst
    return ret :+ createSeL4NotificationConnection(
      connectionCounter,
      monitor.i.name, "monsig",
      dstComponent, Util.genSeL4NotificationName(dstFeature, T)
    )
  }

  def createDataConnection_Ihor(connectionCounter: Counter,
                                conn: ir.ConnectionInstance) :ISZ[ast.Connection] = {
    val monitor = Monitors.getMonitorForConnectionInstance(conn, monitors).get.asInstanceOf[Ihor_Monitor]

    val srcComponent = CommonUtil.getLastName(conn.src.component)
    val dstComponent = CommonUtil.getLastName(conn.dst.component)

    val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get
    val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

    val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
    val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

    var ret: ISZ[ast.Connection] = ISZ()

    // rpc src to mon
    ret = ret :+ createRPCConnection(
      connectionCounter, srcComponent, srcFeatureName, monitor.i.name, monitor.providesSenderVarName)

    // rpc mon to dst
    ret = ret :+ createRPCConnection(
      connectionCounter, dstComponent, dstFeatureName, monitor.i.name, monitor.providesReceiverVarName)

    // notification monsig to dst
    return ret :+ createSeL4NotificationConnection(
      connectionCounter,
      monitor.i.name, "monsig",
      dstComponent, Util.genSeL4NotificationName(dstFeature, T)
    )
  }
}
