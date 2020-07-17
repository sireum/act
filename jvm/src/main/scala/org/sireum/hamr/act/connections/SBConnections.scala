// #Sireum

package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.{ActOptions, ActPlatform, Counter, Monitor, QueueObject, Sel4ConnectorTypes, SharedData, Util, ast}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.common.types.{TypeUtil => CommonTypeUtil}

@record class SBConnections(monitors: HashSMap[String, Monitor],
                          sharedData: HashMap[String, SharedData],
                          srcQueues: Map[String, Map[String, QueueObject]],
                          symbolTable: SymbolTable,
                          aadlTypes: AadlTypes,
                          actOptions: ActOptions,
                          reporter: Reporter) {

  val platform: ActPlatform.Type = actOptions.platform

  var camkesConfiguration: ISZ[ST] = ISZ()

  var connections: ISZ[ast.Connection] = ISZ()

  def processConnectionInstances(connectionCounter: Counter): (ISZ[ast.Connection], ISZ[ST]) = {
    assert(platform == ActPlatform.SeL4 || platform == ActPlatform.SeL4_Only)

    var map: HashSMap[ast.ConnectionEnd, ConnectionHolder] = HashSMap.empty

    def getConnectionHolder(connectionEnd: ast.ConnectionEnd, connectionType: Sel4ConnectorTypes.Type): ConnectionHolder = {
      if (!map.contains(connectionEnd)) {
        val connectionName = Util.getConnectionName(connectionCounter.increment())
        map = map + (connectionEnd ~> ConnectionHolder(connectionName, connectionType, MSZ(), MSZ()))
      }

      return map.get(connectionEnd).get
    }

    def updateHolder(end: ast.ConnectionEnd, holder: ConnectionHolder): Unit = {
      map = map + (end ~> holder)
    }

    for (entry <- symbolTable.outConnections.entries) {
      val srcFeaturePath = entry._1
      val connections = entry._2

      val handledConns: ISZ[ir.ConnectionInstance] = connections.filter(conn => Connections.isHandledConnection(conn, symbolTable))

      for (conn <- handledConns) {
        assert(CommonUtil.getName(conn.src.feature.get) == srcFeaturePath)

        val srcFeature: ir.Feature = symbolTable.getFeatureFromName(conn.src.feature.get)

        val dstFeaturePath: String = CommonUtil.getName(conn.dst.feature.get)
        val dstFeature: ir.Feature = symbolTable.getFeatureFromName(conn.dst.feature.get)

        val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
        val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

        val srcToVM: B = srcAadlThread.toVirtualMachine(symbolTable)
        val dstToVM: B = dstAadlThread.toVirtualMachine(symbolTable)

        val srcCamkesComponentId: String = Util.getThreadIdentifier(srcAadlThread, symbolTable)
        val dstCamkesComponentId: String = Util.getThreadIdentifier(dstAadlThread, symbolTable)

        conn.kind match {
          case ir.ConnectionKind.Port => {
            assert(srcFeature.category == dstFeature.category,
              s"Not currently handling mixed feature types: Source is ${srcFeature.category}, destination is ${dstFeature.category} for ${conn}")

            dstFeature.category match {
              case ir.FeatureCategory.DataPort => {

                { // dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcCamkesFeatureQueueName: String = Util.brand(CommonUtil.getLastName(srcFeature.identifier))
                  val dstCamkesFeatureQueueName: String = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

                  val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureQueueName)
                  val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureQueueName)

                  var holder = getConnectionHolder(srcConnectionEnd, queueConnectorType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

                  //holder.toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(aadlTypes) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    //holder.configurationEntries = holder.configurationEntries :+
                    //  st"""${holder.connectionName}.size = ${size};""".render
                    holder = holder(configurationEntries = holder.configurationEntries :+
                      st"""${holder.connectionName}.size = ${size};""".render)
                  }
                  updateHolder(srcConnectionEnd, holder)
                }
              }

              case ir.FeatureCategory.EventDataPort => {
                // sel4 notification plus shared queue

                val queueSize: Z = srcQueues.get(srcFeaturePath).get.get(dstFeaturePath).get.queueSize

                { // notification connections
                  val srcCamkesFeatureName = Util.genSeL4NotificationQueueName(srcFeature, queueSize)
                  val dstCamkesFeatureName = Util.genSeL4NotificationName(dstFeature, T)

                  val notificationConnectorType: Sel4ConnectorTypes.Type =
                    if (dstToVM) Sel4ConnectorTypes.seL4GlobalAsynch
                    else Sel4ConnectorTypes.seL4Notification

                  val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureName)
                  val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureName)

                  var holder = getConnectionHolder(srcConnectionEnd, notificationConnectorType)

                  //holder.toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd
                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

                  updateHolder(srcConnectionEnd, holder)
                }

                { // queue dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcCamkesFeatureQueueName: String = Util.getEventDataSBQueueSrcFeatureName(CommonUtil.getLastName(srcFeature.identifier), queueSize)
                  val dstCamkesFeatureQueueName: String = Util.getEventDataSBQueueDestFeatureName(CommonUtil.getLastName(dstFeature.identifier))

                  val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureQueueName)
                  val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureQueueName)

                  var holder = getConnectionHolder(srcConnectionEnd, queueConnectorType)

                  //holder.toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd
                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(aadlTypes) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    holder = holder(configurationEntries = holder.configurationEntries :+
                      st"""${holder.connectionName}.size = ${size};""".render)
                  }

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    st"""${srcCamkesComponentId}.${srcCamkesFeatureQueueName}_access = "W";""".render)

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    st"""${dstCamkesComponentId}.${dstCamkesFeatureQueueName}_access = "R";""".render)

                  updateHolder(srcConnectionEnd, holder)
                }
              }

              case ir.FeatureCategory.EventPort => {
                // sel4 notification plus shared counter

                { // notification connection
                  val srcCAmkESFeatureName = Util.brand(CommonUtil.getLastName(srcFeature.identifier))
                  val dstCAmkESFeatureName = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

                  val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCAmkESFeatureName)
                  val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCAmkESFeatureName)

                  val connectionType = Sel4ConnectorTypes.seL4Notification

                  var holder = getConnectionHolder(srcConnectionEnd, connectionType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

                  updateHolder(srcConnectionEnd, holder)
                }

                { // shared data connection for counter
                  val srcCamkesFeatureName = Util.getEventSBCounterName(CommonUtil.getLastName(srcFeature.identifier))
                  val dstCamkesFeatureName = Util.getEventSBCounterName(CommonUtil.getLastName(dstFeature.identifier))

                  val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureName)
                  val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureName)

                  val connectionType = Sel4ConnectorTypes.seL4SharedData

                  var holder = getConnectionHolder(srcConnectionEnd, connectionType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    st"""${srcCamkesComponentId}.${srcCamkesFeatureName}_access = "W";""".render)

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    st"""${dstCamkesComponentId}.${dstCamkesFeatureName}_access = "R";""".render)

                  updateHolder(srcConnectionEnd, holder)
                }
              }

              case x => halt(s"Not expecting Port connection involving ${x}")
            }
          }
          case ir.ConnectionKind.Access => {
            assert(F, s"Not handling access connections for ${platform} platform")
          }
          case x => halt(s"Not expecting connection type ${conn.kind}: ${conn}")
        }
      }
    }

    for (connectionHolderEntry <- map.entries) {
      val fromEnd = connectionHolderEntry._1
      val holder = connectionHolderEntry._2

      connections = connections :+ Util.createConnections(
        holder.connectionName,
        holder.connectionType,
        ISZ(fromEnd),
        holder.toConnectionEnds.toIS)

      val filtered = Set.empty[String] ++ holder.configurationEntries.toIS
      camkesConfiguration = camkesConfiguration ++ filtered.elements.map((m: String) => st"$m")
    }

    return (connections, camkesConfiguration)
  }

}