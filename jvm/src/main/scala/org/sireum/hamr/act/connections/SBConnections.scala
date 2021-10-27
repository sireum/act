// #Sireum

package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.ast.{Attribute, ConnectionEnd, ConnectorType}
import org.sireum.hamr.act.proof.ProofContainer.CAmkESConnectionType
import org.sireum.hamr.act.templates.ConnectionsSbTemplate
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.ir

@datatype class VMConnectionInfo(aadlThread: AadlThread,
                                 aadlThreadPort: AadlPort)

@datatype class SBConnectionContainer(connectionOrigin: ir.ConnectionInstance,

                                      srcComponent: AadlComponent,
                                      srcPort: AadlPort,

                                      dstComponent: AadlComponent,
                                      dstPort: AadlPort,

                                      srcVMInfo: Option[VMConnectionInfo],
                                      dstVMInfo: Option[VMConnectionInfo]
                                     )

object SBConnections {

  def preprocessConnectionInstances(symbolTable: SymbolTable): ISZ[SBConnectionContainer] = {
    var conns: ISZ[SBConnectionContainer] = ISZ()

    for (connections <- symbolTable.outConnections.values) {

      val handledConns: ISZ[ir.ConnectionInstance] = connections.filter(conn =>
        Connections.isHandledConnection(conn, symbolTable))

      for (conn <- handledConns) {
        val srcThread = symbolTable.componentMap.get(CommonUtil.getName(conn.src.component)).get.asInstanceOf[AadlThread]
        val srcProcess = srcThread.getParent(symbolTable)

        val dstThread = symbolTable.componentMap.get(CommonUtil.getName(conn.dst.component)).get.asInstanceOf[AadlThread]
        val dstProcess = dstThread.getParent(symbolTable)

        val srcThreadPort = symbolTable.featureMap.get(CommonUtil.getName(conn.src.feature.get)).get.asInstanceOf[AadlPort]
        val dstThreadPort = symbolTable.featureMap.get(CommonUtil.getName(conn.dst.feature.get)).get.asInstanceOf[AadlPort]

        val (srcComponent, srcPort, srcVMInfo): (AadlComponent, AadlPort, Option[VMConnectionInfo]) = {
          if(srcProcess.toVirtualMachine(symbolTable)) {
            val connRef: ir.ConnectionReference = conn.connectionRefs(0)
            val connection: ir.Connection = symbolTable.aadlMaps.connectionsMap.get(connRef.name.name).get
            val featureName = CommonUtil.getName(connection.dst(0).feature.get)
            val srcProcessPort = symbolTable.featureMap.get(featureName).get.asInstanceOf[AadlPort]

            (srcProcess, srcProcessPort, Some(VMConnectionInfo(srcThread, srcThreadPort)))
          } else {
            (srcThread, srcThreadPort, None())
          }
        }

        val (dstComponent, dstPort, dstVMInfo): (AadlComponent, AadlPort, Option[VMConnectionInfo]) = {
          if(dstProcess.toVirtualMachine(symbolTable)) {
            val connRef: ir.ConnectionReference = ops.ISZOps(conn.connectionRefs).last
            val connection: ir.Connection = symbolTable.aadlMaps.connectionsMap.get(connRef.name.name).get
            val featureName = CommonUtil.getName(connection.src(0).feature.get)
            val dstProcessPort = symbolTable.featureMap.get(featureName).get.asInstanceOf[AadlPort]

            (dstProcess, dstProcessPort, Some(VMConnectionInfo(dstThread, dstThreadPort)))
          } else {
            (dstThread, dstThreadPort, None())
          }
        }

        val connconn: SBConnectionContainer = SBConnectionContainer(connectionOrigin = conn,
          srcComponent = srcComponent,
          srcPort = srcPort,
          dstComponent = dstComponent,
          dstPort = dstPort,
          srcVMInfo = srcVMInfo,
          dstVMInfo = dstVMInfo)

        conns = conns :+ connconn
      }
    }
    return conns
  }
}

@record class SBConnections(monitors: HashSMap[String, Monitor],
                          sharedData: HashMap[String, SharedData],
                          srcQueues: Map[String, Map[String, QueueObject]],
                          symbolTable: SymbolTable,
                          aadlTypes: AadlTypes,
                          actOptions: ActOptions) {

  val platform: ActPlatform.Type = actOptions.platform

  var configurationEntries: ISZ[ast.Configuration] = ISZ()

  var connections: ISZ[ast.Connection] = ISZ()

  val useCaseConnections: B = Connections.useCaseEventDataPortConnector(actOptions.experimentalOptions)

  def processConnectionInstances(connectionCounter: Counter): ConnectionContainer = {
    assert(platform == ActPlatform.SeL4 || platform == ActPlatform.SeL4_Only)

    var map: HashSMap[ast.ConnectionEnd, ConnectionHolder] = HashSMap.empty

    def getConnectionHolder(connectionEnd: ast.ConnectionEnd, connectionType: Sel4ConnectorTypes.Type): ConnectionHolder = {
      if (!map.contains(connectionEnd)) {
        val connectionName = Util.getConnectionName(connectionCounter.increment())
        map = map + (connectionEnd ~> ConnectionHolder(connectionName, connectionType, ISZ(), ISZ()))
      }

      return map.get(connectionEnd).get
    }

    def updateHolder(end: ast.ConnectionEnd, holder: ConnectionHolder): Unit = {
      map = map + (end ~> holder)
    }

    for (entry <- symbolTable.outConnections.entries) {
      val srcFeaturePath = entry._1
      val connections = entry._2

      val handledConns: ISZ[ir.ConnectionInstance] = connections.filter(conn =>
        Connections.isHandledConnection(conn, symbolTable))

      for (conn <- handledConns) {
        assert(CommonUtil.getName(conn.src.feature.get) == srcFeaturePath)

        val srcFeature: ir.Feature = symbolTable.getFeatureFromName(conn.src.feature.get)

        val dstFeaturePath: String = CommonUtil.getName(conn.dst.feature.get)
        val dstFeature: ir.Feature = symbolTable.getFeatureFromName(conn.dst.feature.get)

        val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
        val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

        val srcToVM: B = srcAadlThread.toVirtualMachine(symbolTable)
        val dstToVM: B = dstAadlThread.toVirtualMachine(symbolTable)

        val srcCamkesComponentId: String = Util.getCamkesComponentIdentifier(srcAadlThread, symbolTable)
        val dstCamkesComponentId: String = Util.getCamkesComponentIdentifier(dstAadlThread, symbolTable)

        conn.kind match {
          case ir.ConnectionKind.Port => {
            assert(srcFeature.category == dstFeature.category,
              s"Not currently handling mixed feature types: Source is ${srcFeature.category}, destination is ${dstFeature.category} for ${conn}")

            dstFeature.category match {
              case ir.FeatureCategory.DataPort => {
                // dataport connection

                val srcCamkesFeatureQueueName_DP: String = Util.brand(CommonUtil.getLastName(srcFeature.identifier))
                val dstCamkesFeatureQueueName_DP: String = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

                val srcConnectionEnd_DP = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureQueueName_DP)
                val dstConnectionEnd_DP = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureQueueName_DP)

                if(useCaseConnections) {
                  val queueConnectorType = Sel4ConnectorTypes.CASE_AADL_EventDataport

                  var holder = getConnectionHolder(srcConnectionEnd_DP, queueConnectorType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd_DP)

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(symbolTable) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    holder = holder(configurationEntries = holder.configurationEntries :+
                      ast.GenericConfiguration(s"${holder.connectionName}.size = ${size};"))
                  }

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ConnectionsSbTemplate.caseConnectorConfig_with_signalling(holder.connectionName, F))

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ConnectionsSbTemplate.caseConnectorConfig_connection_type(srcCamkesComponentId, srcCamkesFeatureQueueName_DP, srcToVM))

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ConnectionsSbTemplate.caseConnectorConfig_connection_type(dstCamkesComponentId, dstCamkesFeatureQueueName_DP, dstToVM))

                  updateHolder(srcConnectionEnd_DP, holder)

                }
                else {
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  var holder = getConnectionHolder(srcConnectionEnd_DP, queueConnectorType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd_DP)

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(symbolTable) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    holder = holder(configurationEntries = holder.configurationEntries :+
                      ast.GenericConfiguration(s"${holder.connectionName}.size = ${size};"))
                  }

                  val producerPerms: ast.AccessType.Type = if(srcToVM) ast.AccessType.RW else ast.AccessType.W

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ast.DataPortAccessRestriction(srcCamkesComponentId, srcCamkesFeatureQueueName_DP, producerPerms))

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ast.DataPortAccessRestriction(dstCamkesComponentId, dstCamkesFeatureQueueName_DP, ast.AccessType.R))

                  updateHolder(srcConnectionEnd_DP, holder)
                }
              }

              case ir.FeatureCategory.EventDataPort if !useCaseConnections => {
                // sel4 notification plus shared queue

                val queueSize: Z = srcQueues.get(srcFeaturePath).get.get(dstFeaturePath).get.queueSize

                val srcCamkesFeatureNotificationName = Util.genSeL4NotificationQueueName(srcFeature, queueSize)
                val dstCamkesFeatureNotificationName = Util.genSeL4NotificationName(dstFeature, T)

                { // notification connections
                  val notificationConnectorType: Sel4ConnectorTypes.Type =
                    if (dstToVM) Sel4ConnectorTypes.seL4GlobalAsynch
                    else Sel4ConnectorTypes.seL4Notification

                  val srcConnectionEnd_ED = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureNotificationName)
                  val dstConnectionEnd_ED = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureNotificationName)

                  var holder = getConnectionHolder(srcConnectionEnd_ED, notificationConnectorType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd_ED)

                  updateHolder(srcConnectionEnd_ED, holder)
                }

                { // queue dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcCamkesFeatureQueueName_ED: String = Util.getEventDataSBQueueSrcFeatureName(CommonUtil.getLastName(srcFeature.identifier), queueSize)
                  val dstCamkesFeatureQueueName_ED: String = Util.getEventDataSBQueueDestFeatureName(CommonUtil.getLastName(dstFeature.identifier))

                  val srcConnectionEnd_ED = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureQueueName_ED)
                  val dstConnectionEnd_ED = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureQueueName_ED)

                  var holder = getConnectionHolder(srcConnectionEnd_ED, queueConnectorType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd_ED)

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(symbolTable) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    holder = holder(configurationEntries = holder.configurationEntries :+
                      ast.GenericConfiguration(s"${holder.connectionName}.size = ${size};"))
                  }

                  if(!srcToVM) {
                    holder = holder(configurationEntries = holder.configurationEntries :+
                      ast.DataPortAccessRestriction(srcCamkesComponentId, srcCamkesFeatureQueueName_ED, ast.AccessType.W))
                  } else {
                    // don't add access restrictions when component is in a vm, otherwise get errors like
                    //
                    // default_error_fault_callback@guest_memory_helpers.c:19 Failed to handle fault addr: 0xdf001000
                    // --------
                    // Pagefault from [Linux]: write fault @ PC: 0x4008a4 IPA: 0xdf001000, FSR:
                  }

                  if(!dstToVM) {
                    holder = holder(configurationEntries = holder.configurationEntries :+
                      ast.DataPortAccessRestriction(dstCamkesComponentId, dstCamkesFeatureQueueName_ED, ast.AccessType.R))
                  } else {
                    // don't add access restrictions since in a vm

                    dstAadlThread.getDomain(symbolTable) match {
                      case Some(d) =>
                        // add the notification to the same domain as the component or will get an error like
                        // 'handle_event_bar_fault@cross_vm_connection.c:126 Connection is not configured with an emit function'
                        holder = holder(configurationEntries = holder.configurationEntries :+
                          ast.GenericConfiguration(s"${dstCamkesComponentId}.${dstCamkesFeatureNotificationName}_domain = ${d};"))
                      case _ =>
                    }
                  }


                  updateHolder(srcConnectionEnd_ED, holder)
                }
              }

              case ir.FeatureCategory.EventDataPort if useCaseConnections => {
                // shared queue -- custom CAmkES template handles notification

                val queueSize: Z = srcQueues.get(srcFeaturePath).get.get(dstFeaturePath).get.queueSize

                { // queue dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type = Sel4ConnectorTypes.CASE_AADL_EventDataport

                  val srcCamkesFeatureQueueName: String = Util.getEventDataSBQueueSrcFeatureName(CommonUtil.getLastName(srcFeature.identifier), queueSize)
                  val dstCamkesFeatureQueueName: String = Util.getEventDataSBQueueDestFeatureName(CommonUtil.getLastName(dstFeature.identifier))

                  val srcConnectionEnd: ast.ConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesFeatureQueueName)
                  val dstConnectionEnd: ast.ConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureQueueName)

                  var holder = getConnectionHolder(srcConnectionEnd, queueConnectorType)

                  holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(symbolTable) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    holder = holder(configurationEntries = holder.configurationEntries :+
                      ast.GenericConfiguration(s"${holder.connectionName}.size = ${size};"))
                  }

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ConnectionsSbTemplate.caseConnectorConfig_with_signalling(holder.connectionName, T))

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ConnectionsSbTemplate.caseConnectorConfig_connection_type(srcCamkesComponentId, srcCamkesFeatureQueueName, srcToVM))

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ConnectionsSbTemplate.caseConnectorConfig_connection_type(dstCamkesComponentId, dstCamkesFeatureQueueName, dstToVM))

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
                    ast.DataPortAccessRestriction(srcCamkesComponentId, srcCamkesFeatureName, ast.AccessType.W))

                  holder = holder(configurationEntries = holder.configurationEntries :+
                    ast.DataPortAccessRestriction(dstCamkesComponentId, dstCamkesFeatureName, ast.AccessType.R))

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
      val fromEnd: ConnectionEnd = connectionHolderEntry._1
      val holder: ConnectionHolder = connectionHolderEntry._2

      connections = connections :+ Util.createConnections(
        CAmkESConnectionType.Refinement,
        holder.connectionName,
        holder.connectionType,
        ISZ(fromEnd),
        holder.toConnectionEnds)

      val filtered = Set.empty[ast.Configuration] ++ holder.configurationEntries
      configurationEntries = configurationEntries ++ filtered.elements
    }

    var connectors: ISZ[ConnectorContainer] = ISZ()
    if(useCaseConnections) {
      val connectorName: String = ConnectionsSbTemplate.CASE_AADL_EventDataport
      val assemblyEntry: ast.Connector = ast.Connector(
        name = connectorName,

        from_type = ConnectorType.Dataport,
        from_template = None(),
        from_threads = z"0",
        from_hardware = F,

        to_type = ConnectorType.Dataports,
        to_template = None(),
        to_threads = z"-1",
        to_hardware = F,

        attributes = ISZ(Attribute(typ = "bool", name = "to_global_endpoint", value = "True"))
      )

      val connectorTemplate: ConnectorTemplate = ConnectorTemplate(
        fromTemplateName = ConnectionsSbTemplate.getCASE_AADL_EventDataport_From_TemplateFilename(),
        fromTemplate = Some(ConnectionsSbTemplate.getCASE_AADL_EventDataport_From_Template()),
        toTemplateName = ConnectionsSbTemplate.getCASE_AADL_EventDataport_To_TemplateFilename(),
        toTemplate = Some(ConnectionsSbTemplate.getCASE_AADL_EventDataport_To_Template())
      )

      connectors = connectors :+ ConnectorContainer(
        connectorName = connectorName,
        assemblyEntry = assemblyEntry,
        connectorTemplate = connectorTemplate)
    }

    return ConnectionContainer(connections, configurationEntries, connectors)
  }

}