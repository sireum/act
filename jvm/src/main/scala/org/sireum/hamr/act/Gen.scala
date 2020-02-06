// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.ops.ISZOps
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.ast.{ASTObject, BinarySemaphore, Component, Composition, Connection, ConnectionEnd, ConnectorType, Consumes, Dataport, Direction, Emits, Instance, Method, Parameter, Procedure, Provides, Semaphore, Uses}
import org.sireum.hamr.act.templates.EventDataQueueTemplate
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, FeatureEnd}
import org.sireum.message.Reporter

@record class Gen(model: Aadl, platform: ActPlatform.Type, hamrBasePackageName: Option[String], reporter: Reporter) {

  var topLevelProcess: Option[ir.Component] = None[ir.Component]()
  var typeHeaderFileName: String = ""

  var componentMap: HashMap[String, ir.Component] = HashMap.empty
  var typeMap: HashSMap[String, ir.Component] = HashSMap.empty
  var classifierMap: HashMap[String, ir.Component] = HashMap.empty

  var featureMap: HashMap[String, ir.Feature] = HashMap.empty
  var sharedData: HashMap[String, SharedData] = HashMap.empty
  var samplingPorts: HashMap[String, SamplingPortInterface] = HashMap.empty
  var srcQueues: Map[String, Map[String, QueueObject]] = Map.empty // {dstFeatureId -> {srcFeatureId -> queueobj}}

  // port-paths -> connInstances
  var inConnections: HashMap[String, ISZ[ir.ConnectionInstance]] = HashMap.empty
  var outConnections: HashMap[String, ISZ[ir.ConnectionInstance]] = HashMap.empty

  var connectors: ISZ[ast.Connector] = ISZ()
  var astObjects: ISZ[ASTObject] = ISZ()
  var monitors: HashSMap[String, Monitor] = HashSMap.empty // conn instname -> monitor
  var containers: ISZ[C_Container] = ISZ()
  var auxResourceFiles: ISZ[Resource] = ISZ()
  
  var periodicDispatcherNotifications: ISZ[ast.Emits] = ISZ()
  var periodicDispatcherCalendars: ISZ[ST] = ISZ()
  
  var camkesComponents: ISZ[ast.Component] = ISZ()
  var camkesConnections: ISZ[ast.Connection] = ISZ()
  var camkesConfiguration: ISZ[ST] = ISZ()
  
  var auxCImplIncludes: ISZ[ST] = ISZ()

  var hasPeriodicComponents: B = F
  val preventBadging: B = T

  var performHamrIntegration: B = Util.hamrIntegration(platform)
  
  var globalImports: Set[String] = Set.empty[String] + Util.camkesStdConnectors
  
  val hookupPeriodicComponentsToTimeServer: B = F
  
  var count: Z = 1 // 0 causes capability conflict issues
  def getCounter(): Z = {
    count = count + 1
    return count - 1
  }

  def getTypeHeaderFileName(): String = {
    return s"${typeHeaderFileName}.h"
  }
  
  def getTypeHeaderFileForInclude(): String = {
    return s"<${getTypeHeaderFileName()}>"
  }

  def hasErrors(): B = {
    return reporter.hasError
  }
  
  def process(cSources: ISZ[String]) : (Option[ActContainer], Reporter) = {

    val components = model.components.filter(f => f.category != ir.ComponentCategory.Data)
    if(components.size != z"1" || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }
    val system = components(0)

    buildComponentMap(system)

    auxCImplIncludes = cSources.map(c => st"""#include "../../../${c}"""")

    for(d <- model.dataComponents){ typeMap = typeMap + (Util.getClassifierFullyQualified(d.classifier.get) ~> d) }
    val sortedData = sortData(typeMap.values)

    resolve(system)

    if(!hasErrors()) {
      auxResourceFiles = auxResourceFiles :+ Util.createResource(s"${Util.DIR_INCLUDES}/$typeHeaderFileName.h", processDataTypes(sortedData), T)
    }

    if(!hasErrors()) {
      if(preventBadging) {
        reporter.warn(None(), Util.toolName, "Branding disabled")
      }
      gen(system)
    }

    if(!hasErrors() && performHamrIntegration) {
      val bufferSize = 
        featureMap.values.filter(f => Util.isEventPort(f) || Util.isDataPort(f)).size +  
        componentMap.values.filter(c => Util.isThread(c)).size
      val pair = StringTemplate.hamrIPC(bufferSize, hamrBasePackageName.get)

      auxResourceFiles = auxResourceFiles :+ Util.createResource(s"${Util.DIR_INCLUDES}/ipc.h", pair._1, T)
      auxResourceFiles = auxResourceFiles :+ Util.createResource(s"${Util.DIR_INCLUDES}/ipc.c", pair._2, T)
    }

    if(!hasErrors()) {
      // merge assemblies
      val assemblies: ISZ[Assembly] = astObjects.filter(f => f.isInstanceOf[Assembly]).map(m => m.asInstanceOf[Assembly])
      val otherAstObjects: ISZ[ASTObject] =  astObjects.filter(f => !f.isInstanceOf[Assembly])
      
      var instances: Set[Instance] = Set.empty
      var connections: Set[Connection] = Set.empty

      for(assembly <- assemblies) {
        instances = instances ++ assembly.composition.instances
        connections = connections ++ assembly.composition.connections
      }

      connections = connections ++ camkesConnections
      
      if(hasPeriodicComponents) {
        // add the dispatcher component

        val dispatchComponent = TimerUtil.dispatchComponent(periodicDispatcherNotifications)
        instances = instances + dispatchComponent
        containers = containers :+ C_Container(dispatchComponent.component.name,
          ISZ(TimerUtil.dispatchComponentCSource(getTypeHeaderFileForInclude(), periodicDispatcherCalendars)), ISZ(), ISZ(), ISZ(), ISZ())

        // connect dispatch timer to time server
        connections = connections + createConnection(Sel4ConnectorTypes.seL4TimeServer,
          TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.TIMER_ID_DISPATCHER,
          TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_TIMER_ID)

        connections = connections + createConnection(Sel4ConnectorTypes.seL4GlobalAsynchCallback,
          TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_NOTIFICATION_ID,
          TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.TIMER_NOTIFICATION_DISPATCHER_ID)

        camkesConfiguration = camkesConfiguration :+ st"${TimerUtil.TIMER_INSTANCE}.timers_per_client = 1;"

        camkesConfiguration = camkesConfiguration :+ TimerUtil.configurationTimerAttribute(dispatchComponent.name, getCounter(), T)

        camkesConfiguration = camkesConfiguration :+ StringTemplate.configurationPriority(dispatchComponent.name, Util.DEFAULT_PRIORITY)
      }

      val composition = Composition(
        groups = ISZ(),
        exports = ISZ(),
        instances = instances.elements,
        connections = connections.elements
      )
      
      astObjects = ISZ(Assembly(
        configuration = (Set.empty[String] ++ camkesConfiguration.map((m : ST) => m.render)).elements,
        composition = composition))
      
      astObjects = astObjects ++ otherAstObjects
      
      return (Some(ActContainer(
        rootServer = Util.getLastName(topLevelProcess.get.identifier),
        connectors = connectors,
        models = astObjects,
        monitors = monitors.values,
        samplingPorts = samplingPorts.values,
        cContainers = containers,
        auxFiles = auxResourceFiles,
        globalImports = globalImports.elements,
        requiresTimeServer = hasPeriodicComponents)), reporter)

    } else {
      return ((None[ActContainer](), reporter))
    }
  }

  def gen(c: ir.Component) : Unit = {
    c.category match {
      case ir.ComponentCategory.System =>
        for (sc <- c.subComponents) {
          gen(sc)
        }
        camkesConnections = camkesConnections ++ processConnections(c)

      case ir.ComponentCategory.Process =>
        val g = genContainer(c)
        astObjects = astObjects :+ Assembly(configuration = ISZ(),
          composition = g)
        
      case ir.ComponentCategory.Thread =>
        genThread(c)
                
      case _ =>
        for (sc <- c.subComponents) {
          gen(sc)
        }
    }
  }


  var i: Z = 1

  def newConn(): String = {
    val ret = s"conn${i}"
    i = i + 1
    return ret
  }
  
  def createConnection(connectionType: Sel4ConnectorTypes.Type,
                       srcComponent: String, srcFeature: String,
                       dstComponent: String, dstFeature: String): ast.Connection = {
    val from_ends: ISZ[ConnectionEnd] = ISZ(ConnectionEnd(
      isFrom = T,
      component = srcComponent,
      end = srcFeature))

    val to_ends: ISZ[ConnectionEnd] = ISZ(ConnectionEnd(
      isFrom = F,
      component = dstComponent,
      end = dstFeature))

    val con = Connection(
      name = newConn(),
      connectionType = s"$connectionType",
      from_ends = from_ends,
      to_ends = to_ends
    )

    return con
  }
  
  def processConnections(c: ir.Component): ISZ[ast.Connection] = {
    
    def createNotificationConnection(srcComponent: String, srcFeature: String,
                                     dstComponent: String, dstFeature: String): ast.Connection = {
      return createConnection(Sel4ConnectorTypes.seL4Notification, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createRPCConnection(srcComponent: String, srcFeature: String,
                            dstComponent: String, dstFeature: String) : ast.Connection = {
      return createConnection(Sel4ConnectorTypes.seL4RPCCall, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createSharedDataCounterConnection(conn: ir.ConnectionInstance) : ast.Connection = {
      val srcComponent = Util.getLastName(conn.src.component)
      val srcFeature = featureMap.get(Util.getName(conn.src.feature.get)).get

      val dstComponent = Util.getLastName(conn.dst.component)
      val dstFeature = featureMap.get(Util.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.getEventSBCounterName(Util.getLastName(srcFeature.identifier))
      val dstFeatureName = Util.getEventSBCounterName(Util.getLastName(dstFeature.identifier))

      camkesConfiguration = camkesConfiguration :+ st"""${srcComponent}.${srcFeatureName}_access = "W";"""
      camkesConfiguration = camkesConfiguration :+ st"""${dstComponent}.${dstFeatureName}_access = "R";"""
      
      return createConnection(Sel4ConnectorTypes.seL4SharedData,
        srcComponent, srcFeatureName,
        dstComponent, dstFeatureName
      )
    }

    def createSharedData_SB_Queue_Connection(conn: ir.ConnectionInstance) : ast.Connection = {
      val srcComponent = Util.getLastName(conn.src.component)
      val srcId = Util.getName(conn.src.feature.get)
      val srcFeature = featureMap.get(srcId).get

      val dstComponent = Util.getLastName(conn.dst.component)
      val dstId = Util.getName(conn.dst.feature.get)
      val dstFeature = featureMap.get(dstId).get

      val queueObject: QueueObject = srcQueues.get(srcId).get.get(dstId).get
      
      val srcFeatureName = Util.getEventDataSBQueueSrcFeatureName(Util.getLastName(srcFeature.identifier), queueObject.queueSize)
      val dstFeatureName = Util.getEventDataSBQueueDestFeatureName(Util.getLastName(dstFeature.identifier))

      camkesConfiguration = camkesConfiguration :+ st"""${srcComponent}.${srcFeatureName}_access = "W";"""
      camkesConfiguration = camkesConfiguration :+ st"""${dstComponent}.${dstFeatureName}_access = "R";"""

      return createConnection(Sel4ConnectorTypes.seL4SharedData,
        srcComponent, srcFeatureName,
        dstComponent, dstFeatureName
      )
    }
    def createSharedDataConnection(conn: ir.ConnectionInstance) : ast.Connection = {
      val srcComponent = Util.getLastName(conn.src.component)
      val srcFeature = featureMap.get(Util.getName(conn.src.feature.get)).get

      val dstComponent = Util.getLastName(conn.dst.component)
      val dstFeature = featureMap.get(Util.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.brand(Util.getLastName(srcFeature.identifier))
      val dstFeatureName = Util.brand(Util.getLastName(dstFeature.identifier))

      return createConnection(Sel4ConnectorTypes.seL4SharedData,
        srcComponent, srcFeatureName,
        dstComponent, dstFeatureName
      )
    }

    def createDataConnection(conn: ir.ConnectionInstance) : ISZ[ast.Connection] = {
      val monitor = getMonitorForConnectionInstance(conn).get.asInstanceOf[TB_Monitor]

      val srcComponent = Util.getLastName(conn.src.component)
      val dstComponent = Util.getLastName(conn.dst.component)

      val srcFeature = featureMap.get(Util.getName(conn.src.feature.get)).get
      val dstFeature = featureMap.get(Util.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
      val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

      var ret: ISZ[ast.Connection] = ISZ()
      
      // rpc src to mon
      ret = ret :+ createRPCConnection(srcComponent, srcFeatureName, monitor.i.name, monitor.providesVarName)

      // rpc mon to dst
      ret = ret :+ createRPCConnection(dstComponent, dstFeatureName, monitor.i.name, monitor.providesVarName)

      // notification monsig to dst
      return ret :+ createNotificationConnection(
        monitor.i.name, "monsig",
        dstComponent, Util.genSeL4NotificationName(dstFeature, T)
      )
    }

    def createDataConnection_Ihor(conn: ir.ConnectionInstance) :ISZ[ast.Connection] = {
      val monitor = getMonitorForConnectionInstance(conn).get.asInstanceOf[Ihor_Monitor]

      val srcComponent = Util.getLastName(conn.src.component)
      val dstComponent = Util.getLastName(conn.dst.component)

      val srcFeature = featureMap.get(Util.getName(conn.src.feature.get)).get
      val dstFeature = featureMap.get(Util.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
      val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

      var ret: ISZ[ast.Connection] = ISZ()
      
      // rpc src to mon
      ret = ret :+ createRPCConnection(srcComponent, srcFeatureName, monitor.i.name, monitor.providesSenderVarName)

      // rpc mon to dst
      ret = ret :+ createRPCConnection(dstComponent, dstFeatureName, monitor.i.name, monitor.providesReceiverVarName)

      // notification monsig to dst
      return ret :+ createNotificationConnection(
        monitor.i.name, "monsig",
        dstComponent, Util.genSeL4NotificationName(dstFeature, T)
      )
    }

    var connections: ISZ[ast.Connection] = ISZ()

    val handledConns = c.connectionInstances.filter(conn => isHandledConnection(conn))
    val unhandledConns = c.connectionInstances.filter(conn => !isHandledConnection(conn))

    resolveSharedDataFeatures(handledConns)
    val missingFeatures = sharedData.values.filter((f: SharedData) => f.ownerFeature.isEmpty)
    if(missingFeatures.nonEmpty) {
      reporter.error(None(), Util.toolName, s"Could not find the owner for the following data subcomponents: ${(missingFeatures.map((f: SharedData) => f.subcomponentId), ", ")}")
    }
    
    for(conn <- handledConns) {
      val dstPath = Util.getName(conn.dst.feature.get)
      val fdst: ir.Feature = featureMap.get(dstPath).get

      val srcComponent = Util.getLastName(conn.src.component)
      
      val dstComponentName = Util.getLastName(conn.dst.component)
      val dstFeatureName = Util.getLastName(conn.dst.feature.get)

      conn.kind match {
        case ir.ConnectionKind.Port =>
          fdst.category match {
            case ir.FeatureCategory.DataPort =>
              platform match {
                case ActPlatform.SeL4_TB => connections = connections ++ createDataConnection(conn)
                case ActPlatform.SeL4 => connections = connections ++ createDataConnection(conn)
                case ActPlatform.SeL4_Only => connections = connections :+ createSharedDataConnection(conn) // adventium dataport profile
              }

            case ir.FeatureCategory.EventDataPort =>
              
              def eventDataPort_SB_Connection_Profile(): Unit = {
                // sel4 notification plus shared queue

                val srcPath: String = Util.getName(conn.src.feature.get)
                
                val srcFeature: ir.Feature = featureMap.get(srcPath).get
                
                val queueSize: Z = srcQueues.get(srcPath).get.get(dstPath).get.queueSize
                
                connections = connections :+ createNotificationConnection(
                  srcComponent, Util.genSeL4NotificationQueueName(srcFeature, queueSize), 
                  dstComponentName, Util.genSeL4NotificationName(fdst, T))
                
                connections = connections :+ createSharedData_SB_Queue_Connection(conn)                
              }
              
              platform match {
                case ActPlatform.SeL4 => connections = connections ++ createDataConnection(conn)
                case ActPlatform.SeL4_TB => connections = connections ++ createDataConnection(conn)
                case ActPlatform.SeL4_Only => eventDataPort_SB_Connection_Profile()
              }

            case ir.FeatureCategory.EventPort =>
              
              def eventPort_TB_Connection_Profile(): Unit = {
                if(shouldUseMonitorForEventPort(fdst)) {
                  // monitor
                  connections = connections ++ createDataConnection_Ihor(conn)
                } else {
                  // just a sel4 notification
                  val srcFeature = Util.getLastName(conn.src.feature.get)
                  
                  connections = connections :+ createNotificationConnection(
                    srcComponent, Util.brand(srcFeature), 
                    dstComponentName, Util.brand(dstFeatureName))
                }
              }

              def eventPort_SB_Connection_Profile(): Unit = {
                // sel4 notification plus shared counter
                val srcFeature = Util.getLastName(conn.src.feature.get)
                
                
                connections = connections :+ createNotificationConnection(
                  srcComponent, Util.brand(srcFeature), 
                  dstComponentName, Util.brand(dstFeatureName))
                
                connections = connections :+ createSharedDataCounterConnection(conn)
              }

              platform match {
                case ActPlatform.SeL4 => eventPort_TB_Connection_Profile()
                case ActPlatform.SeL4_TB => eventPort_TB_Connection_Profile()
                case ActPlatform.SeL4_Only => eventPort_SB_Connection_Profile() // ihor aadl-event-direct profile
              }

            case _ => halt (s"not expecting ${fdst.category}")
          }
        case ir.ConnectionKind.Access =>
          fdst.category match {
            case ir.FeatureCategory.SubprogramAccess =>
              val srcFeature = Util.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(srcComponent, srcFeature, dstComponentName, dstFeatureName)
            case ir.FeatureCategory.SubprogramAccessGroup =>
              val srcFeature = Util.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(srcComponent, srcFeature, dstComponentName, dstFeatureName)

            case ir.FeatureCategory.DataAccess =>
              val sd = sharedData.get(Util.getName(conn.src.component)).get
              val dstComp = componentMap.get(Util.getName(conn.dst.component)).get
              val ownerId = Util.getName(sd.owner.identifier)
              val dstId = Util.getName(dstComp.identifier)

              if(ownerId != dstId) {
                connections = connections :+ createConnection(Sel4ConnectorTypes.seL4SharedData,
                  dstComponentName, dstFeatureName,
                  Util.getLastName(sd.owner.identifier), Util.getLastName(sd.ownerFeature.get.identifier)
                )
              } else {
                // Ignore connection to the owner component
              }
            case _ =>  halt (s"not expecting ${fdst.category}")
          }
        case _ => halt(s"not expecting ${conn.kind}")
      }
    }

    return connections
  }  

  def genContainer(c : ir.Component) : Composition = {
    assert(c.category == ir.ComponentCategory.Process)

    var connections: ISZ[Connection] = ISZ()

    var instances: ISZ[Instance] = ISZ()
    var dispatchNotifications: ISZ[Emits] = ISZ()
    var calendars: ISZ[ST]= ISZ()

    for(sc <- c.subComponents) {
      sc.category match {
        case ir.ComponentCategory.Thread =>
          val componentId = Util.getLastName(sc.identifier)
          val classifier = Util.getClassifier(sc.classifier.get)
          instances = instances :+
            Instance(address_space =  "",
              name = componentId,
              component = genThread(sc))
          
          if(Util.getDispatchProtocol(sc) == Some(Dispatch_Protocol.Periodic)) {
            
            if(hookupPeriodicComponentsToTimeServer) {
              // connect component to time server
              connections = connections :+ createConnection(Sel4ConnectorTypes.seL4TimeServer,
                componentId, TimerUtil.TIMER_ID,
                TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_TIMER_ID)

              // timer attribute
              camkesConfiguration = camkesConfiguration :+ TimerUtil.configurationTimerAttribute(
                componentId, getCounter(), F)
            }

            val componentNotificationName = TimerUtil.componentNotificationName(None())
            val dispatcherNotificationName = TimerUtil.componentNotificationName(Some(sc))

            // emit notification when component's period occurs
            dispatchNotifications = dispatchNotifications :+ Emits(
              name = dispatcherNotificationName,
              typ = Util.NOTIFICATION_TYPE)
            
            // connect dispatcher to component
            connections = connections :+ createConnection(Sel4ConnectorTypes.seL4Notification,
              TimerUtil.DISPATCH_PERIODIC_INSTANCE, dispatcherNotificationName,
              componentId, componentNotificationName)

            val period: Z = if(Util.getPeriod(sc).isEmpty) {
              reporter.warn(None(), Util.toolName, s"Period not provided for periodic component ${classifier}, using ${Util.DEFAULT_PERIOD}")
              Util.DEFAULT_PERIOD
            } else {
              Util.getPeriod(sc).get
            }
            calendars = calendars :+ TimerUtil.calendar(sc, period)
          }
          
          Util.getStackSizeInBytes(sc) match {
            case Some(bytes) =>
              camkesConfiguration = camkesConfiguration :+ StringTemplate.configurationStackSize(componentId, bytes)
            case _ =>
          }
        case ir.ComponentCategory.Subprogram =>
          var params: ISZ[Parameter] = ISZ()
          for(f <- sc.features) {
            val fend = f.asInstanceOf[ir.FeatureEnd]
            params = params :+ Parameter(array = F,
              direction = if(fend.direction == ir.Direction.In) Direction.In else Direction.Out,
              name = Util.getLastName(f.identifier),
              typ = Util.getClassifier(fend.classifier.get))
          }
          val method = Method(
            name = Util.getLastName(sc.identifier),
            parameters = params,
            returnType = None[String]()
          )

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(method), includes = ISZ())

        case ir.ComponentCategory.SubprogramGroup =>
          var methods: ISZ[Method] = ISZ()

          for(m <- sc.features) {
            m match {
              case spa: ir.FeatureAccess =>
                if(spa.classifier.nonEmpty) {
                  val spComp = classifierMap.get(spa.classifier.get.name).get
                  assert(spComp.category == ir.ComponentCategory.Subprogram)

                  val methodName = Util.getLastName(spa.identifier)
                  var params: ISZ[Parameter] = ISZ()
                  for(param <- spComp.features) {
                    param match {
                      case p: ir.FeatureEnd =>
                        assert(param.category == ir.FeatureCategory.Parameter)

                        val paramName = Util.getLastName(param.identifier)
                        val dir: Direction.Type = p.direction match {
                          case ir.Direction.In => Direction.In
                          case ir.Direction.Out => Direction.Out
                          case x => halt(s"Unexpected direction ${x}")
                        }
                        val typName = Util.getClassifierFullyQualified(p.classifier.get)

                        params = params :+ Parameter(array = F, direction = dir, name = paramName, typ = typName)

                    }
                  }

                  methods = methods :+ Method(name = methodName, parameters = params, returnType = None[String]())
                } else{
                  reporter.error(None(), Util.toolName, s"Could not resolve feature ${Util.getName(spa.identifier)} from ${Util.getName(sc.identifier)}")
                }
              case _ =>
            }
          }

          if(sc.subComponents.nonEmpty) { halt(s"Subprogram group subcomponents not currently handled: ${sc}")}

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = methods, includes = ISZ())

        case ir.ComponentCategory.Data =>
          val typeName = Util.getClassifierFullyQualified(sc.classifier.get)

          val readMethod = Method(
            name = s"read_${typeName}",
            parameters = ISZ(Parameter(array = F, direction = Direction.Out, name = "arg", typ = typeName)),
            returnType = None[String]()
          )

          val writeMethod = Method(
            name = s"write_${typeName}",
            parameters = ISZ(Parameter(array = F, direction = Direction.Refin, name = "arg", typ = typeName)),
            returnType = None[String]()
          )

          val procName = Util.getSharedDataInterfaceName(sc.classifier.get)
          astObjects = astObjects :+ Procedure(
            name = procName, 
            methods = ISZ(readMethod, writeMethod), 
            includes = ISZ(getTypeHeaderFileForInclude())
          )

          Util.getCamkesOwnerThread(sc.properties) match {
            case Some(owner) =>
              val threads = componentMap.values.filter(f => f.category == ir.ComponentCategory.Thread)
              val thread = threads.filter(f => f.classifier.get.name == owner || Util.getClassifier(f.classifier.get) == owner)

              if(thread.nonEmpty) {
                val theOwner = thread(0)
                if(thread.size > 1) {
                  reporter.warn(None(), Util.toolName, s"Found multiple matches for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property: ${owner}")
                }
                if(Util.getClassifier(theOwner.classifier.get) == owner){
                  reporter.warn(None(), Util.toolName, s"Fully qualified name '${theOwner.classifier.get.name}' should be used for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property")
                }

                val subcomponentId = Util.getLastName(sc.identifier)
                sharedData = sharedData +
                  (Util.getName(sc.identifier) ~> SharedData(theOwner, None[ir.FeatureAccess](), sc.classifier.get, subcomponentId))

              } else {
                reporter.error(None(), Util.toolName, st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread}:  Could not locate component '${owner}'.  Please use one of the following:
                             |  ${(threads.map((m: ir.Component) => m.classifier.get.name), "\n")}""".render)
              }
            case _ =>
              reporter.error(None(), Util.toolName, st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property missing""".render)
          }

        case _ =>
          halt(s"Not handling: subcomponent of type '${sc.category}'.  ${Util.getName(sc.identifier)}")
      }
    }

    if(hasPeriodicComponents) { // add the periodic dispatcher component
      
      globalImports = globalImports + Util.camkesStdConnectors
      globalImports = globalImports + Util.camkesGlobalConnectors
      globalImports = globalImports + TimerUtil.TIMER_SERVER_IMPORT

      periodicDispatcherNotifications = periodicDispatcherNotifications ++ dispatchNotifications
      periodicDispatcherCalendars = periodicDispatcherCalendars ++ calendars
    }

    val monInstances = monitors.values.map((m: Monitor) => m.i)
    return Composition(
      groups = ISZ(),
      exports = ISZ(),
      instances = instances ++ monInstances,
      connections = connections ++ processConnections(c)
    )
  }

  def getSamplingPort(fend: FeatureEnd): SamplingPortInterface = {
    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(fend.classifier.get)).get
    val sel4PortType: String = Util.getSel4TypeName(aadlPortType)
    return samplingPorts.get(sel4PortType).get
  }

  def genThread(c : ir.Component) : Component = {
    assert(c.category == ir.ComponentCategory.Thread)
    assert(c.subComponents.isEmpty)
    assert(c.connectionInstances.isEmpty)

    val cid = Util.getClassifier(c.classifier.get)

    var provides: ISZ[Provides] = ISZ()
    var uses: ISZ[Uses] = ISZ()
    var emits: ISZ[Emits] = ISZ()
    var consumes: ISZ[Consumes] = ISZ()
    var dataports: ISZ[Dataport] = ISZ()

    var cSources: ISZ[Resource] = ISZ()
    
    var camkesIncludes: Set[String] = Set.empty
    camkesIncludes = camkesIncludes + getTypeHeaderFileForInclude()

    var externalCSources: ISZ[String] = ISZ()
    var externalCIncludeDirs: ISZ[String] = ISZ()
    
    var imports : Set[String] = Set.empty

    var cInterfaceStatements: ISZ[ST] = ISZ()
    var cImplIncludes: ISZ[ST] = ISZ()
    var cImpls: ISZ[ST] = ISZ()
    var cPreInits: ISZ[ST] = ISZ()
    var cPostInits: ISZ[ST] = ISZ()
    var cDrainQueues: ISZ[(ST, ST)] = ISZ()

    for(f <- c.features.filter(_f => _f.isInstanceOf[ir.FeatureAccess])) {
      def handleSubprogramAccess(): Unit = {
        val fend = f.asInstanceOf[ir.FeatureAccess]
        val fid = Util.getLastName(f.identifier)

        val proc = Util.getClassifier(fend.classifier.get)

        fend.accessType match {
          case ir.AccessType.Requires =>
            imports = imports + Util.getInterfaceFilename(proc)
            uses = uses :+ Uses(
              name = fid,
              optional = F,
              typ = proc)
          case ir.AccessType.Provides =>
            imports = imports + Util.getInterfaceFilename(proc)
            provides = provides :+ Provides(
              name = fid,
              typ = proc)
        }
      }

      def handleDataAccess(): Unit = {
        val fend = f.asInstanceOf[ir.FeatureAccess]
        val fid = Util.getLastName(f.identifier)

        val typeName = Util.getClassifierFullyQualified(fend.classifier.get)
        val interfaceName = Util.getSharedDataInterfaceName(fend.classifier.get)

        fend.accessType match {
          case ir.AccessType.Requires =>
            imports = imports + Util.getInterfaceFilename(interfaceName)
            dataports = dataports :+ Dataport(
              name = fid,
              optional = F,
              typ = typeName)
          case ir.AccessType.Provides =>
            imports = imports + Util.getInterfaceFilename(interfaceName)
            dataports = dataports :+ Dataport(
              name = fid,
              optional = F,
              typ = typeName)
        }
      }

      f.category match {
        case ir.FeatureCategory.SubprogramAccessGroup =>
          handleSubprogramAccess()
        case ir.FeatureCategory.SubprogramAccess =>
          handleSubprogramAccess()

        case ir.FeatureCategory.DataAccess =>
          handleDataAccess()

        case _ =>
          reporter.error(None(), Util.toolName, s"Not expecting AccessType: ${Util.getName(f.identifier)}")
      }
    }

    for(f <- c.features.filter(_f => _f.isInstanceOf[ir.FeatureEnd])) {
      val fend = f.asInstanceOf[ir.FeatureEnd]
      val fpath = Util.getName(fend.identifier)
      val fid = Util.getLastName(f.identifier)
      
      def handleDataPort_SB_Profile(): Unit = {
        assert(fend.direction == ir.Direction.In || fend.direction == ir.Direction.Out)

        val samplingPort: SamplingPortInterface = getSamplingPort(fend)
        
        camkesIncludes = camkesIncludes + s"<${Os.path(samplingPort.headerPath).name}>"

        externalCSources = externalCSources :+ samplingPort.implPath
        externalCIncludeDirs = externalCIncludeDirs :+ Os.path(samplingPort.headerPath).up.value

        cImpls = StringTemplate.sbSamplingPortGlobalVarDecl(samplingPort, fend) +: cImpls

        camkesConfiguration = camkesConfiguration :+ 
          StringTemplate.sbSamplingPortConfigurationEntry(
            Util.getLastName(c.identifier), samplingPort, fend)
        
        dataports = dataports :+ Dataport(
          name = Util.brand(fid),
          optional = F,
          typ = samplingPort.structName
        )
      }
      
      def handleDataPort_TB_Profile(): Unit = {
        fend.direction match {
          case ir.Direction.In =>
            // uses monitor
            // consumes notification
            if(inConnections.get(fpath).nonEmpty) {
              val monitor = getMonitorForInPort(fend).get.asInstanceOf[TB_Monitor]
              imports = imports + Util.getInterfaceFilename(monitor.interface.name)

              uses = uses :+ Uses(
                name = Util.genMonitorFeatureName(fend, None[Z]()),
                typ = monitor.interface.name,
                optional = F)

              consumes = consumes :+ Consumes(
                name = Util.genSeL4NotificationName(fend, T),
                typ = Util.getMonitorNotificationType(fend.category),
                optional = F)
            }
          case ir.Direction.Out =>
            // uses monitor
            outConnections.get(fpath) match {
              case Some(outs) =>
                var i: Z = 0
                for(o <- outs) {
                  val monitor = monitors.get(Util.getName(o.name)).get.asInstanceOf[TB_Monitor]
                  imports = imports + Util.getInterfaceFilename(monitor.interface.name)

                  uses = uses :+ Uses(
                    name = Util.genMonitorFeatureName(fend, Some(i)),
                    typ = monitor.interface.name,
                    optional = F
                  )
                  i = i + 1
                }
              case _ =>
            }
          case _ =>
            halt(s"Not expecting direction ${fend.direction}")
        }
      }

      def handleEventPort(isEventData: B): Unit = {
        fend.direction match {
          case ir.Direction.In =>
            
            val isConnected = inConnections.contains(Util.getName(fend.identifier))
            
            if(isConnected && (Util.getComputeEntrypointSourceText(fend.properties).nonEmpty || performHamrIntegration)) {
              val name = Util.genSeL4NotificationName(fend, isEventData)
              val handlerName = s"${name}_handler"
              val regCallback = s"${name}_reg_callback"

              if (isEventData) {
                cImpls = cImpls :+ StringTemplate.cEventNotificiationHandler(handlerName, regCallback)
              }
              cPreInits = cPreInits :+ StringTemplate.cRegCallback(handlerName, regCallback)
            } else {
              reporter.warn(None(), Util.toolName, s"port: ${fid} in thread: ${cid} does not have a compute entrypoint and will not be dispatched.")
            }

          case _ =>
        }
      }

      def eventDataPort_SB_Component_Profile(): Unit = {
        // notification plus a shared counter

        // correct for fan out connections?

        val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(fend.classifier.get)).get
        //val portComponentTypeName = Util.getClassifierFullyQualified(portComponentType.classifier.get)
        val sel4TypeName = Util.getSel4TypeName(aadlPortType)
        
        auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()
        
        fend.direction match {
          case ir.Direction.In =>
            val queueSize = Util.getQueueSize(fend)
            val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

            val queueHeaderImplName = Util.getEventData_SB_QueueImplFileName(sel4TypeName, queueSize)
            val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)

            externalCSources = externalCSources :+ s"${Util.DIR_INCLUDES}/${queueHeaderImplName}"
            camkesIncludes = camkesIncludes + s"<${queueHeaderFilename}>"
            
            consumes = consumes :+ Consumes(
              name = Util.genSeL4NotificationName(fend, T),
              typ = Util.EVENT_NOTIFICATION_TYPE,
              optional = F)

            dataports = dataports :+ Dataport(
              name = Util.getEventDataSBQueueDestFeatureName(fid),
              typ = queueType,
              optional = F)

          case ir.Direction.Out =>

            val dsts: Map[String, QueueObject] = srcQueues.get(fpath).get // dstId -> queueObject

            for(qo <- dsts.valueSet.elements) {
              val queueSize = qo.queueSize
              val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)
              val queueHeaderImplName = Util.getEventData_SB_QueueImplFileName(sel4TypeName, queueSize)
              val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)

              externalCSources = externalCSources :+ s"${Util.DIR_INCLUDES}/${queueHeaderImplName}"
              camkesIncludes = camkesIncludes + s"<${queueHeaderFilename}>"

              emits = emits :+ Emits(
                name = Util.genSeL4NotificationQueueName(fend, queueSize),
                typ = Util.EVENT_NOTIFICATION_TYPE)

              dataports = dataports :+ Dataport(
                name = Util.getEventDataSBQueueSrcFeatureName(fid, queueSize),
                typ = queueType,
                optional = F)
            }
          case _ => halt(s"Unexpected port direction: ${fend.direction}")
        }
      }
      
      def handleEventDataPort_Ihor(): Unit = {
        fend.direction match {
          case ir.Direction.In =>
            // uses AADLEvent_Receiver event
            // consumes ReceiveEvent e
            if (inConnections.get(fpath).nonEmpty) {
              val monitor = getMonitorForInPort(fend).get.asInstanceOf[Ihor_Monitor]
              imports = imports + Util.getInterfaceFilename(monitor.interfaceReceiver.name)

              uses = uses :+ Uses(
                name = Util.genMonitorFeatureName(fend, None[Z]()),
                typ = monitor.interfaceReceiver.name,
                optional = F)

              consumes = consumes :+ Consumes(
                name = Util.genSeL4NotificationName(fend, T),
                typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE,
                optional = F)

              val isConnected = inConnections.contains(Util.getName(fend.identifier))
              
              if(isConnected && (Util.getComputeEntrypointSourceText(fend.properties).nonEmpty || performHamrIntegration)) {
                val name = Util.genSeL4NotificationName(fend, T)
                val handlerName = s"${name}_handler"
                val regCallback = s"${name}_reg_callback"

                cImpls = cImpls :+ StringTemplate.cEventNotificiationHandler(handlerName, regCallback)
                cPreInits = cPreInits :+ StringTemplate.cRegCallback(handlerName, regCallback)
              } else {
                reporter.warn(None(), Util.toolName, s"port: ${fid} in thread: ${cid} does not have a compute entrypoint and will not be dispatched.")
              }              
            }

          case ir.Direction.Out =>
            outConnections.get(fpath) match {
              case Some(outs) =>
                var i: Z = 0
                for (o <- outs) {
                  val monitor = monitors.get(Util.getName(o.name)).get.asInstanceOf[Ihor_Monitor]

                  imports = imports + Util.getInterfaceFilename(monitor.interfaceSender.name)

                  uses = uses :+ Uses(
                    name = Util.genMonitorFeatureName(fend, Some(i)),
                    typ = monitor.interfaceSender.name,
                    optional = F
                  )

                  i = i + 1
                }
            }
          case _ => halt(s"Unexpected direction: ${fend.direction}")
        }
      }
      
      f.category match {
        case ir.FeatureCategory.DataPort =>
          platform match {
            case ActPlatform.SeL4_Only => handleDataPort_SB_Profile()
            case ActPlatform.SeL4 => handleDataPort_TB_Profile()              
            case ActPlatform.SeL4_TB => handleDataPort_TB_Profile()
          }
          
        case ir.FeatureCategory.EventDataPort =>
          platform match {
            case ActPlatform.SeL4_Only =>
              //handleEventDataPort_Ihor()
              eventDataPort_SB_Component_Profile()

            case ActPlatform.SeL4 =>
              handleDataPort_TB_Profile()
              handleEventPort(T)

            case ActPlatform.SeL4_TB =>
              handleDataPort_TB_Profile()
              handleEventPort(T)
          }

        case ir.FeatureCategory.EventPort =>
          
          def handleEventPort_TB_Component_Profile(): Unit = {
            
            
            if(shouldUseMonitorForEventPort(fend)) {
              // monitor 
              
              fend.direction match {
                case ir.Direction.In =>
                  // import receiver interface
                  imports = imports + s""""../../interfaces/${Util.MONITOR_INTERFACE_NAME_RECEIVER}.idl4""""

                  // uses 
                  uses = uses :+ Uses(
                    name = Util.genMonitorFeatureName(fend, None[Z]()),
                    typ = Util.MONITOR_INTERFACE_NAME_RECEIVER,
                    optional = F,
                  )

                  // consumes notification
                  consumes = consumes :+ Consumes(
                    name = Util.genSeL4NotificationName(fend, T),
                    typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE,
                    optional = F)

                case ir.Direction.Out =>
                  // import sender interface
                  imports = imports + s""""../../interfaces/${Util.MONITOR_INTERFACE_NAME_SENDER}.idl4""""

                  outConnections.get(fpath) match {
                    case Some(outs) =>
                      var i: Z = 0
                      for (o <- outs) {

                        uses = uses :+ Uses(
                          name = Util.genMonitorFeatureName(fend, Some(i)),
                          typ = Util.MONITOR_INTERFACE_NAME_SENDER,
                          optional = F
                        )

                        i = i + 1
                      }
                  }

                case _ => halt(s"${fpath}: not expecting direction ${fend.direction}")
              }
            } else {
              // queue size == 1 so just an seL4Notification connection
              handleEventPort(F)

              fend.direction match {
                case ir.Direction.In =>
                  // consumes notification
                  consumes = consumes :+ Consumes(
                    name = Util.brand(fid),
                    typ = Util.NOTIFICATION_TYPE,
                    optional = F)
                case ir.Direction.Out =>
                  // emits notification
                  emits = emits :+ Emits(
                    name = Util.brand(fid),
                    typ = Util.NOTIFICATION_TYPE)
                case _ => halt(s"${fpath}: not expecting direction ${fend.direction}")
              }
            }
          }
          
          def handleEventPort_SB_Component_Profile(): Unit = {
            // notification plus a shared counter
            
            // correct for fan out connections?
            
            val counterType = Util.SB_EVENT_COUNTER_TYPE

            // add sb counter type decl resource
            auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()
            camkesIncludes = camkesIncludes + s"<${Util.SB_COUNTER_FILENAME}>"
            
            fend.direction match {
              case ir.Direction.In =>
                consumes = consumes :+ Consumes(
                  name = Util.getEventSBNotificationName(fid),
                  typ = Util.EVENT_NOTIFICATION_TYPE,
                  optional = F)

                dataports = dataports :+ Dataport(
                  name = Util.getEventSBCounterName(fid),
                  typ = counterType,
                  optional = F)
                
              case ir.Direction.Out =>
                emits = emits :+ Emits(
                  name = Util.getEventSBNotificationName(fid),
                  typ = Util.EVENT_NOTIFICATION_TYPE)
                
                dataports = dataports :+ Dataport(
                  name = Util.getEventSBCounterName(fid),
                  typ = counterType,
                  optional = F)

              case _ => halt(s"Unexpected port direction: ${fend.direction}")
            }
          }
          
          platform match {
            case ActPlatform.SeL4 => handleEventPort_TB_Component_Profile()
            case ActPlatform.SeL4_TB => handleEventPort_TB_Component_Profile()
            case ActPlatform.SeL4_Only => handleEventPort_SB_Component_Profile()
          }

        case _ =>
          reporter.warn(None(), Util.toolName, s"Skipping ${f.category} for ${fid}.${fid}")
      }

      generateC_InterfaceMethod(c, f.asInstanceOf[ir.FeatureEnd]) match {
        case Some(C_SimpleContainer(cIncludes, interface, impl, preInit, postInits, drainQueues)) =>
          if(cIncludes.nonEmpty) { cImplIncludes = cImplIncludes ++ cIncludes}
          if(interface.nonEmpty) { cInterfaceStatements = cInterfaceStatements :+ interface.get }
          if(impl.nonEmpty) { cImpls = cImpls :+ impl.get }
          if(preInit.nonEmpty) { cPreInits = cPreInits :+ preInit.get}
          if(postInits.nonEmpty) { cPostInits = cPostInits :+ postInits.get }
          if(drainQueues.nonEmpty) { cDrainQueues = cDrainQueues :+ drainQueues.get}
        case _ =>
      }
    }

    var binarySemaphores: ISZ[BinarySemaphore] = ISZ()
    var semaphores: ISZ[Semaphore] = ISZ()
    val binSem = Util.getDiscreetPropertyValue(c.properties, "camkes::Binary_Semaphore")
    binSem match {
      case Some(v: ir.ValueProp) =>
        binarySemaphores = binarySemaphores :+ BinarySemaphore(v.value)
      case _ =>
    }

    // FIXME: 2018.12.06 - changed binary semaphore to semaphore
    // has semaphore tb_dispatch_sem
    //binarySemaphores = binarySemaphores :+ BinarySemaphore(TimerUtil.SEM_DISPATCH)
    semaphores = semaphores :+ Semaphore(TimerUtil.SEM_DISPATCH)

    var cRunPreEntries: ISZ[ST] = cDrainQueues.map(x => x._1)

    var stRunLoopEntries: ISZ[ST] = cDrainQueues.map(x => x._2)

    Util.getDispatchProtocol(c) match {
      case Some(Dispatch_Protocol.Periodic) =>
        // import Timer.idl4
        imports = imports + Util.camkesStdConnectors;
        
        if(hookupPeriodicComponentsToTimeServer) {
          // uses Timer tb_timer;
          uses = uses :+ Uses(
            name = TimerUtil.TIMER_ID,
            typ = TimerUtil.TIMER_TYPE,
            optional = F)
        }
        
        // consumes Notification from periodic dispatcher
        consumes = consumes :+ Consumes(
          name = TimerUtil.componentNotificationName(None()),
          typ = Util.NOTIFICATION_TYPE,
          optional = F)
        
        cImpls = StringTemplate.periodicDispatchElems(cid, hookupPeriodicComponentsToTimeServer) +: cImpls

        cRunPreEntries = cRunPreEntries :+ StringTemplate.registerPeriodicCallback()

        Util.getComputeEntrypointSourceText(c.properties) match {
          case Some(handler) =>
            cInterfaceStatements = cInterfaceStatements :+ st"void ${handler}(const int64_t *);"

            val drains = StringTemplate.drainPeriodicQueue(cid, handler)

            cImpls = cImpls :+ drains._1
            stRunLoopEntries = stRunLoopEntries :+ drains._2
          case _ => reporter.warn(None(), Util.toolName, s"Periodic thread ${cid} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
        }
      case Some(Dispatch_Protocol.Sporadic) =>

      case x =>
        if(x.nonEmpty) {
          Util.reporter.error(None(), Util.toolName, s"Dispatch protocol $x for ${Util.getLastName(c.identifier)} is not supported")
        } else {
          reporter.warn(None(), Util.toolName, s"Dispatch Protocol not specified for ${Util.getLastName(c.identifier)}, assuming Sporadic")
        }
    }

    if(!performHamrIntegration) {
      Util.getInitializeEntryPoint(c.properties) match {
        case Some(methodName) =>
          cInterfaceStatements = cInterfaceStatements :+ st"""void ${methodName}(const int64_t *arg);"""
          val (cimpl, runEntry) = StringTemplate.componentInitializeEntryPoint(cid, methodName)
          cImpls = cImpls :+ cimpl
          cRunPreEntries = cRunPreEntries :+ runEntry
        case _ =>
      }
    }

    if(performHamrIntegration) {
      
      cImplIncludes = cImplIncludes :+ st"#include <all.h>" :+ st"#include <ipc.h>"

      cPreInits = cPreInits :+ st"" // blank line

      cPreInits = cPreInits :+ StringTemplate.hamrIntialise(hamrBasePackageName.get, cid)

      cPreInits = cPreInits :+ st"// fetch assigned port ids"

      var sends: ISZ[(ST, ST)] = ISZ()

      val destPortId = "destPortId"
      
      val outgoingPorts: ISZ[ir.FeatureEnd] = Util.getOutPorts(c)
      for(f <- outgoingPorts) {
        val srcFeatureName = Util.getName(f.identifier)
        if(outConnections.contains(srcFeatureName)) {
          
          {
            val outgoingConnections: ISZ[ir.ConnectionInstance] = outConnections.get(srcFeatureName).get

            val connectedPorts: ISZ[(ir.Component, ir.FeatureEnd)] = outgoingConnections.map(ci => {
              val dstComponent = componentMap.get(Util.getName(ci.dst.component)).get
              val dstFeature = featureMap.get(Util.getName(ci.dst.feature.get)).get.asInstanceOf[ir.FeatureEnd]
              (dstComponent, dstFeature)
            })

            var counter: Z = 0
            val assignOutgoingPortIds: ISZ[ST] = connectedPorts.map((pair: (ir.Component, ir.FeatureEnd)) => {
              val archId = StringTemplate.hamrGetArchId(hamrBasePackageName.get, pair._1)
              val dstPortName = Util.getLastName(pair._2.identifier)
              val dstGlobalVarId = s"${Util.nameToString(pair._2.identifier)}_id"

              // create a global var for the destination's port id
              cImpls = st"Z ${dstGlobalVarId};" +: cImpls

              // add entry to if else block
              val stpair = (st"${destPortId} == ${dstGlobalVarId}",
                st"${StringTemplate.hamrSendViaCAmkES(f, pair._1, pair._2, counter, hamrBasePackageName.get, typeMap)}")

              sends = sends :+ stpair
              
              counter = counter + 1
              
              // add preInit entry that fetches the id of the destination port
              st"${dstGlobalVarId} = ${archId}(SF)->${dstPortName}.id + seed;"

            })

            cPreInits = cPreInits ++ assignOutgoingPortIds
          }
        }
      }

      var receives: ISZ[ST] = ISZ()
      
      val inPorts: ISZ[ir.FeatureEnd] = Util.getInPorts(c).filter(inPort => {
        val portId = Util.getName(inPort.identifier)
        inConnections.contains(portId)
      })

      val archId = StringTemplate.hamrGetArchId(hamrBasePackageName.get, c)
      val tinPorts: ISZ[ST] = inPorts.map((m: ir.FeatureEnd) => {
        val portName = Util.getLastName(m.identifier)
        val camkesId = s"${portName}_id"

        cImpls = st"Z ${camkesId};" +: cImpls

        receives = receives :+ StringTemplate.hamrDrainQueue(m, hamrBasePackageName.get, typeMap)

        st"${camkesId} = ${archId}(SF)->${portName}.id + seed;"
      })
      cPreInits = cPreInits ++ tinPorts

      cPreInits = cPreInits :+ st"" // blank line
      
      cRunPreEntries = cRunPreEntries :+ StringTemplate.hamrInitialiseEntrypoint(hamrBasePackageName.get, cid)
      
      stRunLoopEntries = StringTemplate.hamrRunLoopEntries(hamrBasePackageName.get, c)

      val id = st"${cid} camkes_sendAsync"
      val ifElses = StringTemplate.ifEsleHelper(sends,
        Some(st"""printf("${id}: not expecting port id %i\n", ${destPortId});"""))

      cImpls = cImpls :+ st"""void camkes_sendAsync(Z ${destPortId}, art_DataContent d) {
                             |  ${ifElses}
                             |}
                             |"""

      cImpls = cImpls :+ st"""void transferIncomingDataToArt() {
                             |  ${(receives, "\n")}
                             |}
                             |"""

      externalCSources = externalCSources :+ s"${Util.DIR_INCLUDES}/ipc.c"
    }

    containers = containers :+ C_Container(
      componentId = cid,
      cSources = ISZ(genComponentTypeImplementationFile(c, auxCImplIncludes ++ cImplIncludes, cImpls, 
        cPreInits, cPostInits, cRunPreEntries, stRunLoopEntries)) ++ cSources,
      cIncludes = ISZ(genComponentTypeInterfaceFile(c, cInterfaceStatements)),
      sourceText = if(performHamrIntegration) ISZ() else Util.getSourceText(c.properties),
      externalCSources = externalCSources,
      externalCIncludeDirs = externalCIncludeDirs
    )

    return Component(
      control = T,
      hardware = F,
      name = cid,

      mutexes = ISZ(),
      binarySemaphores = binarySemaphores,
      semaphores = semaphores,

      dataports = dataports,
      emits = emits,
      uses = uses,
      consumes = consumes,
      provides = provides,
      includes = camkesIncludes.elements,
      attributes = ISZ(),

      imports = imports.elements
    )
  }

  def buildSamplingPortInterfaces(): Unit = {
    platform match {
      case ActPlatform.SeL4_TB => return
      case ActPlatform.SeL4 => return
      case ActPlatform.SeL4_Only => // use new mappings below
    }

    for (portPath <- outConnections.keys.filter(p => outConnections.get(p).get.size > 0)) {

      for (connInst <- outConnections.get(portPath).get()) {
        val dst: ir.Component = componentMap.get(Util.getName(connInst.dst.component)).get
        val dstFeature: ir.Feature = featureMap.get(Util.getName(connInst.dst.feature.get)).get

        connInst.kind match {
          case ir.ConnectionKind.Port =>
            dstFeature.category match {
              case ir.FeatureCategory.DataPort =>

                val dstFend = dstFeature.asInstanceOf[ir.FeatureEnd]
                val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(dstFend.classifier.get)).get
                val sel4PortType: String = Util.getSel4TypeName(aadlPortType)

                val name = s"sp_${sel4PortType}"
                val structName = s"${name}_t"
                
                val spi = SamplingPortInterface(
                  name = name,
                  structName = structName,
                  sel4TypeName = sel4PortType,
                  headerPath = s"${Util.DIR_SAMPLING_PORTS}/${name}.h",
                  implPath = s"${Util.DIR_SAMPLING_PORTS}/${name}.c"
                )

                samplingPorts = samplingPorts + (sel4PortType ~> spi)
              case _ =>
            }
          case ir.ConnectionKind.Access => // no monitor needed
          case _ => reporter.warn(None(), Util.toolName, s"processInConnections: Not handling ${connInst}")
        }
      }
    }
  }
  
  def buildSBQueues(): Unit = {
    platform match {
      case ActPlatform.SeL4_TB => return
      case ActPlatform.SeL4 => return
      case ActPlatform.SeL4_Only => // use new mappings below
    }

    var seen: Set[String] = Set.empty[String]

    for (in <- inConnections.entries.filter(p => p._2.size > 0)) {
      for (connInst <- in._2) {
        val src: ir.Component = componentMap.get(Util.getName(connInst.src.component)).get
        val srcFeature: ir.Feature = featureMap.get(Util.getName(connInst.src.feature.get)).get
        val srcId: String = Util.getName(srcFeature.identifier) 
          
        val dst: ir.Component = componentMap.get(Util.getName(connInst.dst.component)).get
        val dstFeature: ir.Feature = featureMap.get(Util.getName(connInst.dst.feature.get)).get
        val dstId: String = Util.getName(dstFeature.identifier)
        
        if (dstFeature.category == ir.FeatureCategory.EventDataPort) {
          val dstFend = dstFeature.asInstanceOf[ir.FeatureEnd]

          val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(dstFend.classifier.get)).get
          val sel4PortType: String = Util.getSel4TypeName(aadlPortType)

          val queueSize = Util.getQueueSize(dstFend)

          val queueName = Util.getEventDataSBQueueName(sel4PortType, queueSize)
          if (!seen.contains(queueName)) {
            val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4PortType, queueSize)
            val queueImplFilename = Util.getEventData_SB_QueueImplFileName(sel4PortType, queueSize)

            val interface = EventDataQueueTemplate.header(
              sbCounterFileName = Util.SB_COUNTER_FILENAME,
              counterTypeName = Util.SB_EVENT_COUNTER_TYPE,
              typeHeaderFileName = getTypeHeaderFileName(),

              queueElementTypeName = sel4PortType,
              queueSize = queueSize)

            val impl = EventDataQueueTemplate.implementation(
              queueHeaderFilename = queueHeaderFilename,
              queueElementTypeName = sel4PortType,
              queueSize = queueSize,

              counterTypeName = Util.SB_EVENT_COUNTER_TYPE)

            auxResourceFiles = auxResourceFiles :+ Resource(
              path = s"${Util.DIR_INCLUDES}/${queueHeaderFilename}",
              content = interface,
              overwrite = T,
              makeExecutable = F)

            auxResourceFiles = auxResourceFiles :+ Resource(
              path = s"${Util.DIR_INCLUDES}/${queueImplFilename}",
              content = impl,
              overwrite = T,
              makeExecutable = F)

            seen = seen + queueName
          }

          var dstMap = srcQueues.getOrElse(srcId, Map.empty)
          assert(!dstMap.contains(dstId))
          dstMap = dstMap + (dstId ~> QueueObject(queueName, queueSize))
          srcQueues = srcQueues + (srcId ~> dstMap)
        }
      }
    }
  }
  
  def buildMonitors(): Unit = {
    platform match {
      case ActPlatform.SeL4_TB => // 
      case ActPlatform.SeL4 => // 
      case ActPlatform.SeL4_Only => return  
    }
    
    for (portPath <- outConnections.keys.filter(p => outConnections.get(p).get.size > 0)) {
      var i: Z = 0
      for (connInst <- outConnections.get(portPath).get()) {

        val dst: ir.Component = componentMap.get(Util.getName(connInst.dst.component)).get
        val dstFeature: ir.Feature = featureMap.get(Util.getName(connInst.dst.feature.get)).get
        
        def handleDataPort(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceName = Util.getInterfaceName(f)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

          val providesVarName = "mon"
          
          val monitor = Component(
            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(Emits(name = "monsig", typ = Util.getMonitorNotificationType(f.category))),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(Provides(name = providesVarName, typ = interfaceName)),
            includes = ISZ(),
            attributes = ISZ(),
            imports = ISZ(st""""../../../${Util.DIR_INTERFACES}/${interfaceName}.idl4"""".render)
          )

          val inst: Instance = Instance(
            address_space = "", 
            name = StringUtil.toLowerCase(monitorName), 
            component = monitor)

          val paramType: ir.Component = typeMap.get(typeName).get
          val paramTypeName: String = Util.getSel4TypeName(paramType)

          val methods: ISZ[Method] = f.category match {
            case ir.FeatureCategory.EventDataPort => createQueueMethods(paramTypeName)
            case ir.FeatureCategory.DataPort => createReadWriteMethods(paramTypeName)
            case _ =>
              halt(s"not expecting ${f.category}")
          }

          val interface: Procedure = Procedure(
            name = interfaceName,
            methods = methods,
            includes = ISZ(getTypeHeaderFileForInclude())
          )

          val connInstName = Util.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_SRC}/${monitorName}.c"
          val cimplementation: Resource =
            if(f.category == ir.FeatureCategory.DataPort) {
              Util.createResource(implName, StringTemplate.tbMonReadWrite(paramTypeName, Util.getQueueSize(f), monitorName, getTypeHeaderFileForInclude(), preventBadging), T)
            } else {
              Util.createResource(implName, StringTemplate.tbEnqueueDequeue(paramTypeName, Util.getQueueSize(f), monitorName, getTypeHeaderFileForInclude(), preventBadging), T)
            }

          val interName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_INCLUDES}/${monitorName}.h"
          val cincludes = Util.createResource(interName, StringTemplate.tbInterface(s"__${monitorName}_H__"), T)

          monitors = monitors + (connInstName ~> 
            TB_Monitor(inst, interface, 
              providesVarName,
              cimplementation, cincludes, i, connInst))
        }

        def buildIhorMonitor(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceNameReceiver = Util.getInterfaceNameIhor(f, F)
          val interfaceNameSender = Util.getInterfaceNameIhor(f, T)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

          val providesReceiverVarName = "mon_receive"
          val providesSenderVarName = "mon_send"

          val paramType = typeMap.get(typeName).get
          val paramTypeName = Util.getSel4TypeName(paramType)

          val monitor = Component(
            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(Mutex("m")),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Emits(name = "monsig", typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE)),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(
              Provides(name = providesReceiverVarName, typ = interfaceNameReceiver),
              Provides(name = providesSenderVarName, typ = interfaceNameSender)),
            includes = ISZ(),
            attributes = ISZ(),
            imports = ISZ(
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameReceiver}.idl4"""".render,
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameSender}.idl4"""".render)
          )

          val inst: Instance = Instance(address_space = "", name = StringUtil.toLowerCase(monitorName), component = monitor)

          val receiveMethod = Method(
            name = "dequeue",
            parameters = ISZ(
              Parameter(F, Direction.Out, "m", paramTypeName)),
            returnType = Some("bool"))

          val interfaceReceiver: Procedure = Procedure(
            name = interfaceNameReceiver,
            methods = ISZ(receiveMethod),
            includes = ISZ(getTypeHeaderFileForInclude())
          )

          val sendMethod = Method(
            name = "enqueue",
            parameters = ISZ(
              Parameter(F, Direction.Refin, "m", paramTypeName)),
            returnType = Some("bool"))

          val interfaceSender: Procedure = Procedure(
            name = interfaceNameSender,
            methods = ISZ(sendMethod),
            includes = ISZ(getTypeHeaderFileForInclude())
          )

          val connInstName = Util.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_SRC}/${monitorName}.c"
          val cimplementation: Resource =
            Util.createResource(implName,
              StringTemplate.tbEnqueueDequeueIhor(paramTypeName, Util.getQueueSize(f), monitorName, getTypeHeaderFileForInclude(), preventBadging), T)

          val interName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_INCLUDES}/${monitorName}.h"
          val cincludes = Util.createResource(interName, StringTemplate.tbInterface(s"__${monitorName}_H__"), T)

          monitors = monitors + (connInstName ~>
            Ihor_Monitor(inst, interfaceReceiver, interfaceSender,
              providesReceiverVarName, providesSenderVarName,
              cimplementation, cincludes, i, connInst))
        }

        def buildIhorEventMonitor(f: ir.FeatureEnd): Unit = {
          assert(f.category == ir.FeatureCategory.EventPort)
          
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceNameReceiver = Util.getInterfaceNameIhor(f, F)
          val interfaceNameSender = Util.getInterfaceNameIhor(f, T)

          val providesReceiverVarName = "mon_receive"
          val providesSenderVarName = "mon_send"

          val monitor = Component(
            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(Mutex("m")),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Emits(name = "monsig", typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE)),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(
              Provides(name = providesReceiverVarName, typ = interfaceNameReceiver),
              Provides(name = providesSenderVarName, typ = interfaceNameSender)),
            includes = ISZ(),
            attributes = ISZ(),
            imports = ISZ(
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameReceiver}.idl4"""".render,
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameSender}.idl4"""".render)
          )

          val inst: Instance = Instance(address_space = "", name = StringUtil.toLowerCase(monitorName), component = monitor)

          val receiveMethod = Method(
            name = "get_events",
            parameters = ISZ(),
            returnType = Some("int32_t"))

          val interfaceReceiver: Procedure = Procedure(
            name = interfaceNameReceiver,
            methods = ISZ(receiveMethod),
            includes = ISZ(getTypeHeaderFileForInclude())
          )

          val sendMethod = Method(
            name = "raise",
            parameters = ISZ(),
            returnType = Some("void"))

          val interfaceSender: Procedure = Procedure(
            name = interfaceNameSender,
            methods = ISZ(sendMethod),
            includes = ISZ(getTypeHeaderFileForInclude())
          )

          val connInstName = Util.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_SRC}/${monitorName}.c"
          val cimplementation: Resource =
            Util.createResource(
              implName,
              StringTemplate.tbRaiseGetEvents(Util.getQueueSize(f), monitorName, preventBadging),
              T)

          val interName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_INCLUDES}/${monitorName}.h"
          val cincludes = Util.createResource(interName, StringTemplate.tbInterface(s"__${monitorName}_H__"), T)

          monitors = monitors + (connInstName ~>
            Ihor_Monitor(inst, interfaceReceiver, interfaceSender,
              providesReceiverVarName, providesSenderVarName,
              cimplementation, cincludes, i, connInst))
        }
        
        connInst.kind match {
          case ir.ConnectionKind.Port =>
            dstFeature.category match {
              case ir.FeatureCategory.DataPort =>
                platform match {
                  case ActPlatform.SeL4_TB => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4 => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4_Only => // use new mappings
                }
                
              case ir.FeatureCategory.EventDataPort =>
                platform match {
                  case ActPlatform.SeL4_TB => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4 => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4_Only => buildIhorMonitor(dstFeature.asInstanceOf[ir.FeatureEnd])
                }
                
              case ir.FeatureCategory.EventPort =>
                def evenPort_Sel4_TB_Profile(): Unit = {
                  if(shouldUseMonitorForEventPort(dstFeature)) {
                    buildIhorEventMonitor(dstFeature.asInstanceOf[ir.FeatureEnd])
                  } else {
                    // will use Notification
                  }
                }
                platform match {
                  case ActPlatform.SeL4 => evenPort_Sel4_TB_Profile()
                  case ActPlatform.SeL4_TB => evenPort_Sel4_TB_Profile()
                  case ActPlatform.SeL4_Only => // not using monitors 
                }

              case _ =>
                halt(s"not expecting ${dst}")
            }
          case ir.ConnectionKind.Access => // no monitor needed
          case _ =>
            reporter.warn(None(), Util.toolName, s"processInConnections: Not handling ${connInst}")
        }

        i = i + 1
      }
    }
  }

  def createReadWriteMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "read", parameters = ISZ(Parameter(F, Direction.Out, "m", typeName)), returnType = Some("bool")),
      Method(name = "write", parameters = ISZ(Parameter(F, Direction.Refin, "m", typeName)), returnType = Some("bool")))
  }

  def createQueueMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "enqueue", parameters = ISZ(Parameter(F, Direction.Refin, "m", typeName)), returnType = Some("bool")),
      Method(name = "dequeue", parameters = ISZ(Parameter(F, Direction.Out, "m", typeName)), returnType = Some("bool")))
  }


  def getMonitorForConnectionInstance(instance: ir.ConnectionInstance): Option[Monitor] = {
    for(m <- monitors.values if m.ci == instance){
      return Some(m)
    }
    return None[Monitor]()
  }

  def getMonitorForInPort(end: ir.FeatureEnd): Option[Monitor] = {
    val n = Util.getName(end.identifier)
    for(m <- monitors.values if Util.getName(m.ci.dst.feature.get) == n) {
      return Some(m)
    }
    return None[Monitor]()
  }

  def processDataTypes(values: ISZ[ir.Component]): ST = {
    val defs: ISZ[ST] = values.filter(v => TypeUtil.translateBaseType(v.classifier.get.name).isEmpty).map(v => processDataType(v, F))
    val macroname = s"__${Util.cbrand("AADL")}_${typeHeaderFileName}__H"
    return StringTemplate.tbTypeHeaderFile(macroname, typeHeaderFileName, defs, preventBadging)
  }

  def processDataType(c: ir.Component, isField: B): ST = {
    val s: ST =
      if (TypeUtil.isRecordType(c)) {
        val name = Util.getClassifierFullyQualified(c.classifier.get)
        if(isField) {
          st"""$name"""
        } else {
          val fields: ISZ[ST] = c.subComponents.map(sub => {
            val fname: String = Util.getLastName(sub.identifier)
            if(ISZOps(Util.cKeywords).contains(fname)) {
              reporter.error(None(), Util.toolName, s"The attribute '${fname}' from data type ${name} is a reserved keyword")
            }
            st"${processDataType(sub, T)} ${fname};"
          })

          st"""typedef
              |  struct ${name} {
              |    ${(fields, "\n")}
              |  } ${name};"""
        }
      } else if (TypeUtil.isBaseType(c)) {
        st"${TypeUtil.translateBaseType(c.classifier.get.name)}"
      } else if (isField) {
        st"${Util.getClassifierFullyQualified(c.classifier.get)}"
      } else if (TypeUtil.isMissingType(c)) {
        StringTemplate.tbMissingType()
      } else if (TypeUtil.isEnumDef(c)) {
        val enums = Util.getPropertyValues(c.properties, Util.PROP_DATA_MODEL__ENUMERATORS)
        val values: ISZ[String] = enums.map((e: ir.PropertyValue) => {
          e match {
            case ir.ValueProp(value) => value
            case _ =>
              reporter.error(None(), Util.toolName, s"Unexpected enum value: ${e}")
              ""
          }
        })
        val ename = Util.getClassifierFullyQualified(c.classifier.get)
        st"""typedef
            |  enum {${(values, ", ")}} ${ename};"""
      } else if (TypeUtil.isArrayDef(c)) {
        // TODO multidim arrays
        val name = Util.getClassifierFullyQualified(c.classifier.get)
        val dim: Z = TypeUtil.getArrayDimension(c) match {
          case Some(d) => d
          case _ =>
            reporter.error(None(), Util.toolName, s"Array dimension not specified for ${c.classifier.get.name}")
            z"-1"
        }
        val container = Util.getContainerName(name)
        st"""typedef ${TypeUtil.getArrayBaseType(c)} ${name} [${TypeUtil.getArrayDimension(c)}];
            |
            |typedef
            |  struct ${container} {
            |    ${name} f;
            |  } ${container};"""
      } else {
        reporter.error(None(), Util.toolName, s"Unexpected datatype: ${c}")
        st" "
      }


    return s
  }

  def generateC_InterfaceMethod(component: ir.Component, feature: ir.FeatureEnd): Option[C_SimpleContainer] = {

    def handleDataPort(): Option[C_SimpleContainer] = {

      def dataport_TB_Profile() : Option[C_SimpleContainer] = {
        val (suffix, mod) : (String, String) = feature.direction match {
          case ir.Direction.In => ("read", "")
          case ir.Direction.Out => ("write", "const ")
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(feature, None[Z]())
        val paramType = Util.getSel4TypeName(
          typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get)

        val inter = Some(st"""bool ${name}_${suffix}(${mod}${paramType} * ${name});""")
        val impl: Option[ST] =
          if(suffix == "write") {
            outConnections.get(Util.getName(feature.identifier)) match {
              case Some(conns) =>
                var accum: ISZ[ST] = ISZ()
                var i = 0
                val result = Util.brand("result")

                while (i < conns.size) {
                  accum = accum :+ st"""${result} &= ${name}${i}_${suffix}((${paramType} *) ${name});"""
                  i = i + 1
                }

                val methodName = s"${name}_${suffix}"
                Some(st"""/************************************************************************
                         | * ${methodName}:
                         | * Invoked from user code in the local thread.
                         | *
                         | * This is the function invoked by the local thread to make a
                         | * call to write to a remote data port.
                         | *
                         | * XXX: When simulating fan out, the caller of this function will only
                         | * receive a positive response when all enqueues are successful. When a
                         | * negative response is received it only indicates that at least one
                         | * enqueue attempt failed.
                         | *
                         | ************************************************************************/
                         |bool ${methodName}(${mod}${paramType} * ${name}){
                         |  bool ${result} = true;
                         |  ${(accum, "\n")}
                         |  return ${result};
                         |}""")
              case _ => None[ST]()
            }
          } else {
            None[ST]()
          }
        val cIncludes: ISZ[ST] = ISZ()      
        val preInits: Option[ST] = None()
        val postInits: Option[ST] = None()
        val drainQueues: Option[(ST, ST)] = None()

        return Some(C_SimpleContainer(cIncludes, inter, impl, preInits, postInits, drainQueues))
      } 
      
      def dataport_SB_Profile() : Option[C_SimpleContainer] = {
        val samplingPort: SamplingPortInterface = getSamplingPort(feature)

        val cImpl =  StringTemplate.sbSamplingPortImplementation(samplingPort, feature)

        val cInterface = StringTemplate.sbSamplingPortInterface(samplingPort, feature)
        
        val cIncludes: ISZ[ST] = ISZ()
        val preInits: Option[ST] = None()
        val postInits: Option[ST] = None()
        val drainQueues: Option[(ST, ST)] = None()
        return Some(C_SimpleContainer(cIncludes, Some(cInterface), Some(cImpl), preInits, postInits, drainQueues))
      }
      
      val ret: Option[C_SimpleContainer] = platform match {
        case ActPlatform.SeL4 => dataport_TB_Profile()
        case ActPlatform.SeL4_TB => dataport_TB_Profile()
        case ActPlatform.SeL4_Only => dataport_SB_Profile()
      }
      
      return ret
    }

    def handleEventData_SB_GlueCode_Profile(): Option[C_SimpleContainer] = {
      val featurePath = Util.getName(feature.identifier)
      val fid = Util.getLastName(feature.identifier)
      val featureName = Util.brand(fid)

      val aadlPortType = typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get
      val sel4TypeName = Util.getSel4TypeName(aadlPortType)

      
      val ret: Option[C_SimpleContainer] = feature.direction match {
        case ir.Direction.In =>

          val queueSize = Util.getQueueSize(feature)
          val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)
          val queueName = Util.getEventDataSBQueueName(sel4TypeName, queueSize)

          val featureQueueName = Util.getEventDataSBQueueDestFeatureName(fid)
          val featureNotificationName = Util.genSeL4NotificationName(feature, T)
          
          var drainQueue: Option[(ST,ST)] = None()
          var preInits: Option[ST] = None()
          var cInterface: Option[ST] = None()

          val recvQueueFeatureName = Util.getEventData_SB_RecvQueueFeatureName(fid)
          val recvQueueName = Util.getEventData_SB_RecvQueueName(sel4TypeName, queueSize)
          val recvQueueType = Util.getEventData_SB_RecvQueueTypeName(sel4TypeName, queueSize)
          
          val cImplementation: Option[ST] = Util.getComputeEntrypointSourceText(feature.properties) match {
            case Some(entryPointName) =>
              val varName = featureName

              val handlerName = s"${featureNotificationName}_handler"
              val regCallback = s"${featureNotificationName}_reg_callback"
              val counterVar = "numDropped"
              val counterType = Util.SB_EVENT_COUNTER_TYPE
              val dequeueMethodName = s"${featureName}_dequeue"
              val dequeuePollMethodName = s"${featureName}_dequeue_poll"
              val userEntrypointName = Util.getUserEventEntrypointMethodName(component, feature)
              
              drainQueue = Some((st"",
                st"""{
                    |  ${sel4TypeName} ${varName};
                    |  while (${dequeueMethodName}((${sel4TypeName} *) &${varName})) {
                    |    ${userEntrypointName}(&${varName});
                    |  }
                    |}"""))

              cInterface = Some(st"""bool ${dequeueMethodName}(${sel4TypeName} *);
                                    |
                                    |void ${entryPointName}(const ${sel4TypeName} *);""")
              
              preInits = Some(StringTemplate.cRegCallback(handlerName, regCallback))

              val invokeHandler = s"${entryPointName}((${sel4TypeName} *) in_arg);"              
              Some(st"""${StringTemplate.cEventNotificiationHandler(handlerName, regCallback)}
                       |
                       |${recvQueueType} ${recvQueueFeatureName};
                       |
                       |/************************************************************************
                       | * ${dequeuePollMethodName}:
                       | ************************************************************************/
                       |bool ${dequeuePollMethodName}(${counterType} *${counterVar}, ${sel4TypeName} *data) {
                       |  return ${queueName}_dequeue(&${recvQueueFeatureName}, ${counterVar}, data);
                       |}
                       |                     
                       |/************************************************************************
                       | * ${dequeueMethodName}:
                       | ************************************************************************/
                       |bool ${dequeueMethodName}(${sel4TypeName} *data) {
                       |  ${counterType} ${counterVar};
                       |  return ${dequeuePollMethodName}(&${counterVar}, data);
                       |}
                       |
                       |/************************************************************************
                       | * ${userEntrypointName}:
                       | *
                       | * This is the function invoked by an active thread dispatcher to
                       | * call to a user-defined entrypoint function.  It sets up the dispatch
                       | * context for the user-defined entrypoint, then calls it.
                       | *
                       | ************************************************************************/
                       |void ${userEntrypointName}(const ${sel4TypeName} * in_arg) {
                       |  ${invokeHandler}
                       |}
                       |"""
              )
            case _ => None[ST]()
          } 
          
          val cIncludes: ISZ[ST] = ISZ(st"""#include <${queueHeaderName}>
                                           |#include <${Util.SB_COUNTER_FILENAME}>""")
          val postInits: Option[ST]= Some( st"${recvQueueName}_init(&${recvQueueFeatureName}, ${featureQueueName});")
          
          Some(C_SimpleContainer(cIncludes, cInterface, cImplementation, preInits, postInits, drainQueue))
          
        case ir.Direction.Out =>
          
          val dsts: Map[String, QueueObject] = srcQueues.get(featurePath).get

          //val cIncludes: ISZ[ST] = ISZ(st"#include <${queueHeaderName}>")
          var cIncludes: ISZ[ST] = ISZ()
          
          //val postInit = st"${queueName}_init(${featureQueueName});"
          var _postInits: ISZ[ST] = ISZ()
          val enqueueEntries: ISZ[ST] = dsts.valueSet.elements.map(qo => {

            val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, qo.queueSize)
            val queueName = Util.getEventDataSBQueueName(sel4TypeName, qo.queueSize)

            val featureQueueName = Util.getEventDataSBQueueSrcFeatureName(fid, qo.queueSize)
            val featureNotificationName = Util.genSeL4NotificationQueueName(feature, qo.queueSize)
            
            cIncludes = cIncludes :+ st"#include <${queueHeaderName}>"
            _postInits = _postInits :+ st"${queueName}_init(${featureQueueName});"
            
            st"""${queueName}_enqueue(${featureQueueName}, (${sel4TypeName}*) data);
                |${featureNotificationName}_emit();
                |"""
          })
          
          val methodName = s"${featureName}_enqueue"

          val interface = st"""bool ${methodName}(const ${sel4TypeName} *);"""
          
          /*
          val st = st"""bool ${methodName}(const ${sel4TypeName} *data) {
                       |  ${queueName}_enqueue(${featureQueueName}, (${sel4TypeName}*) data);
                       |  ${featureNotificationName}_emit();
                       |  return true;
                       |}"""
          */
          val st = st"""bool ${methodName}(const ${sel4TypeName} *data) {
                       |  ${(enqueueEntries, "\n")}
                       |  return true;
                       |}"""
          
          val cInterface: Option[ST] = Some(interface)
          val cImplementation: Option[ST] = Some(st)
          val preInits: Option[ST] = None()
          val postInits: Option[ST]= Some(st"${(_postInits, "\n")}")
          val drainQueues: Option[(ST,ST)] = None()

          Some(C_SimpleContainer(cIncludes, cInterface, cImplementation, preInits, postInits, drainQueues))
            
        case x => halt(s"Unexpected direction: ${x}")
      }
      return ret
    }
    
    def handleEventData_TB_GlueCode_Profile(): Option[C_SimpleContainer] = {
      val (suffix, mod, paramType): (String, String, String) = feature.direction match {
        case ir.Direction.In => ("dequeue", "",
          Util.getSel4TypeName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get))
        case ir.Direction.Out => ("enqueue", "const ",
          Util.getSel4TypeName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get))
        case x => halt(s"Unexpected direction: ${x}")
      }
      val name = Util.genMonitorFeatureName(feature, None[Z]())

      val genMethodName = s"${name}_${suffix}"
      var inter = Some(st"""bool ${genMethodName}(${mod}${paramType} * ${name});""")
      val preInit: Option[ST] = None[ST]()
      var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()

      val impl: Option[ST] = if(suffix == "enqueue") {
        // OUT
        outConnections.get(Util.getName(feature.identifier)) match {
          case Some(conns) =>
            var accum: ISZ[ST] = ISZ()
            var i = 0
            val result = Util.brand("result")
            while(i < conns.size) {
              accum = accum :+ st"""${result} &= ${name}${i}_${suffix}((${paramType} *) ${name});"""
              i = i + 1
            }
            val methodName: String = s"${name}_${suffix}"

            Some(st"""/************************************************************************
                     | * ${methodName}:
                     | * Invoked from user code in the local thread.
                     | *
                     | * This is the function invoked by the local thread to make a
                     | * call to write to a remote event data port.
                     | *
                     | * XXX: When simulating fan out, the caller of this function will only
                     | * receive a positive response when all enqueues are successful. When a
                     | * negative response is received it only indicates that at least one
                     | * enqueue attempt failed.
                     | *
                     | ************************************************************************/
                     |bool ${methodName}(${mod}${paramType} * ${name}){
                     |  bool ${result} = true;
                     |  ${(accum, "\n")}
                     |  return ${result};
                     |}""")
          case _ => None[ST]()
        }
      } else if(!performHamrIntegration) {
        val userEntrypointName = Util.getUserEventEntrypointMethodName(component, feature)
        
        val invokeHandler: String = Util.getComputeEntrypointSourceText(feature.properties) match {
          case Some(v) =>
            val simpleName = Util.getLastName(feature.identifier)
            val varName = Util.brand(simpleName)
            drainQueue = Some((st"""${paramType} ${varName};""",
              st"""while (${genMethodName}((${paramType} *) &${varName})) {
                  |  ${userEntrypointName}(&${varName});
                  |}"""))

            inter = Some(st"""${inter.get}
                             |
                             |void ${v}(const ${paramType} * in_arg);""")

            s"${v}((${paramType} *) in_arg);"
          case _ => ""
        }

        if(Util.getComputeEntrypointSourceText(feature.properties).nonEmpty){
          Some(st"""/************************************************************************
                   | * ${userEntrypointName}:
                   | *
                   | * This is the function invoked by an active thread dispatcher to
                   | * call to a user-defined entrypoint function.  It sets up the dispatch
                   | * context for the user-defined entrypoint, then calls it.
                   | *
                   | ************************************************************************/
                   |void ${userEntrypointName}(const ${paramType} * in_arg) {
                   |  ${invokeHandler}
                   |}""")          
        } else {
          None[ST]()
        }
      } else {
        None()
      }

      val cIncludes: ISZ[ST] = ISZ()
      val postInits: Option[ST] = None()
      return Some(C_SimpleContainer(cIncludes, inter, impl, preInit, postInits, drainQueue))
    }
    
    def handleEventPort() : Option[C_SimpleContainer] = {
      
      def handleEventPort_TB_Profile(): Option[C_SimpleContainer] = {

        def useMonitor(): Option[C_SimpleContainer] = {
          val dir: String = feature.direction match {
            case ir.Direction.In => "read"
            case ir.Direction.Out => "write"
            case x => halt(s"Unexpected direction: ${x}")
          }
          
          val featureName = Util.getLastName(feature.identifier)
          val checkMethodName = Util.getEventPortSendReceiveMethodName(feature)

          var inter = Some(st"""bool ${checkMethodName}(void);""")
          var preInit: Option[ST] = None[ST]()
          var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()

          val impl: Option[ST] = if(dir == "read") {

            val varName = Util.brand(s"${featureName}_index")
            val callback = s"${Util.genSeL4NotificationName(feature, F)}_handler"
            val callback_reg = Util.brand(s"${featureName}_notification_reg_callback")

            val regCallback = st"CALLBACKOP(${callback_reg}(${callback}, NULL));"
            preInit = Some(regCallback)

            val interfaceName = Util.brand(s"${featureName}_get_events")

            var r = Some(st"""/************************************************************************
                             | *
                             | * Static variables and queue management functions for event port:
                             | *     ${featureName}
                             | *
                             | ************************************************************************/
                             |static int32_t ${varName} = 0;
                             |
                             |/************************************************************************
                             | * ${callback}:
                             | * Invoked by: seL4 notification callback
                             | *
                             | * This is the function invoked by an seL4 notification callback that  
                             | * dispatches the active-thread due to the arrival of an event on 
                             | * its ${featureName} event port
                             | *
                             | ************************************************************************/
                             |static void ${callback}(void *_ UNUSED){
                             |  MUTEXOP(${StringTemplate.SEM_POST}());
                             |  ${regCallback}
                             |}
                             |
                             |/************************************************************************
                             | * ${checkMethodName}:
                             | * Invoked from local active thread.
                             | *
                             | * This is the function invoked by the active thread to decrement the
                             | * input event index.
                             | *
                             | ************************************************************************/
                             |bool ${checkMethodName}(){
                             |  if(${varName} > 0) {
                             |    $varName--;
                             |    return true;
                             |  } else {
                             |    return false;
                             |  }
                             |}
                             |""")


            val simpleName = Util.getLastName(feature.identifier)
            val entrypointMethodName = s"${Util.brand("entrypoint")}${Util.brand("")}${Util.getClassifier(component.classifier.get)}_${simpleName}"

            val invokeHandler: String = Util.getComputeEntrypointSourceText(feature.properties) match {
              case Some(v) =>

                drainQueue = Some(
                  (st"",
                    st"""${varName} = ${interfaceName}();
                        |if(${varName} > 0){
                        |  ${entrypointMethodName}();
                        |}""")
                )

                inter = Some(st"""${inter.get}
                                 |
                                 |void ${v}(void);""")

                s"${v}();"
              case _ => ""
            }

            if(Util.getComputeEntrypointSourceText(feature.properties).nonEmpty) {
              Some(st"""${r.get}
                       |/************************************************************************
                       | *  ${entrypointMethodName}
                       | *
                       | * This is the function invoked by an active thread dispatcher to
                       | * call to a user-defined entrypoint function.  It sets up the dispatch
                       | * context for the user-defined entrypoint, then calls it.
                       | *
                       | ************************************************************************/
                       |void ${entrypointMethodName}(void){
                       |  ${invokeHandler}
                       |}""")              
            } else {
              r
            }
          } else {
            var accum = ISZ[ST]()
            val emit = Util.brand(s"${featureName}")

            outConnections.get(Util.getName(feature.identifier)) match {
              case Some(conns) =>
                var i = 0
                while (i < conns.size) {
                  accum = accum :+ st"""${emit}${i}_raise();"""
                  i = i + 1
                }
              case _ =>
            }

            Some(st"""/************************************************************************
                     | * ${checkMethodName}
                     | * Invoked from user code in the local thread.
                     | *
                     | * This is the function invoked by the local thread to make a
                     | * call to send to a remote event port.
                     | *
                     | ************************************************************************/
                     |bool ${checkMethodName}(void) {
                     |  ${(accum, "\n")}
                     |  return true;
                     |}
                     |""")
          }
          
          val cIncludes: ISZ[ST] = ISZ() 
          val postInits: Option[ST] = None()
          return Some(C_SimpleContainer(cIncludes, inter, impl, preInit, postInits, drainQueue))
        }

        def useNotification(): Option[C_SimpleContainer] = {
          val dir: String = feature.direction match {
            case ir.Direction.In => "read"
            case ir.Direction.Out => "write"
            case x => halt(s"Unexpected direction: ${x}")
          }
          val compName = Util.getClassifier(component.classifier.get)
          val methodName = Util.getLastName(feature.identifier)
          val checkMethodName = Util.getEventPortSendReceiveMethodName(feature)
          
          var inter = Some(st"""bool ${checkMethodName}(void);""")
          val preInit: Option[ST] = None[ST]()
          var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()

          val impl: Option[ST] = if(dir == "read") {
            val varName = Util.brand(s"${methodName}_index")
            val callback = s"${Util.genSeL4NotificationName(feature, F)}_handler"
            val callback_reg = Util.brand(s"${methodName}_reg_callback")

            var r = Some(st"""/************************************************************************
                             | *
                             | * Static variables and queue management functions for event port:
                             | *     ${methodName}
                             | *
                             | ************************************************************************/
                             |static bool ${varName} = false;
                             |
                             |/************************************************************************
                             | * ${callback}:
                             | * Invoked by: seL4 notification callback
                             | *
                             | * This is the function invoked by an seL4 notification callback to 
                             | * write to an active-thread input event port.  It increments a count 
                             | * of received events.
                             | *
                             | ************************************************************************/
                             |static void ${callback}(void *_ UNUSED){
                             |  $varName = true;
                             |  MUTEXOP(${StringTemplate.SEM_POST}());
                             |  CALLBACKOP(${callback_reg}(${callback}, NULL));
                             |}
                             |
                             |/************************************************************************
                             | * ${checkMethodName}:
                             | * Invoked from local active thread.
                             | *
                             | * This is the function invoked by the active thread to decrement the
                             | * input event index.
                             | *
                             | ************************************************************************/
                             |bool ${checkMethodName}(){
                             |  bool result;
                             |  result = ${varName};
                             |  ${varName} = false;
                             |  return result;
                             |}
                             |""")


            val simpleName = Util.getLastName(feature.identifier)
            val entrypointMethodName = s"${Util.brand("entrypoint")}${Util.brand("")}${Util.getClassifier(component.classifier.get)}_${simpleName}"

            val invokeHandler: String = Util.getComputeEntrypointSourceText(feature.properties) match {
              case Some(v) =>

                drainQueue = Some(
                  (st"",
                    st"""if(${checkMethodName}()){
                        |  ${entrypointMethodName}();
                        |}""")
                )

                inter = Some(st"""${inter.get}
                                 |
                                 |void ${v}(void);""")

                s"${v}();"
              case _ => ""
            }

            if(Util.getComputeEntrypointSourceText(feature.properties).nonEmpty) {
              Some(st"""${r.get}
                       |/************************************************************************
                       | *  ${entrypointMethodName}
                       | *
                       | * This is the function invoked by an active thread dispatcher to
                       | * call to a user-defined entrypoint function.  It sets up the dispatch
                       | * context for the user-defined entrypoint, then calls it.
                       | *
                       | ************************************************************************/
                       |void ${entrypointMethodName}(void){
                       |  ${invokeHandler}
                       |}""")  
            } else {
              r
            }

          } else {
            val emit = Util.brand(s"${methodName}_emit()")
            val resultVar = Util.brand("result")
            Some(st"""/************************************************************************
                     | * ${checkMethodName}
                     | * Invoked from user code in the local thread.
                     | *
                     | * This is the function invoked by the local thread to make a
                     | * call to write to a remote event port.
                     | *
                     | ************************************************************************/
                     |bool ${checkMethodName}(void) {
                     |  bool ${resultVar} = true;
                     |  ${emit};
                     |  return ${resultVar};
                     |}
                     |""")
          }
          
          val cIncludes: ISZ[ST] = ISZ() 
          val postInits: Option[ST] = None()
          return Some(C_SimpleContainer(cIncludes, inter, impl, preInit, postInits, drainQueue))
        }
        
        if(shouldUseMonitorForEventPort(feature)) {
          return useMonitor()
        } else {
          return useNotification()
        }
      }
      
      def handleEventPort_SB_Profile(): Option[C_SimpleContainer] = {
        val featureName = Util.getLastName(feature.identifier)
        val compName = Util.getClassifier(component.classifier.get)
        val counterName = Util.getEventSBCounterName(featureName)
                
        feature.direction match {
          case ir.Direction.In =>

            val receivedEventsVarName = Util.brand(s"${featureName}_received_events")
            val lastCounterVarName = Util.brand(s"${featureName}_last_counter")
            
            val globalVars = st"""/************************************************************************
                             | *
                             | * Static variables and queue management functions for event port:
                             | *     ${featureName}
                             | *
                             | ************************************************************************/
                             |static ${Util.SB_EVENT_COUNTER_TYPE} ${receivedEventsVarName} = 0;
                             |static ${Util.SB_EVENT_COUNTER_TYPE} ${lastCounterVarName} = 0;
                             |"""
            
            val callback = s"${Util.genSeL4NotificationName(feature, F)}_handler"
            val callback_reg = Util.brand(s"${featureName}_reg_callback")

            val callbackMethod = st"""/************************************************************************
                                | * ${callback}:
                                | * Invoked by: seL4 notification callback
                                | *
                                | * This is the function invoked by an seL4 notification callback to 
                                | * dispatch the component due to the arrival of an event on port
                                | * ${featureName}
                                | *
                                | ************************************************************************/
                                |static void ${callback}(void *_ UNUSED){
                                |  MUTEXOP(${StringTemplate.SEM_POST}());
                                |  CALLBACKOP(${callback_reg}(${callback}, NULL));
                                |}"""
            
            val preInit = Some(st"CALLBACKOP(${callback_reg}(${callback}, NULL));")

            val checkMethodName = Util.getEventPortSendReceiveMethodName(feature)

            var cHeaderEntries = Some(st"""bool ${checkMethodName}(void);""")
            
            val readMethod = st"""/************************************************************************
                                      | * ${checkMethodName}:
                                      | * Invoked from local active thread.
                                      | *
                                      | * This is the function invoked by the active thread to decrement the
                                      | * input event index.
                                      | *
                                      | ************************************************************************/
                                      |bool ${checkMethodName}() {
                                      |  if(${receivedEventsVarName} > 0) {
                                      |    ${receivedEventsVarName}--;
                                      |    return true;
                                      |  } else {
                                      |    return false;
                                      |  }
                                      |}"""
            
            var drainQueue: Option[(ST, ST)] =  None[(ST, ST)]()
            val entrypointMethodName = s"${Util.brand("entrypoint")}_${Util.getClassifier(component.classifier.get)}_${featureName}"
            
            val invokeHandler: String = Util.getComputeEntrypointSourceText(feature.properties) match {
              case Some(v) =>

                val currentCounter = s"current_${counterName}"
                
                val queueSize = Util.getQueueSize(feature)

                drainQueue = Some(
                  (st"",
                    st"""{ // drain queue for incoming event port '${featureName}'
                        |
                        |  ${Util.SB_EVENT_COUNTER_TYPE} ${currentCounter};
                        |
                        |  ${receivedEventsVarName} = 0; // drop any events not handled during last dispatch
                        |
                        |  // get current shared counter value
                        |  ${currentCounter} = *${counterName};
                        |
                        |  // Acquire memory fence - ensure preceding read occurs before any subsequent read or write
                        |  ${counterName}_acquire();
                        |
                        |  // NOTE: Counters can wrap, so we must use != below instead of >
                        |  while(${currentCounter} != ${lastCounterVarName}){
                        |    ${lastCounterVarName}++;
                        |    ${receivedEventsVarName}++;
                        |  }
                        |
                        |  if(${receivedEventsVarName} > 0) {
                        |  
                        |    // ${featureName}'s queue size is ${queueSize}
                        |    if(${receivedEventsVarName} > ${queueSize}) {
                        |      //printf("${compName}: dropping %i event(s) from incoming event port ${featureName}\n", (${receivedEventsVarName} - ${queueSize}));
                        |    
                        |      // drop events
                        |      ${receivedEventsVarName} = ${queueSize};
                        |    }
                        |
                        |    ${entrypointMethodName}();
                        |  }
                        |}""")
                )

                cHeaderEntries = Some(st"""${cHeaderEntries.get}
                                          |
                                          |void ${v}(void);""")

                s"${v}();"
                
              case _ => ""
            }

            val entryPointMethod: ST =
              if(Util.getComputeEntrypointSourceText(feature.properties).nonEmpty) {
                st"""
                    |/************************************************************************
                    | *  ${entrypointMethodName}
                    | *
                    | * This is the function invoked by an active thread dispatcher to
                    | * call to a user-defined entrypoint function.  It sets up the dispatch
                    | * context for the user-defined entrypoint, then calls it.
                    | *
                    | ************************************************************************/
                    |void ${entrypointMethodName}(void){
                    |  ${invokeHandler}
                    |}"""                
              } else {
                st""
              }
            
            val cImplementationEntries = Some(st"""${globalVars}
                                             |
                                             |${callbackMethod}
                                             |
                                             |${readMethod}
                                             |${entryPointMethod}
                                             |""")

            val cIncludes: ISZ[ST] = ISZ(Util.sbCounterInclude())
            val postInits: Option[ST] = None()
            return Some(C_SimpleContainer(cIncludes, cHeaderEntries, cImplementationEntries, preInit, postInits, drainQueue))
            
          case ir.Direction.Out =>

            val sendMethodName = Util.getEventPortSendReceiveMethodName(feature)
            val emit = Util.brand(s"${featureName}_emit()")

            var cHeaderEntries = st"bool ${sendMethodName}(void);"
            
            val sendMethod = st"""/************************************************************************
                     | * ${sendMethodName}
                     | * Invoked from user code in the local thread.
                     | *
                     | * This is the function invoked by the local thread to make a
                     | * call to send to a remote event port.
                     | *
                     | ************************************************************************/
                     |bool ${sendMethodName}(void) {
                     |  // ${counterName} is a dataport (shared memory) that is written by the sender 
                     |  // and read by the receiver(s). This counter is monotonicly increasing, 
                     |  // but can wrap.
                     |  (*${counterName})++;
                     |  
                     |  // Release memory fence - ensure subsequent write occurs after any preceeding read or write
                     |  ${counterName}_release();
                     |  
                     |  ${emit};
                     |  
                     |  return true;
                     |}
                     |"""
            
            val cIncludes: ISZ[ST] = ISZ(Util.sbCounterInclude())
            return Some(C_SimpleContainer(
              cIncludes = cIncludes, 
              cInterface = Some(cHeaderEntries), 
              cImplementation = Some(sendMethod), 
              preInits = None(), 
              postInits = None(),
              drainQueues = None()))
            
          case _ => halt(s"Unexpected direction ${feature.direction}")
        }
      }
      
      val ret: Option[C_SimpleContainer] = platform match {
        case ActPlatform.SeL4 => handleEventPort_TB_Profile()
        case ActPlatform.SeL4_TB => handleEventPort_TB_Profile()
        case ActPlatform.SeL4_Only => handleEventPort_SB_Profile()
      }
      return ret
    }
    
    val ret: Option[C_SimpleContainer] = feature.category match {
      case ir.FeatureCategory.DataPort => handleDataPort()
      case ir.FeatureCategory.EventPort => handleEventPort()
      case ir.FeatureCategory.EventDataPort =>
        platform match {
          case ActPlatform.SeL4_TB => handleEventData_TB_GlueCode_Profile()  
          case ActPlatform.SeL4 => handleEventData_TB_GlueCode_Profile()
          case ActPlatform.SeL4_Only => handleEventData_SB_GlueCode_Profile()
        }
        
      case _ => None[C_SimpleContainer]()
    }
    return ret
  }

  def genComponentTypeInterfaceFile(component: ir.Component, sts: ISZ[ST]): Resource = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeHeaderFileName = s"${Util.GEN_ARTIFACT_PREFIX}_${name}"
    val macroName = s"__${Util.GEN_ARTIFACT_PREFIX}_AADL_${name}_types__H"

    val ret = st"""#ifndef $macroName
                  |#define $macroName
                  |
                  |#include ${getTypeHeaderFileForInclude()}
                  |
                  |${(sts, "\n\n")}
                  |
                  |#endif // $macroName
                  |"""

    return Util.createResource(s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_INCLUDES}/${compTypeHeaderFileName}.h", ret, T)
  }

  def genComponentTypeImplementationFile(component:ir.Component, 
                                         localCIncludes: ISZ[ST], blocks: ISZ[ST], preInits: ISZ[ST], postInits: ISZ[ST],
                                         cRunPreEntries: ISZ[ST], cDrainQueues: ISZ[ST]): Resource = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeFileName = Util.brand(name)
    val ret: ST =  StringTemplate.componentTypeImpl(compTypeFileName, localCIncludes, blocks, preInits, postInits,
      cRunPreEntries, cDrainQueues, Util.isSporadic(component))

    return Util.createResource(s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_SRC}/${compTypeFileName}.c", ret, T)
  }

  def buildComponentMap(c: ir.Component): Unit = {
    val name = Util.getName(c.identifier)
    assert(!componentMap.contains(name))
    componentMap = componentMap + (name ~> c)
    if(c.classifier.nonEmpty) {
      classifierMap = classifierMap + (c.classifier.get.name ~> c)
    }
    for(f <- c.features){
      featureMap = featureMap + (Util.getName(f.identifier) ~> f)

      f match {
        case fe: ir.FeatureEnd =>
          if (Util.isDataPort(fe) && fe.classifier.isEmpty) {
            reporter.warn(None(), Util.toolName, s"Data type missing for feature ${fe.category} ${Util.getName(fe.identifier)}")
          }
        case _ =>
      }
    }
    for (sc <- c.subComponents) {
      buildComponentMap(sc)
    }
    hasPeriodicComponents = hasPeriodicComponents | Util.isPeriodic(c)
  }

  def resolve(sys : ir.Component): B = {

    for(c <- componentMap.values){


      for(ci <- c.connectionInstances){
        if(isHandledConnection(ci)) {
          def add(portPath: String, isIn: B): Unit = {
            val map: HashMap[String, ISZ[ir.ConnectionInstance]] = isIn match {
              case T => inConnections
              case F => outConnections
            }
            var cis: ISZ[ir.ConnectionInstance] = map.get(portPath) match {
              case Some(x) => x
              case _ => ISZ()
            }
            cis = cis :+ ci
            isIn match {
              case T => inConnections = inConnections + (portPath ~> cis)
              case F => outConnections = outConnections + (portPath ~> cis)
            }
          }
          val portNames = getPortConnectionNames(ci)
          add(portNames._1, F)
          add(portNames._2, T)
        } else {
          val src = componentMap.get(Util.getName(ci.src.component)).get
          val dst = componentMap.get(Util.getName(ci.dst.component)).get
          
          val srcName: String = if(ci.src.feature.nonEmpty) {
            s"${Util.getLastName(src.identifier)}.${Util.getLastName(ci.src.feature.get)}"
          } else {
            Util.getLastName(src.identifier)
          }
          
          val dstName: String = if(ci.dst.feature.nonEmpty) {
            s"${Util.getLastName(dst.identifier)}.${Util.getLastName(ci.dst.feature.get)}"
          } else {
            Util.getLastName(dst.identifier)
          }
          reporter.info(None(), Util.toolName, s"Ignoring ${src.category} to ${dst.category} connection: ${srcName} -> ${dstName}")
        }
      }
    }

    def findProcessor(): Option[ir.Component] = {
      var processes:ISZ[ir.Component] = ISZ()
      for (c <- componentMap.values if (c.category == ir.ComponentCategory.Process) && Util.getTypeHeaderFileName(c).nonEmpty) {
        processes = processes :+ c
      }
      if(processes.size > 0) {
        val candidates = processes.filter(f => ISZOps(f.subComponents).exists(p => p.category == ir.ComponentCategory.Thread))
        if(candidates.isEmpty) {
          reporter.warn(None(), Util.toolName, "None of the bound processes contain subcomponents")
          Some(processes(z"0"))
        } else if (candidates.size > 1) {
          reporter.error(None(), Util.toolName, s"${candidates.size} bound processes discovered, each containing thread subcomponents.")
          None[ir.Component]()
        } else {
          Some(candidates(z"0"))
        }
      } else {
        None[ir.Component]()
      }
    }

    findProcessor() match {
      case Some(p) =>
        topLevelProcess = Some(p)
        typeHeaderFileName = Util.getTypeHeaderFileName(p).get
      case _ =>
        reporter.error(None(), Util.toolName, "No processor bound process defined in the AADL model")
    }

    if(!hasErrors()) {
      buildMonitors()
    }
    
    if(!hasErrors()){
      buildSBQueues()  
    }
    
    if(!hasErrors()) {
      buildSamplingPortInterfaces()
    }
    
    return !hasErrors()
  }


  def resolveSharedDataFeatures(conns: ISZ[ir.ConnectionInstance]): Unit = {
    val dataConnections = conns.filter(f => f.kind == ir.ConnectionKind.Access).filter(
      f => featureMap.get(Util.getName(f.dst.feature.get)).get.category == ir.FeatureCategory.DataAccess)

    for(conn <- dataConnections) {
      val srcComp = componentMap.get(Util.getName(conn.src.component)).get

      if(srcComp.category != ir.ComponentCategory.Data) {
        reporter.error(None(), Util.toolName, s"${Util.getLastName(conn.src.component)} is not a data component")
      }

      val (dataKey, dstFeatureName) = getPortConnectionNames(conn)
      sharedData.get(dataKey) match {
        case Some(sd) =>
          val dstComp = componentMap.get(Util.getName(conn.dst.component)).get
          val ownerId = Util.getName(sd.owner.identifier)
          val dstId = Util.getName(dstComp.identifier)

          if(ownerId == dstId) {
            val _f = dstComp.features.filter(f => Util.getName(f.identifier) == Util.getName(conn.dst.feature.get))
            if(_f.size != 1) {
              reporter.error(None(), Util.toolName, s"There are ${_f.size} matching features for ${Util.getName(conn.dst.feature.get)}, expecting only 1.")
            } else if(!_f(0).isInstanceOf[ir.FeatureAccess]) {
              reporter.error(None(), Util.toolName, s"${Util.getName(conn.dst.feature.get)} is not a FeatureAccess.")
            } else {
              // add the owner's feature
              sharedData = sharedData + (dataKey ~> SharedData(sd.owner, Some(_f(0).asInstanceOf[ir.FeatureAccess]), sd.typ, sd.subcomponentId))
            }
          }
        case _ =>
          reporter.error(None(), Util.toolName, s"Could not find data subcomponent: ${dataKey}")
      }
    }
  }
  
  def shouldUseMonitorForEventPort(f: ir.Feature): B = {
    return T
    /*
    val ret: B = if(!f.isInstanceOf[ir.FeatureEnd]) {
      F
    }
    else {
      assert(platform == ActPlatform.SeL4 || platform == ActPlatform.SeL4_TB)
      assert(f.category == ir.FeatureCategory.EventPort)
      
      val fend = f.asInstanceOf[ir.FeatureEnd]
      
      fend.direction match {
        case ir.Direction.In =>
          Util.getQueueSize(f) > 1

        case ir.Direction.Out =>
          val name = Util.getName(f.identifier)
          val cons: ISZ[ir.ConnectionInstance] = outConnections.get(name).get

          var connectedToFeatureWithQueueGreaterThanOne = F
          for (ci <- cons) {
            val dstFeatureName: ir.Name = ci.dst.feature.get
            val dstName = Util.getName(dstFeatureName)
            val dstFeature = featureMap.get(dstName).get
            val qs: Z = Util.getQueueSize(dstFeature)

            if(qs > 1) {
              connectedToFeatureWithQueueGreaterThanOne = T
            }
          }

          connectedToFeatureWithQueueGreaterThanOne
        case _ =>
          halt(s"Unexpected direction ${fend.direction}")
      }
    }
    return ret
   */
  }

  def sortData(data: ISZ[ir.Component]): ISZ[ir.Component] = {
    def u(_c:ir.Component): String = { return Util.getClassifierFullyQualified(_c.classifier.get) }

    // build dependence graph so that required data types are processed first
    var graph: Graph[ir.Component, String] = Graph.empty
    for(d <- data){
      graph = graph * d
      if(TypeUtil.isRecordType(d)) {
        for (s <- d.subComponents) {
          val pair = (d, typeMap.get(u(s)).get)
          graph = graph + pair
        }
      } else if(TypeUtil.isArrayDef(d)) {
        val pair = (d, typeMap.get(TypeUtil.getArrayBaseType(d).get).get)
        graph = graph + pair
      } else {
        if(!(TypeUtil.isBaseType(d) || TypeUtil.isEnumDef(d) || TypeUtil.isMissingType(d))) {
          reporter.error(None(), Util.toolName, s"Unexpected data type ${d}")
        }
      }
    }

    /*
    def rx(r: String): String = { return StringUtil.replaceAll(r, "*", "star") }
    val nodes: ISZ[String] = graph.nodes.keys.map(c => s"${rx(u(c))}")
    val edges = graph.allEdges.map(e => st"${rx(u(e.source))} -> ${rx(u(e.dest))}")
    val g =st"""digraph {
               |  ${(nodes, ";\n")}
               |
               |  ${(edges, ";\n")}
               |}"""
    println(g.render)
    */

    var sorted: ISZ[ir.Component] = ISZ()
    def sortByClassifier(s: ISZ[ir.Component]): ISZ[ir.Component] = { return ISZOps(s).sortWith((a,b) => u(a) < u(b)) }
    def sortDependents(c : ir.Component): Unit = {
      if(ISZOps(sorted).contains(c)){ return }
      for (dependent <- sortByClassifier(graph.outgoing(c).map(m => m.dest))) {
        sortDependents(dependent)
      }
      sorted = sorted :+ c
    }
    for (r <- sortByClassifier(graph.nodes.keys.filter(k => graph.incoming(k).size == z"0"))) {
      sortDependents(r)
    }
    return sorted
  }
  
  def getPortConnectionNames(c: ir.ConnectionInstance): (String, String) = {
    val src = componentMap.get(Util.getName(c.src.component)).get
    val dst = componentMap.get(Util.getName(c.dst.component)).get

    val ret: (String, String) = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>
        (Util.getName(c.src.feature.get), Util.getName(c.dst.feature.get))
      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        (Util.getName(c.src.component), Util.getName(c.dst.feature.get))

      case _ => halt(s"Unexpected connection: ${c}")
    }
    return ret
  }
  
  def isHandledConnection(c: ir.ConnectionInstance): B = {

    def validFeature(f: ir.Feature): B = {
      var ret: B = f match {
        case fend: ir.FeatureEnd =>
          fend.category match {
            case ir.FeatureCategory.DataAccess => T
            case ir.FeatureCategory.DataPort => T
            case ir.FeatureCategory.EventPort => T
            case ir.FeatureCategory.EventDataPort => T
            case ir.FeatureCategory.SubprogramAccessGroup => T

            case ir.FeatureCategory.AbstractFeature => F
            case ir.FeatureCategory.BusAccess => F
            case ir.FeatureCategory.FeatureGroup => F
            case ir.FeatureCategory.Parameter => F
            case ir.FeatureCategory.SubprogramAccess => F
          }
        case faccess: ir.FeatureAccess =>
          faccess.accessCategory match {
            case ir.AccessCategory.Data => T
            case ir.AccessCategory.SubprogramGroup => T
            case _ => F
          }
        case _ => F
      }
      return ret
    }
    val src = componentMap.get(Util.getName(c.src.component)).get
    val dst = componentMap.get(Util.getName(c.dst.component)).get

    val ret : B = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>

        val srcFeature = featureMap.get(Util.getName(c.src.feature.get)).get
        val dstFeature = featureMap.get(Util.getName(c.dst.feature.get)).get

        validFeature(srcFeature) && validFeature(dstFeature)

      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        val dstFeature = featureMap.get(Util.getName(c.dst.feature.get)).get
        validFeature(dstFeature)

      case _ =>
        F
    }

    return ret
  }

}

