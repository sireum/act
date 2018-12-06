// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.ops.ISZOps
import org.sireum.aadl.{act, ir}
import org.sireum.aadl.act.ast._

@record class Gen() {

  var topLevelProcess: Option[ir.Component] = None[ir.Component]()
  var typeHeaderFileName: String = ""

  var componentMap: HashMap[String, ir.Component] = HashMap.empty
  var typeMap: HashMap[String, ir.Component] = HashMap.empty
  //var featureEndMap: HashMap[String, ir.FeatureEnd] = HashMap.empty
  var featureMap: HashMap[String, ir.Feature] = HashMap.empty
  var sharedData: HashMap[String, SharedData] = HashMap.empty

  // port-paths -> connInstances
  var inConnections: HashMap[String, ISZ[ir.ConnectionInstance]] = HashMap.empty
  var outConnections: HashMap[String, ISZ[ir.ConnectionInstance]] = HashMap.empty

  var connectors: ISZ[ast.Connector] = ISZ()
  var astObjects: ISZ[ASTObject] = ISZ()
  var monitors: HashSMap[String, Monitor] = HashSMap.empty // conn instname -> monitor
  var containers: ISZ[C_Container] = ISZ()
  var auxFiles: ISZ[Resource] = ISZ()

  var configuration: ISZ[ST] = ISZ()
  var auxCSources: ISZ[ST] = ISZ()

  var hasPeriodicComponents: B = F
  var hasErrors: B = F
  val preventBadging: B = T

  var count: Z = 0
  def counter(): Z = {
    count = count + 1
    return count - 1
  }

  def process(model : ir.Aadl, cSources: ISZ[String]) : Option[ActContainer] = {

    val components = model.components.withFilter(f => f.category != ir.ComponentCategory.Data)
    if(components.size != z"1" || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }
    val system = components(0)

    buildComponentMap(system)

    auxCSources = cSources.map(c => st"""#include "../../../${c}"""")

    val dataSubcomponents = componentMap.values.withFilter(f => f.category == ir.ComponentCategory.Data)
    for(d <- (model.dataComponents ++ dataSubcomponents)){ typeMap = typeMap + (Util.getClassifierFullyQualified(d.classifier.get) ~> d) }
    val sortedData = sortData(model.dataComponents)

    resolve(system)

    if(!hasErrors) {
      auxFiles = auxFiles :+ Resource(s"${Util.DIR_INCLUDES}/$typeHeaderFileName.h", processDataTypes(sortedData))
    }

    if(hasPeriodicComponents) {
      auxFiles = auxFiles :+ TimerUtil.timerInterface()
    }

    if(!hasErrors) {
      if(preventBadging) {
        Util.addWarning("Branding disabled")
      }
      gen(system)
    }

    if(!hasErrors) {
      return Some(ActContainer(Util.getLastName(topLevelProcess.get.identifier),
        connectors,
        astObjects,
        monitors.values,
        containers,
        auxFiles))

    } else {
      return None[ActContainer]()
    }
  }

  def gen(c: ir.Component) : Unit = {
    c.category match {
      case ir.ComponentCategory.System =>
        c.subComponents.foreach(sc => gen(sc))
      case ir.ComponentCategory.Process =>
        val g = genContainer(c)
        astObjects = astObjects :+ Assembly(st"""${(configuration, "\n")}""".render, g)
      case ir.ComponentCategory.Thread =>
        genThread(c)
      case _ =>
        c.subComponents.foreach(sc => gen(sc))
    }
  }

  def genContainer(c : ir.Component) : Composition = {
    assert(c.category == ir.ComponentCategory.Process)

    var connections: ISZ[Connection] = ISZ()
    var i: Z = 1

    def newConn(): String = {
      val ret = s"conn${i}"
      i = i + 1
      return ret
    }

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
            // connect component to time server
            createConnection(Sel4ConnectorTypes.seL4TimeServer,
              componentId, TimerUtil.TIMER_ID,
              TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_TIMER_ID)

            // connect dispatcher to component
            createConnection(Sel4ConnectorTypes.seL4Notification,
              TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.componentNotificationName(componentId),
              componentId, TimerUtil.TIMER_NOTIFICATION_ID)

            dispatchNotifications = dispatchNotifications :+ Emits(
              name = TimerUtil.componentNotificationName(componentId),
              typ = Util.NOTIFICATION_TYPE)

            val period: Z = if(Util.getPeriod(sc).isEmpty) {
              Util.addWarning(s"Period not provided for ${classifier}, using ${Util.DEFAULT_PERIOD}")
              Util.DEFAULT_PERIOD
            } else {
              Util.getPeriod(sc).get
            }
            calendars = calendars :+ TimerUtil.calendar(componentId, period)

            configuration = configuration :+ TimerUtil.configurationTimerAttribute(componentId, counter(), F)
            configuration = configuration :+ TimerUtil.configurationTimerGlobalEndpoint(componentId, classifier, TimerUtil.TIMER_ID)

            val priority: Z = if(Util.getPriority(sc).isEmpty){
              Util.addWarning(s"Priority not provided for ${classifier}, using ${Util.DEFAULT_PRIORITY}")
              Util.DEFAULT_PRIORITY
            } else {
              Util.getPriority(sc).get
            }
            configuration = configuration :+ StringTemplate.configurationPriority(componentId, priority)

            val stackSize: Z = if(Util.getStackSize(sc).isEmpty) {
              Util.addWarning(s"Stack Size not provided for ${classifier}, using ${Util.DEFAULT_STACK_SIZE}")
              Util.DEFAULT_STACK_SIZE
            } else {
              Util.getStackSize(sc).get
            }
            configuration = configuration :+ StringTemplate.configurationStackSize(componentId, stackSize)
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
          if(sc.features.nonEmpty) { halt(s"Need to handle subprogram group features: ${sc}")}
          if(sc.subComponents.nonEmpty) { halt(s"Need to handle subprogram group subComponents: ${sc}")}

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(), includes = ISZ())

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
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(readMethod, writeMethod), includes = ISZ(s"<${typeHeaderFileName}.h>"))

          Util.getCamkesOwnerThread(sc.properties) match {
            case Some(owner) =>
              val threads = componentMap.values.withFilter(f => f.category == ir.ComponentCategory.Thread)
              val thread = threads.withFilter(f => f.classifier.get.name == owner || Util.getClassifier(f.classifier.get) == owner)

              if(thread.nonEmpty) {
                val theOwner = thread(0)
                if(thread.size > 1) {
                  Util.addWarning(s"Found multiple matches for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property: ${owner}")
                }
                if(Util.getClassifier(theOwner.classifier.get) == owner){
                  Util.addWarning(s"Fully qualified name '${theOwner.classifier.get.name}' should be used for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property")
                }

                val subcomponentId = Util.getLastName(sc.identifier)
                sharedData = sharedData +
                  (Util.getName(sc.identifier) ~> SharedData(theOwner, None[ir.FeatureAccess](), sc.classifier.get, subcomponentId))

              } else {
                addError(st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread}:  Could not locate component '${owner}'.  Please use one of the following:
                             |  ${(threads.map(m => m.classifier.get.name), "\n")}""".render)
              }
            case _ =>
          }

        case _ =>
          halt(s"Not handling: subcomponent of type '${sc.category}'.  ${Util.getName(sc.identifier)}")
      }
    }

    if(hasPeriodicComponents) { // add the periodic dispatcher component

      connectors = connectors :+ ast.Connector(name = "seL4TimeServer",
        from_type = ConnectorType.Procedures, from_template = Some("seL4TimeServer-from.template.c"), from_threads = 0, from_hardware = F,
        to_type = ConnectorType.Procedure, to_template = Some("seL4TimeServer-to.template.c"), to_threads = 0, to_hardware = F)

      connectors = connectors :+ ast.Connector(name = "seL4GlobalAsynchCallback",
        from_type = ConnectorType.Events, from_template = Some("seL4GlobalAsynchCallback-from.template.c"), from_threads = 0, from_hardware = F,
        to_type = ConnectorType.Event, to_template = Some("seL4GlobalAsynchCallback-to.template.c"), to_threads = 0, to_hardware = F)

      // add placeholder time server
      val timerComponent = TimerUtil.timerComponent()
      instances = instances :+ timerComponent
      containers = containers :+ C_Container(timerComponent.component.name, ISZ(TimerUtil.timerCSource()), ISZ(), ISZ())

      // add the dispatcher component
      val dispatchComponent = TimerUtil.dispatchComponent(dispatchNotifications)
      instances = instances :+ dispatchComponent
      containers = containers :+ C_Container(dispatchComponent.component.name,
        ISZ(TimerUtil.dispatchComponentCSource(s"<${typeHeaderFileName}.h>", calendars)), ISZ(), ISZ())

      // connect dispatch timer to time server
      createConnection(Sel4ConnectorTypes.seL4TimeServer,
        TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.TIMER_ID_DISPATCHER,
        TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_TIMER_ID)

      createConnection(Sel4ConnectorTypes.seL4GlobalAsynchCallback,
        TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_NOTIFICATION_ID,
        TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.TIMER_NOTIFICATION_DISPATCHER_ID)

      configuration = configuration :+ TimerUtil.configurationTimerAttribute(dispatchComponent.name, counter(), T)

      // FIXME: TB uses "periodic_dispatcher" rather than the assigned classifier "dispatch_periodic"???
      configuration = configuration :+ TimerUtil.configurationTimerGlobalEndpoint(dispatchComponent.name, dispatchComponent.component.name, TimerUtil.TIMER_ID_DISPATCHER)
      configuration = configuration :+ TimerUtil.configurationTimerGlobalEndpoint(dispatchComponent.name, dispatchComponent.component.name, TimerUtil.TIMER_NOTIFICATION_DISPATCHER_ID)

      // FIXME: is there a default priority for dispatch component?
      configuration = configuration :+ StringTemplate.configurationPriority(dispatchComponent.name, Util.DEFAULT_PRIORITY)
    }

    def createConnection(connectionType: Sel4ConnectorTypes.Type,
                         srcComponent: String, srcFeature: String,
                         dstComponent: String, dstFeature: String): Unit = {
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

      connections = connections :+ con
    }

    def createNotificationConnection(srcComponent: String, srcFeature: String,
                                     dstComponent: String, dstFeature: String): Unit = {
      createConnection(Sel4ConnectorTypes.seL4Notification, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createRPCConnection(srcComponent: String, srcFeature: String,
                            dstComponent: String, dstFeature: String) : Unit = {
      createConnection(Sel4ConnectorTypes.seL4RPCCall, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createDataConnection(conn: ir.ConnectionInstance) : Unit = {
      val monitor = getMonitorForConnectionInstance(conn).get

      val srcComponent = Util.getLastName(conn.src.component)
      val dstComponent = Util.getLastName(conn.dst.component)

      //val srcFeature = featureEndMap.get(Util.getName(conn.src.feature.get)).get
      //val dstFeature = featureEndMap.get(Util.getName(conn.dst.feature.get)).get
      val srcFeature = featureMap.get(Util.getName(conn.src.feature.get)).get
      val dstFeature = featureMap.get(Util.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
      val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

      // rpc src to mon
      createRPCConnection(srcComponent, srcFeatureName, monitor.i.name, "mon")

      // rpc mon to dst
      createRPCConnection(dstComponent, dstFeatureName, monitor.i.name, "mon")

      // notification monsig to dst
      createNotificationConnection(
        monitor.i.name, "monsig",
        dstComponent, Util.genMonitorNotificationFeatureName(dstFeature)
      )
    }

    resolveSharedDataFeatures(c.connectionInstances)
    val missingFeatures = sharedData.values.withFilter(f => f.ownerFeature == None[ir.FeatureAccess])
    if(missingFeatures.nonEmpty) {
      addError(s"Could not find the owner for the following data subcomponents: ${(missingFeatures.map(f => f.subcomponentId), ", ")}")
    }

    for(conn <- c.connectionInstances) {
      val fdst = featureMap.get(Util.getName(conn.dst.feature.get)).get

      val srcComponent = Util.getLastName(conn.src.component)
      val srcFeature = Util.getLastName(conn.src.feature.get)
      val dstComponent = Util.getLastName(conn.dst.component)
      val dstFeature = Util.getLastName(conn.dst.feature.get)

      conn.kind match {
        case ir.ConnectionKind.Port =>
          fdst.category match {
            case ir.FeatureCategory.DataPort =>
              createDataConnection(conn)
            case ir.FeatureCategory.EventDataPort =>
              createDataConnection(conn)

            case ir.FeatureCategory.EventPort =>
              createNotificationConnection(srcComponent, srcFeature, dstComponent, dstFeature)
            case _ => halt (s"not expecting ${fdst.category}")
          }
        case ir.ConnectionKind.Access =>
          fdst.category match {
            case ir.FeatureCategory.SubprogramAccess =>
              createRPCConnection(srcComponent, srcFeature, dstComponent, dstFeature)
            case ir.FeatureCategory.SubprogramAccessGroup =>
              createRPCConnection(srcComponent, srcFeature, dstComponent, dstFeature)

            case ir.FeatureCategory.DataAccess =>
              val sd = sharedData.get(Util.getName(conn.src.feature.get)).get
              val dstComp = componentMap.get(Util.getName(conn.dst.component)).get
              val ownerId = Util.getName(sd.owner.identifier)
              val dstId = Util.getName(dstComp.identifier)

              if(ownerId != dstId) {
                 createConnection(Sel4ConnectorTypes.seL4SharedData,
                   dstComponent, dstFeature,
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

    val monInstances = monitors.values.map((m: Monitor) => m.i)
    return Composition(
      groups = ISZ(),
      exports = ISZ(),
      instances = instances ++ monInstances,
      connections = connections
    )
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

    var imports : Set[String] = Set.empty

    var cIncludes: ISZ[ST] = ISZ()
    var cImpls: ISZ[ST] = ISZ()
    var cPreInits: ISZ[ST] = ISZ()
    var cDrainQueues: ISZ[(ST, ST)] = ISZ()

    for(f <- c.features.withFilter(_f => _f.isInstanceOf[ir.FeatureAccess])) {
      def handleSubprogramAccess(): Unit = {
        val fend = f.asInstanceOf[ir.FeatureAccess]
        val fpath = Util.getName(fend.identifier)
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
        val fpath = Util.getName(fend.identifier)
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
          Console.err.println(s"Not expecting AccessType: ${Util.getName(f.identifier)}")
      }
    }

    for(f <- c.features.withFilter(_f => _f.isInstanceOf[ir.FeatureEnd])) {
      val fend = f.asInstanceOf[ir.FeatureEnd]
      val fpath = Util.getName(fend.identifier)
      val fid = Util.getLastName(f.identifier)

      def handleDataPort(): Unit = {
        fend.direction match {
          case ir.Direction.In =>
            // uses monitor
            // consumes notification
            if(inConnections.get(fpath).nonEmpty) {
              val monitor = getMonitorForInPort(fend).get
              imports = imports + Util.getInterfaceFilename(monitor.interface.name)

              uses = uses :+ Uses(
                name = Util.genMonitorFeatureName(fend, None[Z]()),
                typ = monitor.interface.name,
                optional = F)

              consumes = consumes :+ Consumes(
                name = Util.genMonitorNotificationFeatureName(fend),
                typ = Util.getMonitorNotificationType(fend.category),
                optional = F)
            }
          case ir.Direction.Out =>
            // uses monitor
            outConnections.get(fpath) match {
              case Some(outs) =>
                var i: Z = 0
                for(o <- outs) {
                  val monitor = monitors.get(Util.getName(o.name)).get
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

      def handleEventPort(): Unit = {
        fend.direction match {
          case ir.Direction.In =>
            Util.getComputeEntrypointSourceText(fend.properties) match {
              case Some(v) =>
                val name = Util.genMonitorNotificationFeatureName(fend)
                val handlerName = s"${name}_handler"
                val regCallback = s"${name}_reg_callback"

                cImpls = cImpls :+ StringTemplate.cEventNotificiationHandler(handlerName, regCallback)
                cPreInits = cPreInits :+ StringTemplate.cRegCallback(handlerName, regCallback)
              case _ =>
                Util.addWarning(s"port: ${fid} in thread: ${cid} does not have a compute entrypoint and will not be dispatched.")
            }

          case _ =>
        }
      }

      f.category match {
        case ir.FeatureCategory.DataPort =>
          handleDataPort()
        case ir.FeatureCategory.EventDataPort =>
          handleDataPort()
          handleEventPort()

        case ir.FeatureCategory.EventPort =>
          handleEventPort()

          fend.direction match {
            case ir.Direction.In =>
              // consumes notification
              consumes = consumes :+ Consumes(
                name = fid,
                typ = Util.NOTIFICATION_TYPE,
                optional = F)
            case ir.Direction.Out =>
              // emits notification
              emits = emits :+ Emits(
                name = fid,
                typ = Util.NOTIFICATION_TYPE)
            case _ => halt(s"${fpath}: not expecting direction ${fend.direction}")
          }

        case _ =>
          Util.addWarning(s"Skipping ${f.category} for ${fid}.${fid}")
      }

      generateC_InterfaceMethod(c, f.asInstanceOf[ir.FeatureEnd]) match {
        case Some(C_SimpleContainer(inter, impl, preInit, drainQueues)) =>
          if(inter.nonEmpty) { cIncludes = cIncludes :+ inter.get }
          if(impl.nonEmpty) { cImpls = cImpls :+ impl.get }
          if(preInit.nonEmpty) { cPreInits = cPreInits :+ preInit.get}
          if(drainQueues.nonEmpty) { cDrainQueues = cDrainQueues :+ drainQueues.get}
        case _ =>
      }
    }

    var binarySemaphores: ISZ[BinarySemaphore] = ISZ()
    val binSem = Util.getDiscreetPropertyValue(c.properties, "camkes::Binary_Semaphore")
    binSem match {
      case Some(v: ir.ValueProp) =>
        binarySemaphores = binarySemaphores :+ BinarySemaphore(v.value)
      case _ =>
    }

    // has semaphore tb_dispatch_sem
    binarySemaphores = binarySemaphores :+ BinarySemaphore(TimerUtil.SEM_DISPATCH)

    Util.getDispatchProtocol(c) match {
      case Some(Dispatch_Protocol.Periodic) =>
        // import Timer.idl4
        imports = imports + Util.getInterfaceFilename(TimerUtil.TIMER_TYPE)

        // uses Timer tb_timer;
        uses = uses :+ Uses(
          name = TimerUtil.TIMER_ID,
          typ = TimerUtil.TIMER_TYPE,
          optional = F)

        // consumes Notification tb_timer_complete
        consumes = consumes :+ Consumes(
          name = TimerUtil.TIMER_NOTIFICATION_ID,
          typ = Util.NOTIFICATION_TYPE,
          optional = F)

        cIncludes = cIncludes :+ st"""
                                     |// user entrypoints for periodic dispatch
                                     |void component_entry(const int64_t * periodic_dispatcher);
                                     |void component_init(const int64_t *arg);"""

      case Some(Dispatch_Protocol.Sporadic) =>

      case _ =>
        Util.addWarning(s"Dispatch Protocol not specified for ${Util.getLastName(c.identifier)}")
    }

    var cRunEntries: ISZ[ST] = cDrainQueues.map(x => x._1)

    Util.getInitializeEntryPoint(c.properties) match {
      case Some(methodName) =>
        cIncludes = cIncludes :+ st"""void ${methodName}(const int64_t *arg);"""
        val (cimpl, runEntry) = StringTemplate.componentInitializeEntryPoint(cid, methodName)
        cImpls = cImpls :+ cimpl
        cRunEntries = cRunEntries :+ runEntry
      case _ =>
    }

    containers = containers :+ C_Container(cid,
      ISZ(genComponentTypeImplementationFile(c, cImpls, cPreInits, cRunEntries, cDrainQueues.map(x => x._2))),
      ISZ(genComponentTypeInterfaceFile(c, cIncludes)),
      Util.getSourceText(c.properties)
    )

    return Component(
      control = uses.nonEmpty,
      hardware = F,
      name = cid,

      mutexes = ISZ(),
      binarySemaphores = binarySemaphores,
      semaphores = ISZ(),

      dataports = dataports,
      emits = emits,
      uses = uses,
      consumes = consumes,
      provides = provides,
      includes = ISZ(s"<${typeHeaderFileName}.h>"),
      attributes = ISZ(),

      imports = imports.elements
    )
  }

  def buildMonitors(): Unit = {
    for (portPath <- outConnections.keys.withFilter(p => outConnections.get(p).get.size > 0)) {
      var i: Z = 0
      for (connInst <- outConnections.get(portPath).get()) {

        val dst: ir.Component = componentMap.get(Util.getName(connInst.dst.component)).get

        def handleDataPort(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceName = Util.getInterfaceName(f)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

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
            provides = ISZ(Provides(name = "mon", typ = interfaceName)),
            includes = ISZ(),
            attributes = ISZ(),
            imports = ISZ(st""""../../../${Util.DIR_INTERFACES}/${interfaceName}.idl4"""".render)
          )

          val inst: Instance = Instance(address_space = "", name = StringUtil.toLowerCase(monitorName), component = monitor)

          val paramType = typeMap.get(typeName).get
          val paramTypeName = Util.getMonitorWriterParamName(paramType)

          val methods: ISZ[Method] = f.category match {
            case ir.FeatureCategory.EventDataPort => createQueueMethods(paramTypeName)
            case ir.FeatureCategory.DataPort => createReadWriteMethods(paramTypeName)
            case _ =>
              halt(s"not expecting ${f.category}")
          }

          val interface: Procedure = Procedure(
            name = interfaceName,
            methods = methods,
            includes = ISZ(s"<${typeHeaderFileName}.h>")
          )

          val writer: Procedure = Procedure (
            name = Util.getMonitorWriterName(f),
            methods = ISZ(Method(
              name = s"write_${typeName}",
              parameters = ISZ(Parameter(F, Direction.Refin, "arg", paramTypeName)),
              returnType = Some("bool")
            )),
            includes = ISZ(s"<${typeHeaderFileName}.h>")
          )

          val connInstName = Util.getName(connInst.name)


          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_SRC}/${monitorName}.c"
          val cimplementation: Resource =
            if(f.category == ir.FeatureCategory.DataPort) {
              Resource(implName, StringTemplate.tbMonReadWrite(paramTypeName, Util.getQueueSize(f), monitorName, typeHeaderFileName, preventBadging))
            } else {
              Resource(implName, StringTemplate.tbEnqueueDequeue(paramTypeName, Util.getQueueSize(f), monitorName, typeHeaderFileName, preventBadging))
            }

          val interName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_INCLUDES}/${monitorName}.h"
          val cincludes = Resource(interName, StringTemplate.tbInterface(s"__${monitorName}_H__"))

          monitors = monitors + (connInstName ~> Monitor(inst, interface, writer, cimplementation, cincludes, i, connInst))
        }

        val dstFeature: ir.Feature = featureMap.get(Util.getName(connInst.dst.feature.get)).get

        connInst.kind match {
          case ir.ConnectionKind.Port =>
            dstFeature.category match {
              case ir.FeatureCategory.DataPort =>
                handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
              case ir.FeatureCategory.EventDataPort =>
                handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
              case ir.FeatureCategory.EventPort =>
                // will use Notification
              case _ =>
                halt(s"not expecting ${dst}")
            }
          case _ =>
            Util.addWarning(s"processInConnections: Not handling ${connInst}")
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
    val defs: ISZ[ST] = values.withFilter(v => TypeUtil.translateBaseType(v.classifier.get.name).isEmpty).map(v => processDataType(v, F))
    val macroname = s"__TB_AADL_${typeHeaderFileName}__H"
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
              addError(s"The attribute '${fname}' from data type ${name} is a reserved keyword")
            }
            st"${processDataType(sub, T)} ${fname};"
          })

          st"""typedef
              |  struct ${name} {
              |    ${(fields, "\n")}
              |  } ${name};"""
        }
      } else if (TypeUtil.isBaseType(c.classifier.get.name)) {
        st"${TypeUtil.translateBaseType(c.classifier.get.name)}"
      } else if (isField) {
        st"${Util.getClassifierFullyQualified(c.classifier.get)}"
      } else if (TypeUtil.isArrayDef(c)) {
        // TODO multidim arrays
        val name = Util.getClassifierFullyQualified(c.classifier.get)
        val container = Util.getContainerName(name)
        st"""typedef ${TypeUtil.getArrayBaseType(c)} ${name} [${TypeUtil.getArrayDimension(c)}];
            |
            |typedef
            |  struct ${container} {
            |    ${name} f;
            |  } ${container};"""
      } else {
        addError(s"Unexpected datatype: ${c}")
        st" "
      }


    return s
  }

  def generateC_InterfaceMethod(component: ir.Component, feature: ir.FeatureEnd): Option[C_SimpleContainer] = {
    val ret: Option[C_SimpleContainer] = feature.category match {
      case ir.FeatureCategory.DataPort =>
        val (suffix, mod) : (String, String) = feature.direction match {
          case ir.Direction.In => ("read", "")
          case ir.Direction.Out => ("write", "const ")
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(feature, None[Z]())
        val paramType = Util.getMonitorWriterParamName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get)

        val inter = Some(st"""bool ${name}_${suffix}(${mod}${paramType} * ${name});""")
        val impl: Option[ST] =
          if(suffix == "write") {
            outConnections.get(Util.getName(feature.identifier)) match {
              case Some(conns) =>
                var accum: ISZ[ST] = ISZ()
                var i = 0
                while (i < conns.size) {
                  accum = accum :+ st"""tb_result &= ${name}${i}_${suffix}((${paramType} *) ${name});"""
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
                    |  bool tb_result = true;
                    |  ${(accum, "\n")}
                    |  return tb_result;
                    |}""")
              case _ => None[ST]()
            }
          } else {
            None[ST]()
          }
        Some(C_SimpleContainer(inter, impl, None[ST](), None[(ST, ST)]()))

      case ir.FeatureCategory.EventDataPort =>
        val (suffix, mod, paramType): (String, String, String) = feature.direction match {
          case ir.Direction.In => ("dequeue", "", Util.getMonitorWriterParamName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get))
          case ir.Direction.Out => ("enqueue", "const ", Util.getClassifierFullyQualified(feature.classifier.get))
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(feature, None[Z]())

        val genMethodName = s"${name}_${suffix}"
        var inter = Some(st"""bool ${genMethodName}(${mod}${paramType} * ${name});""")
        var preInit: Option[ST] = None[ST]()
        var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()

        val impl: Option[ST] = if(suffix == "enqueue") {
          // OUT
          outConnections.get(Util.getName(feature.identifier)) match {
            case Some(conns) =>
              var accum: ISZ[ST] = ISZ()
              var i = 0
              while(i < conns.size) {
                accum = accum :+ st"""tb_result &= ${name}${i}_${suffix}((${paramType} *) ${name});"""
                i = i + 1
              }
              val methodName: String = s"${name}_${suffix}"

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
                  |  bool tb_result = true;
                  |  ${(accum, "\n")}
                  |  return tb_result;
                  |}""")
            case _ => None[ST]()
          }
        } else {
          val simpleName = Util.getLastName(feature.identifier)
          val methodName = s"tb_entrypoint_tb_${Util.getClassifier(component.classifier.get)}_${simpleName}"

          val invokeHandler: String = Util.getComputeEntrypointSourceText(feature.properties) match {
            case Some(v) =>
              val varName = s"tb_${simpleName}";
              drainQueue = Some((st"""${paramType} ${varName};""",
                st"""while (${genMethodName}((${paramType} *) &${varName})) {
                    |  ${methodName}(&${varName});
                    |}"""))

              inter = Some(st"""${inter.get}
                               |
                               |void ${v}(const ${paramType} * in_arg);""")

              s"${v}((${paramType} *) in_arg);"
            case _ => ""
          }

          Some(st"""/************************************************************************
              | * ${methodName}:
              | *
              | * This is the function invoked by an active thread dispatcher to
              | * call to a user-defined entrypoint function.  It sets up the dispatch
              | * context for the user-defined entrypoint, then calls it.
              | *
              | ************************************************************************/
              |void ${methodName}(const ${paramType} * in_arg) {
              |  ${invokeHandler}
              |}""")
        }
        Some(C_SimpleContainer(inter, impl, preInit, drainQueue))

      case ir.FeatureCategory.EventPort =>
        val dir: String = feature.direction match {
          case ir.Direction.In => "read"
          case ir.Direction.Out => "write"
          case x => halt(s"Unexpected direction: ${x}")
        }
        val compName = Util.getClassifier(component.classifier.get)
        val methodName = Util.getLastName(feature.identifier)
        val name = s"${Util.GEN_ARTIFACT_PREFIX}_${compName}_${dir}_${methodName}"

        val inter = Some(st"""bool ${name}(void);""")

        val impl: Option[ST] = if(dir == "read") {
          val varName = s"${methodName}_index"
          val callback = s"${methodName}_callback"
          val callback_reg = s"${methodName}_reg_callback"

          Some(st"""/************************************************************************
              | *
              | * Static variables and queue management functions for event port:
              | *     ${methodName}
              | *
              | ************************************************************************/
              |static bool ${varName} = false;
              |
              |/************************************************************************
              | * ${callback}:
              | * Invoked by: remote RPC
              | *
              | * This is the function invoked by a remote RPC to write to an active-thread
              | * input event port.  It increments a count of received messages.
              | *
              | ************************************************************************/
              |bool ${callback}(void *_ UNUSED){
              |  $varName = true;
              |  //CALLBACK(${callback_reg}(${callback}, NULL));
              |  return true;
              |}
              |
              |/************************************************************************
              | * ${name}:
              | * Invoked from local active thread.
              | *
              | * This is the function invoked by the active thread to decrement the
              | * input event index.
              | *
              | ************************************************************************/
              |bool ${name}(){
              |  bool result;
              |  result = ${varName};
              |  ${varName} = false;
              |  return result;
              |}
              |""")

        } else {
          val emit = s"${methodName}_emit()"
          Some(st"""/************************************************************************
              | * ${name}
              | * Invoked from user code in the local thread.
              | *
              | * This is the function invoked by the local thread to make a
              | * call to write to a remote data port.
              | *
              | ************************************************************************/
              |bool ${name}(void) {
              |  bool tb_result = true;
              |  ${emit};
              |  return tb_result;
              |}
              |""")
        }
        Some(C_SimpleContainer(inter, impl, None[ST](), None[(ST, ST)]()))

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
                  |#include "../../../${Util.DIR_INCLUDES}/${typeHeaderFileName}.h"
                  |
                  |${(sts, "\n\n")}
                  |
                  |#endif // $macroName
                  |"""

    return Resource(s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_INCLUDES}/${compTypeHeaderFileName}.h", ret)
  }

  def genComponentTypeImplementationFile(component:ir.Component, sts: ISZ[ST], preInitComments: ISZ[ST],
                                         cRunPreEntries: ISZ[ST], cDrainQueues: ISZ[ST]): Resource = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeFileName = s"${Util.GEN_ARTIFACT_PREFIX}_${name}"
    val ret: ST =  StringTemplate.componentTypeImpl(compTypeFileName, auxCSources, sts, preInitComments, cRunPreEntries, cDrainQueues, Util.isSporadic(component))

    return Resource(s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_SRC}/${compTypeFileName}.c", ret)
  }

  def isThreadConnection(ci: ir.ConnectionInstance): B = {
    val src = componentMap.get(Util.getName(ci.src.component)).get.category
    val dst = componentMap.get(Util.getName(ci.dst.component)).get.category
    return dst == ir.ComponentCategory.Thread && src == ir.ComponentCategory.Thread
  }


  def buildComponentMap(c: ir.Component): Unit = {
    val name = Util.getName(c.identifier)
    assert(!componentMap.contains(name))
    componentMap = componentMap + (name ~> c)
    c.subComponents.foreach(sc => buildComponentMap(sc))
    hasPeriodicComponents = hasPeriodicComponents | Util.isPeriodic(c)
  }

  def resolve(sys : ir.Component): B = {

    for(c <- componentMap.values){
      for(f <- c.features){
        //featureEndMap = featureEndMap + (Util.getName(f.identifier) ~> f.asInstanceOf[ir.FeatureEnd])
        featureMap = featureMap + (Util.getName(f.identifier) ~> f)

        f match {
          case fe: ir.FeatureEnd =>
            if (Util.isDataport(fe) && fe.classifier.isEmpty) {
              addError(s"Data type required for ${fe.category} ${Util.getName(fe.identifier)}")
            }
          case _ =>
        }
      }

      for(ci <- c.connectionInstances){
        if(isThreadConnection(ci)) {
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

          add(Util.getName(ci.src.feature.get), F)
          add(Util.getName(ci.dst.feature.get), T)
        } else {
          val src = componentMap.get(Util.getName(ci.src.component)).get
          val dst = componentMap.get(Util.getName(ci.dst.component)).get
          val srcName = s"${Util.getLastName(src.identifier)}.${Util.getLastName(ci.src.feature.get)}"
          val dstName = s"${Util.getLastName(dst.identifier)}.${Util.getLastName(ci.src.feature.get)}"
          Util.addMessage(s"Ignoring ${src.category} to ${dst.category} connection: ${srcName} -> ${dstName}")
        }
      }
    }

    def findProcessor(): Option[ir.Component] = {
      for (c <- componentMap.values if (c.category == ir.ComponentCategory.Process) && Util.getTypeHeaderFileName(c).nonEmpty) {
        return Some(c)
      }
      return None[ir.Component]()
    }

    findProcessor() match {
      case Some(p) =>
        topLevelProcess = Some(p)
        typeHeaderFileName = Util.getTypeHeaderFileName(p).get
      case _ =>
        addError("No processor bound process defined")
    }

    if(!hasErrors) {
      buildMonitors()
    }

    return !hasErrors
  }


  def resolveSharedDataFeatures(conns: ISZ[ir.ConnectionInstance]): Unit = {
    val dataConnections = conns.withFilter(f => f.kind == ir.ConnectionKind.Access).withFilter(
      f => featureMap.get(Util.getName(f.dst.feature.get)).get.category == ir.FeatureCategory.DataAccess)

    for(conn <- dataConnections) {
      val srcComp = componentMap.get(Util.getName(conn.src.component)).get

      if(srcComp.category != ir.ComponentCategory.Data) {
        addError(s"${Util.getLastName(conn.src.component)} is not a data component")
      }

      val dataKey = Util.getName(conn.src.feature.get)
      sharedData.get(dataKey) match {
        case Some(sd) =>
          val dstComp = componentMap.get(Util.getName(conn.dst.component)).get
          val ownerId = Util.getName(sd.owner.identifier)
          val dstId = Util.getName(dstComp.identifier)

          if(ownerId == dstId) {
            val _f = dstComp.features.withFilter(f => Util.getName(f.identifier) == Util.getName(conn.dst.feature.get))
            if(_f.size != 1) {
              addError(s"There are ${_f.size} matching features for ${Util.getName(conn.dst.feature.get)}, expecting only 1.")
            } else if(!_f(0).isInstanceOf[ir.FeatureAccess]) {
              addError(s"${Util.getName(conn.dst.feature.get)} is not a FeatureAccess.")
            } else {
              // add the owner's feature
              sharedData = sharedData + (dataKey ~> SharedData(sd.owner, Some(_f(0).asInstanceOf[ir.FeatureAccess]), sd.typ, sd.subcomponentId))
            }
          }
        case _ =>
          addError(s"Could not find data subcomponent: ${dataKey}")
      }
    }
  }

  def sortData(data: ISZ[ir.Component]): ISZ[ir.Component] = {
    def u(_c:ir.Component): String = { return Util.getClassifierFullyQualified(_c.classifier.get) }
    def sort(s: ISZ[ir.Component]): ISZ[ir.Component] = { return ISZOps(s).sortWith((a,b) => u(a) < u(b)) }

    // build dependence graph so that required data types are processed first
    var graph: Graph[ir.Component, String] = Graph(HashMap.empty, ISZ(), HashMap.empty, HashMap.empty, 0, F)
    for(d <- data){
      graph = graph * d
      for(s <- d.subComponents) {
        val pair = (d, typeMap.get(u(s)).get)
        graph = graph + pair
      }
    }

    var sorted: ISZ[ir.Component] = ISZ()
    def build(c : ir.Component): Unit = {
      if(ISZOps(sorted).contains(c)){ return }
      sort(graph.outgoing(c).map(m => m.dest)).foreach(o => build(o))
      sorted = sorted :+ c
    }
    sort(graph.nodes.keys.withFilter(k => graph.incoming(k).size == z"0")).foreach(r => build(r))
    return sorted
  }

  def addError(msg:String): Unit = {
    hasErrors = T
    Util.addError(msg)
  }
}

