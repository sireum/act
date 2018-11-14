// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.ops.ISZOps
import org.sireum.aadl.ir
import org.sireum.aadl.act.ast._

@record class Gen() {

  var topLevelProcess: Option[ir.Component] = None[ir.Component]()
  var typeHeaderFileName: String = ""

  var componentMap: HashMap[String, ir.Component] = HashMap.empty
  var typeMap: HashMap[String, ir.Component] = HashMap.empty
  var featureEndMap: HashMap[String, ir.FeatureEnd] = HashMap.empty

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

  var count: Z = 0
  def counter(): Z = {
    count = count + 1
    return count - 1
  }

  def process(model : ir.Aadl, cSources: ISZ[String]) : ActContainer = {

    auxCSources = cSources.map(c => st"""#include "../../../${c}"""")

    val datatypes = model.components.withFilter(f => f.category == ir.ComponentCategory.Data)
    for(d <- datatypes) { typeMap = typeMap + (Util.getClassifierFullyQualified(d.classifier.get) ~> d) }
    val sortedData = sortData(datatypes)

    val components = model.components.withFilter(f => f.category != ir.ComponentCategory.Data)
    if(components.size != z"1" || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }

    val system = components(0)
    if(resolve(system)) {
      auxFiles = auxFiles :+ Resource(s"${Util.DIR_INCLUDES}/$typeHeaderFileName.h", processDataTypes(sortedData))
      if(hasPeriodicComponents) {
        auxFiles = auxFiles :+ TimerUtil.timerInterface()
      }

      gen(system)
    }

    return ActContainer(Util.getLastName(topLevelProcess.get.identifier),
      connectors,
      astObjects,
      monitors.values,
      containers,
      auxFiles)
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
            createConnection(Util.CONNECTOR_SEL4_TIMESERVER,
              componentId, TimerUtil.TIMER_ID,
              TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_TIMER_ID)

            // connect dispatcher to component
            createConnection(Util.CONNECTOR_SEL4_NOTIFICATION,
              TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.componentNotificationName(componentId),
              componentId, TimerUtil.TIMER_NOTIFICATION_ID)

            dispatchNotifications = dispatchNotifications :+ Emits(
              name = TimerUtil.componentNotificationName(componentId),
              typ = Util.NOTIFICATION_TYPE)

            val period: Z = if(Util.getPeriod(sc).isEmpty) {
              cprint(T, s"Period not provided for ${classifier}, using ${Util.DEFAULT_PERIOD}")
              Util.DEFAULT_PERIOD
            } else {
              Util.getPeriod(sc).get
            }
            calendars = calendars :+ TimerUtil.calendar(componentId, period)

            configuration = configuration :+ TimerUtil.configurationTimerAttribute(componentId, counter(), F)
            configuration = configuration :+ TimerUtil.configurationTimerGlobalEndpoint(componentId, classifier, TimerUtil.TIMER_ID)

            val priority: Z = if(Util.getPriority(sc).isEmpty){
              cprint(T, s"Priority not provided for ${classifier}, using ${Util.DEFAULT_PRIORITY}")
              Util.DEFAULT_PRIORITY
            } else {
              Util.getPriority(sc).get
            }
            configuration = configuration :+ StringTemplate.configurationPriority(componentId, priority)

            val stackSize: Z = if(Util.getStackSize(sc).isEmpty) {
              cprint(T, s"Stack Size not provided for ${classifier}, using ${Util.DEFAULT_STACK_SIZE}")
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
        case _ =>
          halt(s"gen: Not handling: ${sc}")
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
      containers = containers :+ C_Container(timerComponent.component.name, ISZ(TimerUtil.timerCSource()), ISZ())

      // add the dispatcher component
      val dispatchComponent = TimerUtil.dispatchComponent(dispatchNotifications)
      instances = instances :+ dispatchComponent
      containers = containers :+ C_Container(dispatchComponent.component.name,
        ISZ(TimerUtil.dispatchComponentCSource(s"<${typeHeaderFileName}.h>", calendars)), ISZ())

      // connect dispatch timer to time server
      createConnection(Util.CONNECTOR_SEL4_TIMESERVER,
        TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.TIMER_ID_DISPATCHER,
        TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_TIMER_ID)

      createConnection(Util.CONNECTOR_SEL4_GLOBAL_ASYNCH_CALLBACK,
        TimerUtil.TIMER_INSTANCE, TimerUtil.TIMER_SERVER_NOTIFICATION_ID,
        TimerUtil.DISPATCH_PERIODIC_INSTANCE, TimerUtil.TIMER_NOTIFICATION_DISPATCHER_ID)

      configuration = configuration :+ TimerUtil.configurationTimerAttribute(dispatchComponent.name, counter(), T)

      // FIXME: TB uses "periodic_dispatcher" rather than the assigned classifier "dispatch_periodic"???
      configuration = configuration :+ TimerUtil.configurationTimerGlobalEndpoint(dispatchComponent.name, dispatchComponent.component.name, TimerUtil.TIMER_ID_DISPATCHER)
      configuration = configuration :+ TimerUtil.configurationTimerGlobalEndpoint(dispatchComponent.name, dispatchComponent.component.name, TimerUtil.TIMER_NOTIFICATION_DISPATCHER_ID)

      // FIXME: is there a default priority for dispatch component?
      configuration = configuration :+ StringTemplate.configurationPriority(dispatchComponent.name, Util.DEFAULT_PRIORITY)
    }

    def createConnection(connectionType: String,
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
        connectionType = connectionType,
        from_ends = from_ends,
        to_ends = to_ends
      )

      connections = connections :+ con
    }

    def createNotificationConnection(srcComponent: String, srcFeature: String,
                                     dstComponent: String, dstFeature: String): Unit = {
      createConnection(Util.CONNECTOR_SEL4_NOTIFICATION, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createRPCConnection(srcComponent: String, srcFeature: String,
                            dstComponent: String, dstFeature: String) : Unit = {
      createConnection(Util.CONNECTOR_RPC, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createDataConnection(conn: ir.ConnectionInstance) : Unit = {
      val monitor = getMonitorForConnectionInstance(conn).get

      val srcComponent = Util.getLastName(conn.src.component)
      val dstComponent = Util.getLastName(conn.dst.component)

      val srcFeature = featureEndMap.get(Util.getName(conn.src.feature.get)).get
      val dstFeature = featureEndMap.get(Util.getName(conn.dst.feature.get)).get

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

    for(conn <- c.connectionInstances) {
      val fdst = featureEndMap.get(Util.getName(conn.dst.feature.get)).get

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

    var imports : Set[String] = Set.empty

    var cIncludes: ISZ[ST] = ISZ()
    var cImpls: ISZ[ST] = ISZ()

    for(f <- c.features) {
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
            } else {
              cprintln(T,s"in port '${fpath}' is not connected")
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

      def handleSubprogramAccess(): Unit = {
        val proc = Util.getClassifier(fend.classifier.get)
        val kind = Util.getDiscreetPropertyValue(f.properties, "AccessType")
        kind match {
          case Some(v : ir.ValueProp) =>
            if(v.value == "requires") {
              imports = imports + Util.getInterfaceFilename(proc)
              uses = uses :+ Uses(
                name = fid,
                optional = F,
                typ = proc)
            } else if (v.value == "provides") {
              imports = imports + Util.getInterfaceFilename(proc)
              provides = provides :+ Provides(
                name = fid,
                typ = proc)
            } else {
              halt(s"Unexpected: ${v}")
            }
          case _ =>
            halt("Unexpected")
        }
      }

      f.category match {
        case ir.FeatureCategory.DataPort =>
          handleDataPort()
        case ir.FeatureCategory.EventDataPort =>
          handleDataPort()

        case ir.FeatureCategory.SubprogramAccessGroup =>
          handleSubprogramAccess()
        case ir.FeatureCategory.SubprogramAccess =>
          handleSubprogramAccess()

        case ir.FeatureCategory.EventPort =>
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
          cprintln(F, s"Skipping ${f.category} for ${fid}.${fid}")
      }

      generateC_InterfaceMethod(c, f.asInstanceOf[ir.FeatureEnd]) match {
        case Some((interface, impl)) =>
          cIncludes = cIncludes :+ interface
          cImpls = cImpls :+ impl
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
        println()
      case _ =>
        cprint(T, s"Dispatch Protocol not specified for ${c}")
    }
    containers = containers :+ C_Container(cid,
      ISZ(genComponentTypeImplementationFile(c, cImpls)),
      ISZ(genComponentTypeInterfaceFile(c, cIncludes))
    )

    return Component(
      control = uses.nonEmpty,
      hardware = F,
      name = cid,

      mutexes = ISZ(),
      binarySemaphores = binarySemaphores,
      semaphores = ISZ(),

      dataports = ISZ(),
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
        val dstFeature: ir.FeatureEnd = featureEndMap.get(Util.getName(connInst.dst.feature.get)).get

        def handleDataPort(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, dstFeature)
          val interfaceName = Util.getInterfaceName(dstFeature)

          val typeName = Util.getClassifierFullyQualified(dstFeature.classifier.get)

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
            name = Util.getMonitorWriterName(dstFeature),
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
              Resource(implName, StringTemplate.tbMonReadWrite(paramTypeName, Util.getQueueSize(f), monitorName, typeHeaderFileName))
            } else {
              Resource(implName, StringTemplate.tbEnqueueDequeue(paramTypeName, Util.getQueueSize(f), monitorName, typeHeaderFileName))
            }

          val interName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_INCLUDES}/${monitorName}.h"
          val cincludes = Resource(interName, StringTemplate.tbInterface(s"__${monitorName}_H__"))

          monitors = monitors + (connInstName ~> Monitor(inst, interface, writer, cimplementation, cincludes, i, connInst))
        }

        connInst.kind match {
          case ir.ConnectionKind.Port =>
            dstFeature.category match {
              case ir.FeatureCategory.DataPort =>
                handleDataPort(dstFeature)
              case ir.FeatureCategory.EventDataPort =>
                handleDataPort(dstFeature)
              case ir.FeatureCategory.EventPort =>
                // will use Notification
              case _ =>
                halt(s"not expecting ${dst}")
            }
          case _ =>
            cprintln(T,s"processInConnections: Not handling ${connInst}")
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
    val body = st"""#ifndef ${macroname}
                   |#define ${macroname}
                   |
                   |#include <stdbool.h>
                   |#include <stdint.h>
                   |
                   |#ifndef TB_VERIFY
                   |#include <stddef.h>
                   |#endif // TB_VERIFY
                   |
                   |#define __TB_OS_CAMKES__
                   |#define TB_MONITOR_READ_ACCESS 111
                   |#define TB_MONITOR_WRITE_ACCESS 222
                   |
                   |#ifndef TB_VERIFY
                   |#define MUTEXOP(OP)\
                   |if((OP) != 0) {\
                   |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);\
                   |  *((int*)0)=0xdeadbeef;\
                   |}
                   |#else
                   |#define MUTEXOP(OP) OP
                   |#endif // TB_VERIFY
                   |#ifndef TB_VERIFY
                   |#define CALLBACKOP(OP)\
                   |if((OP) != 0) {\
                   |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);\
                   |  *((int*)0)=0xdeadbeef;\
                   |}
                   |#else
                   |#define CALLBACKOP(OP) OP
                   |#endif // TB_VERIFY
                   |
                   |${(defs, "\n\n")}
                   |
                   |#endif // __TB_AADL_${typeHeaderFileName}__H
                   |"""
    return body
  }

  def processDataType(c: ir.Component, isField: B): ST = {
    val s: ST =
      if (TypeUtil.isRecordType(c)) {
        val name = Util.getClassifierFullyQualified(c.classifier.get)
        if(isField) {
          st"""$name"""
        } else {
          val fields: ISZ[ST] = c.subComponents.map(sub => {
            st"""${processDataType(sub, T)} ${Util.getLastName(sub.identifier)};"""
          })


          st"""typedef
              |  struct ${name} {
              |    ${(fields, "\n")}
              |  } ${name};"""
        }
      } else if (TypeUtil.isBaseType(c.classifier.get.name)) {
        st"""${TypeUtil.translateBaseType(c.classifier.get.name)}"""
      } else if (isField) {
        st"""${Util.getClassifierFullyQualified(c.classifier.get)}"""
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
        cprintln(T, s"Unexpected datatype: ${c}")
        st""" """
      }


    return s
  }

  def generateC_InterfaceMethod(component: ir.Component, feature: ir.FeatureEnd): Option[(ST, ST)] = {
    val ret: Option[(ST, ST)] = feature.category match {
      case ir.FeatureCategory.DataPort =>
        val (suffix, mod) : (String, String) = feature.direction match {
          case ir.Direction.In => ("read", "")
          case ir.Direction.Out => ("write", "const ")
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(feature, None[Z]())
        val paramType = Util.getMonitorWriterParamName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get)

        val inter = st"""bool ${name}_${suffix}(${mod}${paramType} * ${name});"""
        val impl: ST =
          if(suffix == "write") {
            st"""bool ${name}_${suffix}(${mod}${paramType} * ${name}){
                |  bool tb_result = true;
                |  tb_result &= ${name}_${suffix}((${paramType} *) ${name});
                |  return tb_result;
                |}"""
          } else {
            st""" """
          }
        Some((inter, impl))

      case ir.FeatureCategory.EventDataPort =>
        val (suffix, mod, paramType): (String, String, String) = feature.direction match {
          case ir.Direction.In => ("dequeue", "", Util.getMonitorWriterParamName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get))
          case ir.Direction.Out => ("enqueue", "const ", Util.getClassifierFullyQualified(feature.classifier.get))
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(feature, None[Z]())

        val inter = st"""bool ${name}_${suffix}(${mod}${paramType} * ${name});"""

        val impl: ST = if(suffix == "enqueue") {
          st"""bool ${name}_${suffix}(${mod}${paramType} * ${name}){
              |  bool tb_result = true;
              |  tb_result &= ${name}_${suffix}((${paramType} *) ${name});
              |  return tb_result;
              |}"""
        } else {
          val simpleName = Util.getLastName(feature.identifier)
          st"""void tb_entrypoint_tb_${Util.getClassifier(component.classifier.get)}_${simpleName}(const ${paramType} * in_arg) { }"""
        }
        Some((inter, impl))

      case ir.FeatureCategory.EventPort =>
        val dir: String = feature.direction match {
          case ir.Direction.In => "read"
          case ir.Direction.Out => "write"
          case x => halt(s"Unexpected direction: ${x}")
        }
        val compName = Util.getClassifier(component.classifier.get)
        val methodName = Util.getLastName(feature.identifier)
        val name = s"${Util.GEN_ARTIFACT_PREFIX}_${compName}_${dir}_${methodName}"

        val inter = st"""bool ${name}(void);"""

        val impl: ST = if(dir == "read") {
          val varName = s"${methodName}_index"
          val callback = s"${methodName}_callback"
          val callback_reg = s"${methodName}_reg_callback"

          st"""static bool ${varName} = false;
              |
              |bool ${callback}(void *_ UNUSED){
              |  $varName = true;
              |  //CALLBACK(${callback_reg}(${callback}, NULL));
              |  return true;
              |}
              |
              |bool ${name}(){
              |  bool result;
              |  result = ${varName};
              |  ${varName} = false;
              |  return result;
              |}
              |"""

        } else {
          val emit = s"${methodName}_emit()"
          st"""bool ${name}(void) {
              |  bool tb_result = true;
              |  ${emit};
              |  return tb_result;
              |}
              |"""
        }
        Some((inter, impl))

      case _ => None[(ST,ST)]()
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

  def genComponentTypeImplementationFile(component:ir.Component, sts: ISZ[ST]): Resource = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeFileName = s"${Util.GEN_ARTIFACT_PREFIX}_${name}"

    val ret = st"""#include "../${Util.DIR_INCLUDES}/${compTypeFileName}.h"
                  |${(auxCSources, "\n")}
                  |#include <string.h>
                  |#include <camkes.h>
                  |
                  |${(sts, "\n\n")}
                  |
                  |void pre_init(void) { }
                  |
                  |int run(void) {
                  |  // Initial lock to await dispatch input.
                  |  MUTEXOP(tb_dispatch_sem_wait())
                  |  for(;;) {
                  |    MUTEXOP(tb_dispatch_sem_wait())
                  |    // Drain the queues
                  |  }
                  |  return 0;
                  |}
                  |"""
    return Resource(s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_SRC}/${compTypeFileName}.c", ret)
  }

  def isThreadConnection(ci: ir.ConnectionInstance): B = {
    val src = componentMap.get(Util.getName(ci.src.component)).get.category
    val dst = componentMap.get(Util.getName(ci.dst.component)).get.category
    return dst == ir.ComponentCategory.Thread && src == ir.ComponentCategory.Thread
  }


  def resolve(sys : ir.Component): B = {
    def constructMap(c: ir.Component): Unit = {
      val name = Util.getName(c.identifier)
      assert(!componentMap.contains(name))
      componentMap = componentMap + (name ~> c)
      c.subComponents.foreach(sc => constructMap(sc))
      hasPeriodicComponents = hasPeriodicComponents | Util.isPeriodic(c)
    }
    constructMap(sys)

    componentMap.values.foreach(c => {
      for (f <- c.features.withFilter(f => f.isInstanceOf[ir.FeatureEnd])) {
        featureEndMap = featureEndMap + (Util.getName(f.identifier) ~> f.asInstanceOf[ir.FeatureEnd])
      }

      c.connectionInstances.withFilter(ci => isThreadConnection(ci)).foreach(ci => {
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
      })
    })

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
        cprintln(T, "No processor bound process defined")
        return false
    }

    buildMonitors()

    return true
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
}
