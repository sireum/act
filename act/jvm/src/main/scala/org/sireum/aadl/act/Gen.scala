// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.aadl.ir

@record class Gen() {

  var topLevelProcess: Option[ir.Component] = None[ir.Component]
  var typeHeaderFileName: String = ""

  var componentMap: HashMap[String, ir.Component] = HashMap.empty
  var featureEndMap: HashMap[String, ir.FeatureEnd] = HashMap.empty

  var inConnections: HashMap[String, ISZ[ir.ConnectionInstance]] = HashMap.empty
  var outConnections: HashMap[String, ISZ[ir.ConnectionInstance]] = HashMap.empty


  var astObjects: ISZ[ASTObject] = ISZ()
  var ginstances: ISZ[Instance] = ISZ()
  var monitors: HashMap[String, Monitor] = HashMap.empty
  var procedures: ISZ[ASTObject] = ISZ()

  def gen(m : ir.Aadl) : ISZ[ASTObject] = {

    def resolve(m : ir.Aadl): B = {
      def r(c: ir.Component): Unit = {
        val name = Util.getName(c.identifier)
        assert(!componentMap.contains(name))
        componentMap = componentMap + (name ~> c)

        c.features.foreach(f =>
          if (f.isInstanceOf[ir.FeatureEnd]) {
            featureEndMap = featureEndMap + (Util.getName(f.identifier) ~> f.asInstanceOf[ir.FeatureEnd])
          })

        c.connectionInstances.foreach(ci => {
          def add(portPath: String, isIn: B): Unit = {
            var map: HashMap[String, ISZ[ir.ConnectionInstance]] = isIn match {
              case T => inConnections
              case F => outConnections
            }
            var cis: ISZ[ir.ConnectionInstance] = map.get(portPath) match {
              case Some(cis) => cis
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
        c.subComponents.foreach(sc => r(sc))
      }

      m.components.foreach(c => r(c))

      def getProcessor(): Option[ir.Component] = {
        for (c <- componentMap.values if (c.category == ir.ComponentCategory.Process) && Util.getTypeHeaderFileName(c).nonEmpty) {
          return Some(c)
        }
        return None[ir.Component]
      }

      getProcessor() match {
        case Some(p) =>
          topLevelProcess = Some(p)
          typeHeaderFileName = Util.getTypeHeaderFileName(p).get
        case _ =>
          Console.err.println("No processor bound process defined")
          return false
      }

      buildMonitors()

      return true
    }

    if(resolve(m)) {
      m.components.foreach(c => gen(c))

      astObjects = astObjects ++ procedures
    }

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
          val method = Method(
            name = Util.getLastName(sc.identifier),
            parameters = params,
            returnType = None[String]
          )

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(method), includes = ISZ())
        case _ =>
          halt(s"genContainer: Not handling: ${sc}")
      }
    }

    var connections: ISZ[Connection] = ISZ()
    var i: Z = 1

    def newConn(): String = {
      val ret = s"conn${i}"
      i = i + 1
      return ret
    }

    def createConnection(isNotification: B,
                         srcComponent: String, srcFeature: String,
                         dstComponent: String, dstFeature: String): Unit = {
      val connector = Connector(
          name = if(isNotification) Util.CONNECTOR_SEL4_NOTIFICATION else Util.CONNECTOR_RPC,
          from_type = "", to_type = "",
          from_hardware = F, from_multiple = F, from_threads = 1, to_hardware = F, to_multiple = F, to_threads = 1
        )

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
        connector = connector,
        from_ends = from_ends,
        to_ends = to_ends
      )

      connections = connections :+ con
    }

    def createNotificationConnection(srcComponent: String, srcFeature: String,
                                     dstComponent: String, dstFeature: String): Unit = {
      createConnection(T, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createRPCConnection(srcComponent: String, srcFeature: String,
                            dstComponent: String, dstFeature: String) : Unit = {
      createConnection(F, srcComponent, srcFeature, dstComponent, dstFeature)
    }

    def createDataConnection(conn: ir.ConnectionInstance) : Unit = {
      val monitor = getMonitorForConnectionInstance(conn).get

      val srcComponent = Util.getLastName(conn.src.component)
      val dstComponent = Util.getLastName(conn.dst.component)

      val srcFeature = featureEndMap.get(Util.getName(conn.src.feature.get)).get
      val dstFeature = featureEndMap.get(Util.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
      val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z])

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

    return Composition(
      groups = ISZ(),
      exports = ISZ(),
      instances = instances ++ ginstances,
      connections = connections
    )
  }

  def genThread(c : ir.Component) : Component = {
    assert(c.category == ir.ComponentCategory.Thread)
    assert(c.subComponents.isEmpty)
    assert(c.connectionInstances.isEmpty)

    var provides: ISZ[Provides] = ISZ()
    var uses: ISZ[Uses] = ISZ()
    var emits: ISZ[Emits] = ISZ()
    var consumes: ISZ[Consumes] = ISZ()

    var imports : Set[String] = Set.empty[String]()

    for(f <- c.features){
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
              imports = imports + Util.getInterfaceFilename(monitor.p.name)

              uses = uses :+ Uses(
                name = Util.genMonitorFeatureName(fend, None[Z]),
                typ = monitor.p.name,
                optional = F)

              consumes = consumes :+ Consumes(
                name = Util.genMonitorNotificationFeatureName(fend),
                typ = Util.getMonitorNotificationType(fend.category),
                optional = F)
            } else {
              Console.err.println(s"in port '${fpath}' is not connected")
            }
          case ir.Direction.Out =>
            // uses monitor
            outConnections.get(fpath) match {
              case Some(outs) =>
                var i: Z = 0
                outs.foreach(o => {
                  val dstFeature = featureEndMap.get(fpath).get
                  val interfaceName = Util.getInterfaceName(dstFeature)
                  imports = imports + Util.getInterfaceFilename(interfaceName)

                  uses = uses :+ Uses(
                    name = Util.genMonitorFeatureName(fend, Some(i)),
                    typ = interfaceName,
                    optional = F
                  )
                  i = i + 1
                  nop()
                })
              case _ =>
            }
          case _ =>
            halt(s"Not expecting direction ${fend.direction}")
        }
      }

      def handleSubprogramAccess(): Unit = {
        val proc = Util.getClassifier(fend.classifier.get)
        val kind: Option[ir.ValueProp] = Util.getDiscreetPropertyValue[ir.ValueProp](f.properties, "AccessType")
        kind match {
          case Some(v) =>
            if(v.value == "requires") {
              uses = uses :+ Uses(
                name = fid,
                optional = F,
                typ = proc)
            } else if (v.value == "provides") {
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
          Console.err.println(s"Skipping ${f.category} for ${fid}.${fid}")
      }
    }

    var binarySemaphores: ISZ[BinarySemaphore] = ISZ()
    val binSem = Util.getDiscreetPropertyValue[ir.ValueProp](c.properties, "camkes::Binary_Semaphore")
    binSem match {
      case Some(v) =>
        binarySemaphores = binarySemaphores :+ BinarySemaphore(v.value)
      case _ =>
    }

    val cid = Util.getClassifier(c.classifier.get)
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
      includes = ISZ(s"<${typeHeaderFileName}>"),
      attributes = ISZ(),

      imports.elements // FIXME
    )
  }

  def buildMonitors(): Unit = {
    for (portPath <- outConnections.keys.withFilter(p => outConnections.get(p).get.size > 0)) {
      var i: Z = 0
      for (connInst <- outConnections.get(portPath).get()) {

        val dst: ir.Component = componentMap.get(Util.getName(connInst.dst.component)).get
        val dstFeature: ir.FeatureEnd = featureEndMap.get(Util.getName(connInst.dst.feature.get)).get()

        val src: ir.Component = componentMap.get(Util.getName(connInst.src.component)).get
        val srcFeature: ir.FeatureEnd = featureEndMap.get(Util.getName(connInst.src.feature.get)).get()

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
            includes = ISZ(s"${interfaceName}.idl4"),
            attributes = ISZ(),
            imports = ISZ()
          )

          val inst: Instance = Instance(address_space = "", name = Util.toLowerCase(monitorName), component = monitor)

          val methods: ISZ[Method] = f.category match {
            case ir.FeatureCategory.EventDataPort => createQueueMethods(typeName)
            case ir.FeatureCategory.DataPort => createReadWriteMethods(typeName)
            case _ =>
              halt(s"not expecting ${f.category}")
          }

          val proc: Procedure = Procedure(
            name = interfaceName,
            methods = methods,
            includes = ISZ(typeHeaderFileName)
          )

          val connInstName = Util.getName(connInst.name)
          monitors = monitors + (connInstName ~> Monitor(inst, proc, i, connInst))

          procedures = procedures :+ proc
          ginstances = ginstances :+ inst
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
                nop()
              case _ =>
                halt(s"not expecting ${dst}")
            }
          case _ =>
            Console.err.println(s"processInConnections: Not handling ${connInst}")
        }

        i = i + 1
      }
    }
  }

  def createReadWriteMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "read", parameters = ISZ(Parameter(F, Direction.Refin, "m", typeName)), returnType = Some("bool")),
      Method(name = "write", parameters = ISZ(Parameter(F, Direction.Out, "m", typeName)), returnType = Some("bool")))
  }

  def createQueueMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "enqueue", parameters = ISZ(Parameter(F, Direction.Refin, "m", typeName)), returnType = Some("bool")),
      Method(name = "dequeue", parameters = ISZ(Parameter(F, Direction.Out, "m", typeName)), returnType = Some("bool")))
  }


  def getMonitorForConnectionInstance(instance: ir.ConnectionInstance): Option[Monitor] = {
    for(m <- monitors.values if(m.ci == instance)){
      return Some(m)
    }
    return None[Monitor]()
  }

  def getMonitorForInPort(end: ir.FeatureEnd): Option[Monitor] = {
    val n = Util.getName(end.identifier)
    for(m <- monitors.values if(Util.getName(m.ci.dst.feature.get) == n)) {
      return Some(m)
    }
    return None[Monitor]()
  }

  def nop(): Unit = {}
}
