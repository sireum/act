// #Sireum

package org.sireum.aadl.act

import org.sireum._
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


  var astObjects: ISZ[ASTObject] = ISZ()
  var ginstances: ISZ[Instance] = ISZ()
  var monitors: HashMap[String, Monitor] = HashMap.empty // conn instname -> monitor
  var procedures: ISZ[ASTObject] = ISZ()
  var auxFiles: ISZ[(String, ST)] = ISZ()

  def process(model : ir.Aadl) : ActContainer = {

    def resolve(sys : ir.Component): B = {
      def r(c: ir.Component): Unit = {
        val name = Util.getName(c.identifier)
        assert(!componentMap.contains(name))
        componentMap = componentMap + (name ~> c)

        for (f <- c.features.withFilter(f => f.isInstanceOf[ir.FeatureEnd])) {
          featureEndMap = featureEndMap + (Util.getName(f.identifier) ~> f.asInstanceOf[ir.FeatureEnd])
        }

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

      r(sys)

      def getProcessor(): Option[ir.Component] = {
        for (c <- componentMap.values if (c.category == ir.ComponentCategory.Process) && Util.getTypeHeaderFileName(c).nonEmpty) {
          return Some(c)
        }
        return None[ir.Component]()
      }

      getProcessor() match {
        case Some(p) =>
          topLevelProcess = Some(p)
          typeHeaderFileName = Util.getTypeHeaderFileName(p).get
        case _ =>
          cprintln(T,"No processor bound process defined")
          return false
      }

      buildMonitors()

      return true
    }

    val data = model.components.withFilter(f => f.category == ir.ComponentCategory.Data)
    val components = model.components.withFilter(f => f.category != ir.ComponentCategory.Data)
    if(components.size != 1 || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }
    val system = components(0)

    for(d <- data) {
      typeMap = typeMap + (Util.getClassifierFullyQualified(d.classifier.get) ~> d)
    }

    if(resolve(system)) {
      val x: (String, ST) = (s"${Util.DIR_INCLUDES}/${typeHeaderFileName}.h", processDataTypes(data))
      auxFiles = auxFiles :+ x

      gen(system)

      astObjects = astObjects ++ procedures
    }

    return ActContainer(astObjects, auxFiles)
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
            returnType = None[String]()
          )

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(method), includes = ISZ())
        case _ =>
          halt(s"gen: Not handling: ${sc}")
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

    var imports : Set[String] = Set.empty

    var cIncludes: ISZ[ST] = ISZ()
    var cImpls: ISZ[ST] = ISZ()

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
          cprintln(F, s"Skipping ${f.category} for ${fid}.${fid}")
      }

      generateC_InterfaceMethod(c, f.asInstanceOf[ir.FeatureEnd]) match {
        case Some((interface, impl)) =>
          cIncludes = cIncludes :+ interface
          cImpls = cImpls :+ impl
        case _ =>
      }
    }

    genComponentTypeInterfaceFile(c, cIncludes)
    genComponentTypeImplementationFile(c, cImpls)

    var binarySemaphores: ISZ[BinarySemaphore] = ISZ()
    val binSem = Util.getDiscreetPropertyValue(c.properties, "camkes::Binary_Semaphore")
    binSem match {
      case Some(v: ir.ValueProp) =>
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
      includes = ISZ(s"<${typeHeaderFileName}.h>"),
      attributes = ISZ(),

      imports = imports.elements // FIXME
    )
  }

  def buildMonitors(): Unit = {
    for (portPath <- outConnections.keys.withFilter(p => outConnections.get(p).get.size > 0)) {
      var i: Z = 0
      for (connInst <- outConnections.get(portPath).get()) {

        val dst: ir.Component = componentMap.get(Util.getName(connInst.dst.component)).get
        val dstFeature: ir.FeatureEnd = featureEndMap.get(Util.getName(connInst.dst.feature.get)).get

        val src: ir.Component = componentMap.get(Util.getName(connInst.src.component)).get
        val srcFeature: ir.FeatureEnd = featureEndMap.get(Util.getName(connInst.src.feature.get)).get

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
            imports = ISZ(s"${interfaceName}.idl4")
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
            includes = ISZ(s"<${typeHeaderFileName}.h>")
          )

          val paramType = typeMap.get(typeName).get
          val writer: Procedure = Procedure (
            name = Util.getMonitorWriterName(dstFeature),
            methods = ISZ(Method(
              name = s"write_${typeName}",
              parameters = ISZ(Parameter(F, Direction.Refin, "arg", Util.getMonitorWriterParamName(paramType))),
              returnType = Some("bool")
            )),
            includes = ISZ(s"<${typeHeaderFileName}.h>")
          )

          val connInstName = Util.getName(connInst.name)
          monitors = monitors + (connInstName ~> Monitor(inst, proc, writer, i, connInst))

          procedures = procedures :+ proc :+ writer
          ginstances = ginstances :+ inst

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_SRC}/${monitorName}.c"
          val impl: (String, ST) = (implName, StringTemplate.tbImpl(typeName, Util.getQueueSize(f), monitorName, typeHeaderFileName))
          auxFiles = auxFiles :+ impl

          val interName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/${Util.DIR_INCLUDES}/${monitorName}.h"
          val inter: (String, ST) = (interName, StringTemplate.tbInterface(s"__${monitorName}_H__"))
          auxFiles = auxFiles :+ inter
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

  def processDataTypes(values: ISZ[ir.Component]): ST = {
    val defs: ISZ[ST] = values.withFilter(v => TypeUtil.translateBaseType(v.classifier.get.name).isEmpty).map(v => processDataType(v, F))
    val macroname = s"__TB_AADL_${typeHeaderFileName}__H"
    val body = st"""#ifndef ${macroname}
                   |#define ${macroname}
                   |
                   |include <stdbool.h>
                   |include <stdint.h>
                   |
                   |${(defs, "\n\n")}
                   |
                   |#endif // __TB_AADL_${typeHeaderFileName}__H
                   |"""
    return body
  }

  def processDataType(c: ir.Component, isField: B): ST = {
    var includes: ISZ[ST] = ISZ()
    val s: ST =
      if(TypeUtil.isRecordType(c)) {

        val fields: ISZ[ST] = c.subComponents.map(sub => {
          st"""${processDataType(sub, T)} ${Util.getLastName(sub.identifier)};"""
        })

        val name = Util.getClassifierFullyQualified(c.classifier.get)
        st"""typedef
            |  struct ${name} {
            |    ${(fields, "\n")}
            |  } ${name};"""
      } else {
        if(TypeUtil.isBaseType(c.classifier.get.name)) {
          st"""${TypeUtil.translateBaseType(c.classifier.get.name)}"""
        } else if (isField) {
          st"""${Util.getClassifierFullyQualified(c.classifier.get)}"""
        } else if(TypeUtil.isArrayDef(c)) {
          // TODO multidim arrays
          val name = Util.getClassifierFullyQualified(c.classifier.get)
          val container = Util.getContainerName(name)
          st"""typdef ${TypeUtil.getArrayBaseType(c)} ${name} [${TypeUtil.getArrayDimension(c)}];
              |
              |typedef
              |  struct ${container} {
              |    ${name} f;
              |  } ${container};"""
        } else {
          halt(s"Unexpected ${c}")
        }
      }

    return s
  }

  def generateC_InterfaceMethod(component: ir.Component, feature: ir.FeatureEnd): Option[(ST,ST)] = {
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
                |  tb_result &= ${name}0_${suffix}((${paramType} *) ${name});
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
              |  tb_result &= ${name}0_${suffix}((${paramType} *) ${name});
              |  return tb_result;
              |}"""
        } else {
          st""""""
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
              |  CALLBACK(${callback_reg}(${callback}, NULL));
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

  def genComponentTypeInterfaceFile(component: ir.Component, sts: ISZ[ST]): Unit = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeHeaderFileName = s"${Util.GEN_ARTIFACT_PREFIX}_${name}"
    val macroName = s"__${Util.GEN_ARTIFACT_PREFIX}_AADL_${name}_types__H"

    val ret = st"""#ifndef $macroName
                  |#define $macroName
                  |
                  |#include <${typeHeaderFileName}.h>
                  |
                  |${(sts, "\n")}
                  |
                  |#endif // $macroName
                  |"""

    val x: (String, ST) = (s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_INCLUDES}/${compTypeHeaderFileName}.h", ret)
    auxFiles = auxFiles :+ x
  }

  def genComponentTypeImplementationFile(component:ir.Component, sts: ISZ[ST]): Unit = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeFileName = s"${Util.GEN_ARTIFACT_PREFIX}_${name}"

    val ret = st"""#include "${compTypeFileName}.h"
                  |#include <string.h>
                  |#include <camkes.h>
                  |
                  |${(sts, "\n")}
                  |"""
    val x: (String, ST) = (s"${Util.DIR_COMPONENTS}/${name}/${Util.DIR_SRC}/${compTypeFileName}.c", ret)
    auxFiles = auxFiles :+ x
  }
}
