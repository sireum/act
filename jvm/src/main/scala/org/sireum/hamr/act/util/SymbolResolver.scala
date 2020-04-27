// #Sireum

package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.act.{Dispatch_Protocol, Util, util}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Component, RangeProp}
import org.sireum.message.Reporter


@datatype class SymbolTable(rootSystem: AadlSystem,

                            componentMap: HashSMap[String, AadlComponent],

                            airComponentMap: HashSMap[String, ir.Component],
                            airFeatureMap: HashSMap[String, ir.Feature],
                            airClassifierMap: HashSMap[String, ir.Component],

                            inConnections: HashSMap[String, ISZ[ir.ConnectionInstance]],
                            outConnections: HashSMap[String, ISZ[ir.ConnectionInstance]]
                           ) {

  def getInConnections(featurePath: String): ISZ[ir.ConnectionInstance] = {
    return if(inConnections.contains(featurePath)) inConnections.get(featurePath).get
    else ISZ()
  }


  def getMaxDomain(): Z = {
    var max: Z = z"2" // threads start a domain 2
    for(p <- getPeriodicThreads()) {
      p.getDomain(this) match {
        case Some(z) => if((z + z"1") > max) {
          max = z + z"1"
        }
        case _ => 
      }
    }
    return max
  }

  def hasPeriodicThreads(): B = {
    return ops.ISZOps(getThreads()).exists(p => Util.isPeriodic(p.component))
  }

  def getThread(c: Component): AadlThread = { 
    return componentMap.get(Util.getName(c.identifier)).get.asInstanceOf[AadlThread]
  }

  def getPeriodicThreads(): ISZ[AadlThread] = {
    return ops.ISZOps(getThreads()).filter(p => Util.isPeriodic(p.component))
  }

  def getProcess(id: String): AadlProcess = { return componentMap.get(id).get.asInstanceOf[AadlProcess] }
  
  def getProcesses(): ISZ[AadlProcess] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlProcess])
    return c.map(m => m.asInstanceOf[AadlProcess])
  }

  
  def getThreads(): ISZ[AadlThread] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlThread])
    return c.map(m => m.asInstanceOf[AadlThread])
  }
  
  def getBoundProcesses(c: AadlProcessor): ISZ[AadlProcess] = {
    val ret: ISZ[AadlComponent] = componentMap.values.filter((p: AadlComponent) => p match {
      case process: AadlProcess =>
        process.boundProcessor match {
          case Some(processor) => processor == c.path
          case _ => F
        }
      case _ => F
    })
    return ret.map(m => m.asInstanceOf[AadlProcess])
  }
  
  def getBoundProcessor(c: AadlProcess): Option[AadlProcessor] = {
    val ret: Option[AadlProcessor] = c.boundProcessor match {
      case Some(p) => Some(componentMap.get(p).get.asInstanceOf[AadlProcessor])
      case None() => None()
    }
    return ret
  }

  def getAllBoundProcessors(): ISZ[AadlProcessor] = {
    var processors: Set[AadlProcessor] = Set.empty

    for(process <- getProcesses()){
      getBoundProcessor(process) match {
        case Some(processor) => processors = processors + processor
        case _ => halt(s"Unexpected: ${process.path} does not have a bound processor")
      }
    }
    return processors.elements
  }
}


@sig trait AadlObject

@sig trait AadlComponent extends AadlObject {
  def component: ir.Component
  def parent: Option[String]

  def path: String
  def identifier: String
}

@datatype class AadlSystem(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                          
                           subComponents: ISZ[AadlComponent]) extends AadlComponent

@datatype class AadlProcessor(val component: ir.Component,
                              val parent: Option[String],
                              val path: String,
                              val identifier: String) extends AadlComponent {
  def getFramePeriod(): Option[Z] = {
    val ret: Option[Z] = Util.getDiscreetPropertyValue(component.properties, Util.PROP_Timing_Properties__Frame_Period) match {
      case Some(ir.UnitProp(value, unit)) => Util.convertToMS(value, unit)
      case _ => None()
    }
    return ret
  }

  def getClockPeriod(): Option[Z] = {
    val ret: Option[Z] = Util.getDiscreetPropertyValue(component.properties, Util.PROP_Timing_Properties__Clock_Period) match {
      case Some(ir.UnitProp(value, unit)) => Util.convertToMS(value, unit)
      case _ => None()
    }
    return ret
  }

  def getScheduleSourceText(): Option[String] = {
    val ret: Option[String] = Util.getDiscreetPropertyValue(component.properties, Util.PROP_CASE_Scheduling__Schedule_Source_Text) match {
      case Some(ir.ValueProp(value)) => Some(value)
      case _ => None()
    }
    return ret
  }
}


@datatype class AadlProcess(val component: ir.Component,
                            val parent: Option[String],
                            val path: String,
                            val identifier: String,

                            boundProcessor: Option[String],

                            subComponents: ISZ[AadlComponent],

                           ) extends AadlComponent {
  
  def getDomain(): Option[Z] = {
    return Util.getUnitPropZ(component.properties, Util.PROP_CASE_Scheduling__Domain)
  }
}

@datatype class AadlThread(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,

                           dispatchProtocol: Dispatch_Protocol.Type,
                           period: Option[Z],

                           ports: ISZ[AadlFeature]
                          ) extends AadlComponent {


  def isPeriodic(): B = { return dispatchProtocol == Dispatch_Protocol.Periodic }
  
  def isSporadic(): B = { return dispatchProtocol == Dispatch_Protocol.Sporadic }

  def getMaxComputeExecutionTime(): Z = {
    val ret: Z = Util.getDiscreetPropertyValue(component.properties, Util.PROP_Timing_Properties__Compute_Execution_Time) match {
      case Some(RangeProp(low, high)) => 
        Util.convertToMS(high.value, high.unit) match {
          case Some(z) => z
          case _ => z"0"
        }
      case _ => z"0"
    }
    return ret
  }

  def getDomain(symbolTable: SymbolTable): Option[Z] = {
    val p: AadlProcess = symbolTable.getProcess(parent.get)
    return p.getDomain()
  }

  def getComputeEntrypointSourceText(): Option[String] = {
    return Util.getComputeEntrypointSourceText(component.properties)
  }
}

@datatype class AadlTODOComponent(val component: ir.Component,
                                  val parent: Option[String],
                                  val path: String,
                                  val identifier: String) extends  AadlComponent

@sig trait AadlFeature

@datatype class AadlFeaturePort(feature: ir.Feature) extends AadlFeature

@datatype class AadlFeatureTODO(feature: ir.Feature) extends AadlFeature



object SymbolResolver {
  
  def resolve(model: ir.Aadl,
              basePackageName: Option[String],
              reporter: Reporter): SymbolTable = {
    
    var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty
    
    //var topLevelProcess: Option[ir.Component] = None[ir.Component]
    //var typeHeaderFileName: String = ""
    
    // port-paths -> connInstances
    var inConnections: HashSMap[String, ISZ[ir.ConnectionInstance]] = HashSMap.empty
    var outConnections: HashSMap[String, ISZ[ir.ConnectionInstance]] = HashSMap.empty


    val components = model.components.filter(f => f.category != ir.ComponentCategory.Data)
    if (components.size != z"1" || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }
    val system = components(0)

    def buildComponentMap(c: ir.Component): Unit = {
      val name = Util.getName(c.identifier)
      assert(!airComponentMap.contains(name))
      airComponentMap = airComponentMap + (name ~> c)
      if (c.classifier.nonEmpty) {
        airClassifierMap = airClassifierMap + (c.classifier.get.name ~> c)
      }
      for (f <- c.features) {
        airFeatureMap = airFeatureMap + (Util.getName(f.identifier) ~> f)

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
    }

    buildComponentMap(system)

    def resolver(sys: ir.Component): B = {

      for (c <- airComponentMap.values) {

        for (ci <- c.connectionInstances) {
          if (isHandledConnection(ci, airComponentMap, airFeatureMap)) {
            def add(portPath: String, isIn: B): Unit = {
              val map: HashSMap[String, ISZ[ir.ConnectionInstance]] = isIn match {
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

            val portNames = getPortConnectionNames(ci, airComponentMap)
            add(portNames._1, F)
            add(portNames._2, T)
          } else {
            val src = airComponentMap.get(Util.getName(ci.src.component)).get
            val dst = airComponentMap.get(Util.getName(ci.dst.component)).get

            val srcName: String = if (ci.src.feature.nonEmpty) {
              s"${Util.getLastName(src.identifier)}.${Util.getLastName(ci.src.feature.get)}"
            } else {
              Util.getLastName(src.identifier)
            }

            val dstName: String = if (ci.dst.feature.nonEmpty) {
              s"${Util.getLastName(dst.identifier)}.${Util.getLastName(ci.dst.feature.get)}"
            } else {
              Util.getLastName(dst.identifier)
            }
            reporter.info(None(), Util.toolName, s"Ignoring ${src.category} to ${dst.category} connection: ${srcName} -> ${dstName}")
          }
        }
      }
      
      return !reporter.hasError
    }

    resolver(system)
        
    var componentMap: HashSMap[String, AadlComponent] = HashSMap.empty
    
    def process(c: ir.Component, parent: Option[String]): AadlComponent = {
      val path = Util.getName(c.identifier)
      val identifier = Util.getLastName(c.identifier)
      
      if(componentMap.contains(path)) {
        return componentMap.get(path).get
      }  
      
      val aadlComponent: AadlComponent = c.category match {
        case ir.ComponentCategory.System =>
          val subComponents: ISZ[AadlComponent] = for(sc <- c.subComponents) yield process(sc, Some(path))

          AadlSystem(component = c, parent = parent, path = path, identifier = identifier, subComponents = subComponents)
          
        case ir.ComponentCategory.Processor =>
          AadlProcessor(component = c, parent = None(), path = path, identifier = identifier)
          
        case ir.ComponentCategory.Process =>
          val subComponents: ISZ[AadlComponent] = for(sc <- c.subComponents) yield process(sc, Some(path))
          
          val processor: Option[String] = Util.getActualProcessorBinding(c)
          
          AadlProcess(component = c, parent = parent, path = path, identifier = identifier, boundProcessor = processor, subComponents = subComponents)
          
        case ir.ComponentCategory.Thread =>
          assert(c.subComponents.isEmpty) // TODO handle subprograms etc
          var aadlPorts: ISZ[AadlFeature] = ISZ()
          for(feature <- c.features) {
            val fname = Util.getName(feature.identifier)
            val _feature = airFeatureMap.get(fname).get 
            val port: AadlFeature = _feature.category match {
              case ir.FeatureCategory.EventDataPort => AadlFeaturePort(_feature)
              case ir.FeatureCategory.EventPort => AadlFeaturePort(_feature)
              case ir.FeatureCategory.DataPort => AadlFeaturePort(_feature)
              case _ => AadlFeatureTODO(_feature) 
            }
            aadlPorts = aadlPorts :+ port
          }
          val dispatchProtocol: Dispatch_Protocol.Type = Util.getDispatchProtocol(c) match {
            case Some(x) => x
            case _ =>
              // TODO should be handled by a rewriter  
              reporter.warn(None(), Util.toolName, "Dispatch Protocol not specified, assuming Sporadic")
              Dispatch_Protocol.Sporadic
          }
          
          val period = Util.getPeriod(c)
          
          AadlThread(component = c, parent = parent,
            path = path, identifier = identifier,
            dispatchProtocol = dispatchProtocol, period = period,
            ports = aadlPorts)
          
        case _ => AadlTODOComponent(component = c, parent = parent, path = path, identifier = identifier)
      }
      
      componentMap = componentMap + (path ~> aadlComponent)
      return aadlComponent
    }
    
    val aadlSystem = process(system, None()).asInstanceOf[AadlSystem]
    
    
    return SymbolTable(rootSystem = aadlSystem,

      componentMap = componentMap,

      airComponentMap = airComponentMap,
      airFeatureMap = airFeatureMap,
      airClassifierMap = airClassifierMap,
      
      inConnections = inConnections,
      outConnections = outConnections)
  }

  def getPortConnectionNames(c: ir.ConnectionInstance, 
                             componentMap: HashSMap[String, ir.Component]): (String, String) = {
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
  
  def isHandledConnection(c: ir.ConnectionInstance, 
                          componentMap: HashSMap[String, ir.Component],
                          featureMap: HashSMap[String, ir.Feature]): B = {

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

      val ret: B = (src.category, dst.category) match {
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
