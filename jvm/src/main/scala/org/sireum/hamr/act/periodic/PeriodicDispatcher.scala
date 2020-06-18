// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.periodic.PeriodicDispatcherTemplate._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir
import org.sireum.message.Reporter

@datatype class PeriodicDispatcher(val symbolTable: SymbolTable,
                                   val actOptions: ActOptions) extends PeriodicImpl {
  
  val hookupPeriodicComponentsToTimeServer: B = F

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String,
                               reporter: Reporter): CamkesAssemblyContribution = {

    val components = symbolTable.airComponentMap.values
    
    var imports: ISZ[String] = ISZ()
    var instances: ISZ[ast.Instance] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var configurations: ISZ[ST]= ISZ()
    var cContainers: ISZ[C_Container] = ISZ()
    var auxResources: ISZ[Resource] = ISZ()
    
    val periodicComponents = components.filter(c => CommonUtil.isPeriodic(c) && CommonUtil.isThread(c))

    if(periodicComponents.nonEmpty) {
      var periodicDispatcherNotifications: ISZ[ast.Emits] = ISZ()
      var periodicDispatcherCalendars: ISZ[ST] = ISZ()

      for(c <- periodicComponents) {
        val componentId = CommonUtil.getLastName(c.identifier)
        val classifier = Util.getClassifier(c.classifier.get)

        if(hookupPeriodicComponentsToTimeServer) {
          // connect camkes component to time server
          connections = connections :+ Util.createConnection(
            Util.getConnectionName(connectionCounter.increment()),
            Sel4ConnectorTypes.seL4TimeServer,
            componentId, PeriodicDispatcherTemplate.TIMER_ID,
            PeriodicDispatcherTemplate.TIMER_INSTANCE, PeriodicDispatcherTemplate.TIMER_SERVER_TIMER_ID)

          // timer attribute
          configurations = configurations :+
            PeriodicDispatcherTemplate.configurationTimerAttribute(componentId, timerAttributeCounter.increment(), F)
        }

        val componentNotificationName = PeriodicDispatcherTemplate.componentNotificationName(None())
        val dispatcherNotificationName = PeriodicDispatcherTemplate.componentNotificationName(Some(c))

        // emit notification when component's period occurs
        periodicDispatcherNotifications = periodicDispatcherNotifications :+ Emits(
          name = dispatcherNotificationName,
          typ = Util.NOTIFICATION_TYPE)

        // connect dispatcher to component
        connections = connections :+ Util.createConnection(
          Util.getConnectionName(connectionCounter.increment()),
          Sel4ConnectorTypes.seL4Notification,
          PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE, dispatcherNotificationName,
          componentId, componentNotificationName)

        val period: Z = PropertyUtil.getPeriod(c) match {
          case Some(_period) => _period
          case _ =>
            reporter.warn(None(), Util.toolName, s"Period not provided for periodic component ${classifier}, using ${Util.DEFAULT_PERIOD}")
            Util.DEFAULT_PERIOD
        }
        periodicDispatcherCalendars = periodicDispatcherCalendars :+ PeriodicDispatcherTemplate.calendar(c, period)
      }

      // add the dispatcher component

      val dispatchCamkesComponent: ast.Instance = genDispatchCamkesComponent(periodicDispatcherNotifications)

      instances = instances :+ dispatchCamkesComponent

      val dispatchCSource = PeriodicDispatcherTemplate.dispatchComponentCSource(
        headerInclude, periodicDispatcherCalendars)

      cContainers = cContainers :+ C_Container(
        instanceName = dispatchCamkesComponent.component.name,
        componentId = dispatchCamkesComponent.component.name,
        cSources = ISZ(dispatchCSource),
        cIncludes = ISZ(),
        sourceText = ISZ(),
        cmakeSOURCES = ISZ(),
        cmakeINCLUDES = ISZ(),
        cmakeLIBS = ISZ(Util.SBTypeLibrary)
      )

      // connect dispatch timer to time server
      connections = connections :+ Util.createConnection(
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4TimeServer,
        PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE, PeriodicDispatcherTemplate.TIMER_ID_DISPATCHER,
        PeriodicDispatcherTemplate.TIMER_INSTANCE, PeriodicDispatcherTemplate.TIMER_SERVER_TIMER_ID)

      // connect notification/callback from time server to dispatch timer
      connections = connections :+ Util.createConnection(
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4GlobalAsynchCallback,
        PeriodicDispatcherTemplate.TIMER_INSTANCE, PeriodicDispatcherTemplate.TIMER_SERVER_NOTIFICATION_ID,
        PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE, PeriodicDispatcherTemplate.TIMER_NOTIFICATION_DISPATCHER_ID)

      configurations = configurations :+ st"${PeriodicDispatcherTemplate.TIMER_INSTANCE}.timers_per_client = 1;"

      configurations = configurations :+ PeriodicDispatcherTemplate.configurationTimerAttribute(dispatchCamkesComponent.name,
        timerAttributeCounter.increment(), T)

      configurations = configurations :+ StringTemplate.configurationPriority(dispatchCamkesComponent.name, Util.DEFAULT_PRIORITY)

      imports = imports ++ ISZ(Util.camkesStdConnectors, Util.camkesGlobalConnectors, PeriodicDispatcherTemplate.TIMER_SERVER_IMPORT)
    }

    val settingsCmakeEntries: ISZ[ST] = ISZ()
    return CamkesAssemblyContribution(imports, instances, connections, configurations, cContainers,
      settingsCmakeEntries, auxResources)
  }


  def handlePeriodicComponent(aadlThread: AadlThread,
                              reporter: Reporter): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    val component = aadlThread.component
    val classifier = Util.getClassifier(component.classifier.get)

    var imports: ISZ[String] = ISZ()
    var uses: ISZ[ast.Uses] = ISZ()
    var consumes: ISZ[ast.Consumes] = ISZ()

    var gcHeaderMethods: ISZ[ST] = ISZ()

    var gcMethods: ISZ[ST] = ISZ()

    var gcMainPreInitStatements: ISZ[ST] = ISZ()

    var gcMainPreLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopStms: ISZ[ST]= ISZ()

    // import Timer.idl4
    imports = imports :+ Util.camkesStdConnectors

    if(hookupPeriodicComponentsToTimeServer) {
      // uses Timer tb_timer;
      uses = uses :+ Uses(
        name = PeriodicDispatcherTemplate.TIMER_ID,
        typ = PeriodicDispatcherTemplate.TIMER_TYPE,
        optional = F)
    }

    // consumes Notification from periodic dispatcher
    consumes = consumes :+ Consumes(
      name = PeriodicDispatcherTemplate.componentNotificationName(None()),
      typ = Util.NOTIFICATION_TYPE,
      optional = F)

    gcMethods = gcMethods :+ PeriodicDispatcherTemplate.periodicDispatchElems(classifier, hookupPeriodicComponentsToTimeServer)

    gcMainPreInitStatements = gcMainPreInitStatements :+ PeriodicDispatcherTemplate.registerPeriodicCallback()

    Util.getComputeEntrypointSourceText(component.properties) match {
      case Some(handler) =>
        gcHeaderMethods = gcHeaderMethods :+ st"void ${handler}(const int64_t *);"

        val drains = PeriodicDispatcherTemplate.drainPeriodicQueue(classifier, handler)

        gcMethods = gcMethods :+ drains._1

        gcMainLoopStms = gcMainLoopStms :+ drains._2

      case _ => 
        reporter.warn(None(), Util.toolName, s"Periodic thread ${classifier} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
    }

    val shell = ast.Component(
      imports = imports,
      uses = uses,
      consumes = consumes,

      // filler
      control = F, hardware = F, name = "", mutexes = ISZ(), binarySemaphores = ISZ(), semaphores = ISZ(),
      dataports = ISZ(), emits = ISZ(), provides = ISZ(), includes = ISZ(), attributes = ISZ(),
      preprocessorIncludes = ISZ(), externalEntities = ISZ()
    )
    val componentContributions = CamkesComponentContributions(shell)

    val glueCodeContributions = CamkesGlueCodeContributions(
      CamkesGlueCodeHeaderContributions(includes = ISZ(), methods = gcHeaderMethods),
      CamkesGlueCodeImplContributions(includes = ISZ(), globals = ISZ(), methods = gcMethods,
        preInitStatements = gcMainPreInitStatements,
        postInitStatements = ISZ(), 

        mainPreLoopStatements = gcMainPreLoopStms,
        mainLoopStartStatements = ISZ(),
        mainLoopStatements = gcMainLoopStms,
        mainLoopEndStatements = ISZ(),
        mainPostLoopStatements = ISZ())
    )

    return (componentContributions, glueCodeContributions)
  }

  def genDispatchCamkesComponent(notifications: ISZ[ast.Emits]): ast.Instance = {
    val i = ast.Instance(
      address_space = "",
      name = DISPATCH_PERIODIC_INSTANCE,
      component = ast.Component(
        control = T,
        hardware = F,
        name = DISPATCH_CLASSIFIER,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = ISZ(),
        emits = notifications,
        uses = ISZ(ast.Uses(
          name = DISPATCH_TIMER_ID,
          typ = TIMER_TYPE,
          optional = F)),
        consumes = ISZ(ast.Consumes(
          name = TIMER_NOTIFICATION_DISPATCHER_ID,
          typ = Util.NOTIFICATION_TYPE,
          optional = F)),
        provides = ISZ(),
        includes = ISZ(),
        attributes = ISZ(),
        preprocessorIncludes = ISZ(),
        imports = ISZ(Util.camkesGlobalConnectors),
        externalEntities = ISZ()
      ))
    return i
  }

  def timerComponent(): ast.Instance = {
    val i = ast.Instance(
      address_space = "",
      name = TIMER_INSTANCE,
      component = ast.Component(
        control = F,
        hardware = F,
        name = TIMER_SERVER_CLASSIFIER,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = ISZ(),
        emits = ISZ(ast.Emits(
          name = TIMER_SERVER_NOTIFICATION_ID,
          typ = Util.NOTIFICATION_TYPE)),
        uses = ISZ(),
        consumes = ISZ(),
        provides = ISZ(ast.Provides(
          name = TIMER_SERVER_TIMER_ID,
          typ = TIMER_TYPE)),
        includes = ISZ(),
        attributes = ISZ(),
        preprocessorIncludes = ISZ(),
        imports = ISZ(),
        externalEntities = ISZ()
      )
    )
    return i
  }


}
