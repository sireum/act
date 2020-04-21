// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.ast.{Consumes, Emits}
import org.sireum.hamr.act.{ActOptions, C_Container, CamkesAssemblyContribution, CamkesComponentContributions, CamkesGlueCodeContributions, CamkesGlueCodeHeaderContributions, CamkesGlueCodeImplContributions, Counter, Resource, Sel4ConnectorTypes, StringTemplate, Util, ast}
import org.sireum.hamr.act.util.{AadlProcessor, AadlThread, SymbolTable}
import org.sireum.message.Reporter

@datatype class Pacer(val symbolTable: SymbolTable,
                      val actOptions: ActOptions) extends PeriodicImpl {

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String,
                               reporter: Reporter): CamkesAssemblyContribution = {
    var imports: ISZ[String] = ISZ()
    var instances: ISZ[ast.Instance] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var configurations: ISZ[ST]= ISZ()
    var cContainers: ISZ[C_Container] = ISZ()
    var auxResources: ISZ[Resource] = ISZ()

    imports = imports :+ PacerTemplate.pacerImport()

    val periodicComponents = symbolTable.getPeriodicThreads()
    
    if(periodicComponents.nonEmpty) {

      auxResources = auxResources ++ getSchedule(periodicComponents)
      
      // TODO handle components with different periods
      var emitPeriods: ISZ[ast.Emits] = ISZ(emitPeriod(PacerTemplate.PACER_PERIOD_IDENTIFIER))
      var gcEmitPeriods: ISZ[ST] = ISZ(gcEmitPeriod(PacerTemplate.PACER_PERIOD_IDENTIFIER))
      
      // add pacer domain configuration
      configurations = configurations :+ PacerTemplate.pacerDomainConfiguration(PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_DOMAIN)
      
      // tick/tock connection
      connections = connections :+ Util.createConnection(
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4Notification,
        PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_TICK_IDENTIFIER,
        PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_TOCK_IDENTIFIER)

      for (aadlThread <- periodicComponents) {
        val componentId = aadlThread.identifier

        configurations = configurations :+ PacerTemplate.pacerDomainConfiguration(componentId, aadlThread.getDomain(symbolTable).get)
        
        // connect dispatcher to component
        connections = connections :+ Util.createConnection(
          Util.getConnectionName(connectionCounter.increment()),
          Sel4ConnectorTypes.seL4Notification,
          PacerTemplate.PACER_IDENTIFIER, PacerTemplate.pacerPeriodIdentifier(),
          componentId, PacerTemplate.pacerClientNotificationIdentifier())
      }
      
      val pacerComponent: ast.Instance = genPacerCamkesComponent(emitPeriods)
      
      instances = instances :+ pacerComponent
      
      val pacerCCode: Resource = genPacerGlueCode(gcEmitPeriods)
      
      cContainers = cContainers :+ C_Container(
        instanceName = pacerComponent.component.name,
        componentId = pacerComponent.component.name,
        cSources = ISZ(pacerCCode),
        cIncludes = ISZ(),
        sourceText = ISZ(),
        externalCSources = ISZ(),
        externalCIncludeDirs = ISZ()
      )
    }
    
    return CamkesAssemblyContribution(imports, instances, connections, configurations, cContainers, auxResources)
  }

  def handlePeriodicComponent(aadlThread: AadlThread,
                              reporter: Reporter): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    val component = aadlThread.component
    val classifier = Util.getClassifier(component.classifier.get)
    
    var consumes: ISZ[ast.Consumes] = ISZ()
    
    var gcHeaderMethods: ISZ[ST] = ISZ()

    var gcMethods: ISZ[ST] = ISZ()
    var gcMainPreLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopStartStms: ISZ[ST] = ISZ()
    var gcMainLoopStms: ISZ[ST]= ISZ()

    // get user defined time triggered method 
    Util.getComputeEntrypointSourceText(component.properties) match {
      case Some(handler) =>
        // header method so developer knows required signature
        gcHeaderMethods = gcHeaderMethods :+ st"void ${handler}(const int64_t * in_arg);"
        
        // initial pacer/period wait
        gcMainPreLoopStms = gcMainPreLoopStms :+ PacerTemplate.pacerWait()

        // pacer/period wait at start of loop
        gcMainLoopStartStms = gcMainLoopStartStms :+ PacerTemplate.pacerWait()
                
        gcMethods = gcMethods :+ PacerTemplate.wrapPeriodicComputeEntrypoint(classifier, handler) 
        
        gcMainLoopStms = gcMainLoopStms :+ PacerTemplate.callPeriodicComputEntrypoint(classifier, handler)

      case _ =>
        reporter.warn(None(), Util.toolName, s"Periodic thread ${classifier} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
    }
    
    consumes = consumes :+ ast.Consumes(
      name = PacerTemplate.pacerClientNotificationIdentifier(),
      typ = "Period",
      optional = F)

    val shell = ast.Component(
      consumes = consumes,

      // filler
      control = F, hardware = F, name = "", mutexes = ISZ(), binarySemaphores = ISZ(), semaphores = ISZ(),
      imports = ISZ(), emits = ISZ(), dataports = ISZ(), uses = ISZ(), provides = ISZ(), includes = ISZ(), attributes = ISZ()
    )
    
    val componentContributions = CamkesComponentContributions(shell)

    val glueCodeContributions = CamkesGlueCodeContributions(
      CamkesGlueCodeHeaderContributions(includes = ISZ(), methods = gcHeaderMethods),
      CamkesGlueCodeImplContributions(includes = ISZ(), globals = ISZ(), methods = gcMethods, preInitStatements = ISZ(),
        postInitStatements = ISZ(), 
        mainPreLoopStatements = gcMainPreLoopStms, 
        mainLoopStartStatements = gcMainLoopStartStms, mainLoopStatements = gcMainLoopStms, mainLoopEndStatements = ISZ(),
        mainPostLoopStatements = ISZ()
      )
    )

    return (componentContributions, glueCodeContributions)
  }

  def emitPeriod(id: String): Emits = {
    return Emits(name = id,
      typ = PacerTemplate.PACER_PERIOD_TYPE)
  }

  def gcEmitPeriod(id: String): ST = { return st"${id}_emit()" }
  
  def genPacerCamkesComponent(periods: ISZ[Emits]): ast.Instance = {
    val emits: Emits = Emits(
      name = PacerTemplate.PACER_TICK_IDENTIFIER,
      typ = PacerTemplate.PACER_TICK_TOCK_TYPE)

    val consumes: Consumes = Consumes(
      name = PacerTemplate.PACER_TOCK_IDENTIFIER,
      typ = PacerTemplate.PACER_TICK_TOCK_TYPE,
      optional = F)

    val instance: ast.Instance = ast.Instance(
      address_space = "",
      name = PacerTemplate.PACER_IDENTIFIER,
      component = ast.Component(
        control =  T,
        hardware = F,
        name = PacerTemplate.PACER_COMPONENT_TYPE,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = ISZ(),
        emits = periods :+ emits,
        uses = ISZ(),
        consumes = ISZ(consumes),
        provides = ISZ(),
        includes = ISZ(),
        attributes = ISZ(),
        imports = ISZ()
      )
    )
    return instance
  }

  def genPacerGlueCode(periods: ISZ[ST]): Resource = {
    val glueCode = PacerTemplate.pacerGlueCode(periods)
 
    return Util.createResource(PacerTemplate.pacerGlueCodePath(), glueCode, T)
  }
  
  def getSchedule(periodicComponents: ISZ[AadlThread]): ISZ[Resource] = {

    val aadlProcessors: ISZ[AadlProcessor] = symbolTable.getAllBoundProcessors()
    if(aadlProcessors.isEmpty || aadlProcessors.size > 1) {
      // TODO: ?? 
      halt(s"Unexpected: Expecting exactly one bound processor but found ${aadlProcessors.size}")
    }
    val aadlProcessor = aadlProcessors(0)

    val path = "kernel/domain_schedule.c"

    val contents: ST = aadlProcessor.getScheduleSourceText() match {
      case Some(path) =>
        if(Os.path(path).exists) {
          val p = Os.path(path)
          st"${p.read}"
        } else {
          actOptions.aadlRootDirectory match {
            case Some(root) =>
              val candidate = Os.path(root) / path
              if(candidate.exists) {
                st"${candidate.read}"
              } else {
                halt(s"Could not locate Schedule_Source_Text ${candidate}")
              }
            case None() => halt(s"Unexpected: Couldn't locate Schedule_Source_Text ${path}")
          }
        }
      case _ => 
        var entries: ISZ[ST] = ISZ()

        val clockPeriod: Z = aadlProcessor.getClockPeriod() match {
          case Some(z) => z
          case _ => halt("Unexpected: Clock_Period not specified")
        }
        
        val framePeriod: Z = aadlProcessor.getFramePeriod() match {
          case Some(z) => z
          case _ => halt("Unexpected: Frame_Period not specified")
        }
        
        val otherLen = z"200"
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", otherLen / clockPeriod, 
          Some(st" // all other seL4 threads, init, ${otherLen}ms"))
        
        val pacerLen = z"10" 
        val pacerDomain = PacerTemplate.PACER_DOMAIN
        entries = entries :+ PacerTemplate.pacerScheduleEntry(pacerDomain, pacerLen / clockPeriod, 
          Some(st" // pacer ${pacerLen}ms.  Should always be in domain ${pacerDomain}"))
        
        var threadComments: ISZ[ST] = ISZ()
        var sumExecutionTime = z"0"
        for(p <- periodicComponents) {
          val domain = p.getDomain(symbolTable).get
          val computeExecutionTime = p.getMaxComputeExecutionTime()
          val period = p.period
          val comment = Some(st" // ${p.identifier}  ${computeExecutionTime}ms")

          threadComments = threadComments :+ 
            PacerTemplate.pacerScheduleThreadPropertyComment(p.identifier,
              ISZ(st"${Util.PROP_CASE_Scheduling__Domain} : ${domain}",
                st"${Util.PROP_Timing_Properties__Compute_Execution_Time} : ${computeExecutionTime} ms",
                st"${Util.PROP_Timing_Properties__Period} : ${period} ms"))  
          
          entries = entries :+ PacerTemplate.pacerScheduleEntry(domain, computeExecutionTime / clockPeriod, comment)
          
          sumExecutionTime = sumExecutionTime + computeExecutionTime
        }
        
        val pad: Z = (framePeriod - (otherLen + pacerLen + sumExecutionTime)) / clockPeriod
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", pad, Some(st" // pad rest of frame period"))
        
        PacerTemplate.pacerExampleSchedule(clockPeriod, framePeriod, threadComments, entries)
    }
    
    val maxDomain = symbolTable.getMaxDomain()
    val settingsCmake = Util.createResource("settings.cmake", PacerTemplate.settingsCmake(maxDomain), F)
    
    return ISZ(settingsCmake, Util.createResource(path, contents, F))
  }
}
