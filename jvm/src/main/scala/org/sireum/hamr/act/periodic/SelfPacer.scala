// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act._
import org.sireum.hamr.act.proof.ProofContainer.CAmkESConnectionType
import org.sireum.hamr.act.proof.ProofUtil
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlProcessor, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil

@datatype class SelfPacer (val symbolTable: SymbolTable,
                           val actOptions: ActOptions) extends PeriodicImpl {

  val performHamrIntegration: B = Util.hamrIntegration(actOptions.platform)

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String): CamkesAssemblyContribution = {

    val threads = symbolTable.getThreads()

    var configurations: ISZ[ast.Configuration] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var auxResources: ISZ[Resource] = ISZ()

    if(threads.nonEmpty) {
      auxResources = auxResources ++ getSchedule(threads)
    }

    val periodicThreads = symbolTable.getPeriodicThreads()

    for(aadlThread <- periodicThreads) {
      val componentId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)

      configurations = configurations :+ PacerTemplate.domainConfiguration(componentId, aadlThread.getDomain(symbolTable).get)

      val connection = Util.createConnection(
        connectionCategory = CAmkESConnectionType.SelfPacing,
        connectionName = Util.getConnectionName(connectionCounter.increment()),
        connectionType = Sel4ConnectorTypes.seL4Notification,
        srcComponent = componentId,
        srcFeature = SelfPacerTemplate.selfPacerClientTickIdentifier(),
        dstComponent = componentId,
        dstFeature = SelfPacerTemplate.selfPacerClientTockIdentifier()
      )

      connections = connections :+ connection
    }

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)
    val maxDomain: Z = aadlProcessor.getMaxDomain() match {
      case Some(z) => z + 1
      case _ => symbolTable.computeMaxDomain()
    }
    val settingCmakeEntries: ISZ[ST] = ISZ(PacerTemplate.settings_cmake_entries(maxDomain))

    return CamkesAssemblyContribution(
      connections = connections,
      configurations = configurations,
      settingCmakeEntries = settingCmakeEntries,
      auxResourceFiles = auxResources,

      imports = ISZ(),
      instances = ISZ(),
      cContainers = ISZ())
  }

  def handlePeriodicComponent(aadlThread: AadlThread): (CamkesComponentContributions, CamkesGlueCodeContributions) = {

    assert(aadlThread.isPeriodic())

    val classifier = Util.getClassifier(aadlThread.component.classifier.get)

    var emits: ISZ[ast.Emits] = ISZ()
    var consumes: ISZ[ast.Consumes] = ISZ()

    var gcHeaderMethods: ISZ[ST] = ISZ()

    var gcMethods: ISZ[ST] = ISZ()
    var gcMainPreLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopStartStms: ISZ[ST] = ISZ()
    var gcMainLoopStms: ISZ[ST]= ISZ()
    var gcMainLoopEndStms: ISZ[ST] = ISZ()

    // initial self pacer/period emit
    gcMainPreLoopStms = gcMainPreLoopStms :+ SelfPacerTemplate.selfPacerEmit()

    // self pacer/period wait at start of loop
    gcMainLoopStartStms = gcMainLoopStartStms :+ SelfPacerTemplate.selfPacerWait()

    // self pacer/period emit at end of loop
    gcMainLoopEndStms = gcMainLoopEndStms :+ SelfPacerTemplate.selfPacerEmit()

    if(!performHamrIntegration) {
      // get user defined time triggered method
      Util.getComputeEntrypointSourceText(aadlThread.component.properties) match {
        case Some(handler) =>
          // header method so developer knows required signature
          gcHeaderMethods = gcHeaderMethods :+ st"void ${handler}(const int64_t * in_arg);"

          gcMethods = gcMethods :+ SelfPacerTemplate.wrapPeriodicComputeEntrypoint(classifier, handler)

          gcMainLoopStms = gcMainLoopStms :+ SelfPacerTemplate.callPeriodicComputEntrypoint(classifier, handler)

        case _ =>
          reporter.warn(None(), Util.toolName, s"Periodic thread ${classifier} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
      }
    }

    emits = emits :+ Util.createEmits_SelfPacing(
      aadlThread = aadlThread,
      symbolTable = symbolTable,
      name = SelfPacerTemplate.selfPacerClientTickIdentifier(),
      typ = SelfPacerTemplate.selfPacerTickTockType())

    consumes = consumes :+ Util.createConsumes_SelfPacing(
      aadlThread = aadlThread,
      symbolTable = symbolTable,
      name = SelfPacerTemplate.selfPacerClientTockIdentifier(),
      typ = SelfPacerTemplate.selfPacerTickTockType(),
      optional = F)

    val shell = ast.Component(
      emits = emits,
      consumes = consumes,

      // filler
      control = F, hardware = F, name = "",
      dataports = ISZ(), includes = ISZ(), mutexes = ISZ(), binarySemaphores = ISZ(), semaphores = ISZ(),
      imports = ISZ(), uses = ISZ(), provides = ISZ(), attributes = ISZ(),
      preprocessorIncludes = ISZ(), externalEntities = ISZ()
    )

    val componentContributions = CamkesComponentContributions(shell)

    val glueCodeContributions = CamkesGlueCodeContributions(
      CamkesGlueCodeHeaderContributions(includes = ISZ(), methods = gcHeaderMethods),
      CamkesGlueCodeImplContributions(includes = ISZ(), globals = ISZ(), methods = gcMethods, preInitStatements = ISZ(),
        postInitStatements = ISZ(),
        mainPreLoopStatements = gcMainPreLoopStms,
        mainLoopStartStatements = gcMainLoopStartStms,
        mainLoopStatements = gcMainLoopStms,
        mainLoopEndStatements = gcMainLoopEndStms,
        mainPostLoopStatements = ISZ()
      )
    )

    return (componentContributions, glueCodeContributions)
  }

  def getSchedule(allThreads: ISZ[AadlThread]): ISZ[Resource] = {

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)

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
            case _ => halt(s"Unexpected: Couldn't locate Schedule_Source_Text ${path}")
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

        var threadComments: ISZ[ST] = ISZ()
        var sumExecutionTime = z"0"
        for(p <- allThreads) {
          val domain = p.getDomain(symbolTable).get
          val computeExecutionTime = p.getMaxComputeExecutionTime()
          val comment = Some(st" // ${p.identifier}  ${computeExecutionTime}ms")

          threadComments = threadComments :+
            PacerTemplate.pacerScheduleThreadPropertyComment(p.identifier,
              domain, p.dispatchProtocol, computeExecutionTime, p.period)

          entries = entries :+ PacerTemplate.pacerScheduleEntry(domain, computeExecutionTime / clockPeriod, comment)

          sumExecutionTime = sumExecutionTime + computeExecutionTime
        }

        val pad: Z = (framePeriod - (otherLen + pacerLen + sumExecutionTime)) / clockPeriod
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", pad, Some(st" // pad rest of frame period"))

        PacerTemplate.pacerExampleSchedule(clockPeriod, framePeriod, threadComments, entries)
    }

    return ISZ(ResourceUtil.createResource(path, contents, F))
  }
}
