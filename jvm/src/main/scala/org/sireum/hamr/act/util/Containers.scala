// #Sireum

package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.connections.ConnectorContainer
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.ir

@datatype class ActContainer(rootServer: String,
                             connectors: ISZ[ConnectorContainer],
                             models: ISZ[ast.ASTObject],
                             monitors: ISZ[Monitor],
                             samplingPorts: ISZ[SamplingPortInterface],
                             cContainers: ISZ[C_Container],
                             auxFiles: ISZ[Resource],
                             globalImports: ISZ[String],
                             globalPreprocessorIncludes: ISZ[String],
                             requiresTimeServer: B
                            )

@sig trait Monitor {
  def i: ast.Instance
  def cimplementation: Resource
  def cinclude: Resource
  def index: Z
  def ci: ir.ConnectionInstance
}

@datatype class TB_Monitor (i: ast.Instance,           // camkes monitor
                            interface: ast.Procedure,  // camkes interface
                            providesVarName: String,
                            cimplementation: Resource,
                            cinclude: Resource,
                            index: Z,                  // fan-out index
                            ci: ir.ConnectionInstance  // aadl connection
                           ) extends Monitor

@datatype class Ihor_Monitor (i: ast.Instance,           // camkes monitor
                              interfaceReceiver: ast.Procedure,  // camkes interface
                              interfaceSender: ast.Procedure,  // camkes interface
                              providesReceiverVarName: String,
                              providesSenderVarName: String,
                              cimplementation: Resource,
                              cinclude: Resource,
                              index: Z,                  // fan-out index
                              ci: ir.ConnectionInstance // aadl connection
                             ) extends Monitor


@datatype class C_Container(instanceName: String,
                            componentId: String,

                            cSources: ISZ[Resource],
                            cIncludes: ISZ[Resource],

                            sourceText: ISZ[String],

                            cmakeSOURCES: ISZ[String], // for CMakeLists SOURCES
                            cmakeINCLUDES: ISZ[String], // for CMakeLists INCLUDES
                            cmakeLIBS: ISZ[String] // for CMakeLists LIBS
                           )

@datatype class C_SimpleContainer(cIncludes: ISZ[ST],
                                  cInterface: Option[ST],
                                  cImplementation: Option[ST],
                                  preInits: Option[ST],
                                  postInits: Option[ST],
                                  drainQueues: Option[(ST, ST)])

@datatype class CamkesAssemblyContribution(imports: ISZ[String],
                                           instances: ISZ[ast.Instance],
                                           connections: ISZ[ast.Connection],
                                           configurations: ISZ[ast.Configuration],
                                           cContainers: ISZ[C_Container],
                                           settingCmakeEntries: ISZ[ST],
                                           auxResourceFiles: ISZ[Resource])

@datatype class CamkesComponentContributions(shell: ast.Component)

@datatype class CamkesGlueCodeContributions(header: CamkesGlueCodeHeaderContributions,
                                            impl: CamkesGlueCodeImplContributions)

@datatype class CamkesGlueCodeHeaderContributions(includes: ISZ[String],
                                                  methods: ISZ[ST])

@datatype class CamkesGlueCodeImplContributions(includes: ISZ[String],
                                                globals: ISZ[ST],

                                                methods: ISZ[ST],

                                                preInitStatements: ISZ[ST],
                                                postInitStatements: ISZ[ST],

                                                mainPreLoopStatements: ISZ[ST],

                                                mainLoopStartStatements: ISZ[ST],
                                                mainLoopStatements: ISZ[ST],
                                                mainLoopEndStatements: ISZ[ST],

                                                mainPostLoopStatements: ISZ[ST]
                                               )

@datatype class SamplingPortInterface(name: String,
                                      structName: String,
                                      sel4TypeName: String,
                                      headerFilename: String,
                                      implFilename: String) {

  def headerPath: String = { return s"${Util.getTypeRootPath()}/includes/${headerFilename}" }

  def implPath: String = { return s"${Util.getTypeRootPath()}/src/${implFilename}" }
}

@datatype class SharedData(owner: ir.Component,
                           ownerFeature: Option[ir.FeatureAccess],
                           typ: ir.Classifier,
                           subcomponentId: String)

@datatype class QueueObject(queueName: String,
                            queueSize: Z)

@enum object Sel4ConnectorTypes {
  'seL4GlobalAsynch
  'seL4GlobalAsynchCallback
  'seL4Notification
  'seL4RPCCall
  'seL4RPCDataport
  'seL4SharedData
  'seL4SharedDataWithCaps
  'seL4SerialServer
  'seL4TimeServer
  'seL4VMDTBPassthrough

  'CASE_AADL_EventDataport
}


@datatype class ActResult(val resources: ISZ[Resource])

@enum object ReportKind{
  'Info
  'Warning
  'Error
}

@enum object ActPlatform {
  'SeL4
  'SeL4_Only
  'SeL4_TB
}

@datatype class ActOptions(outputDir: String,
                           auxFiles: Map[String, String],
                           aadlRootDirectory: Option[String],
                           platform: ActPlatform.Type,
                           hamrBasePackageName: Option[String],
                           experimentalOptions: ISZ[String])

@record class Counter() {
  var count: Z = 0 // start at 0 so first is 1 which prevents capability conflict issues

  def increment(): Z = {
    count = count + 1
    return count
  }
}
