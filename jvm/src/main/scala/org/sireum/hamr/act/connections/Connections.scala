// #Sireum

package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.util.ExperimentalOptions
import org.sireum.hamr.ir

@datatype class ConnectionHolder(connectionName: String,
                                 connectionType: Sel4ConnectorTypes.Type,
                                 toConnectionEnds: ISZ[ast.ConnectionEnd],
                                 configurationEntries: ISZ[String])

@datatype class ConnectionContainer(connections: ISZ[ast.Connection],
                                    configurationEntries: ISZ[ST],
                                    optConnectorHolder: ISZ[ConnectorContainer])

@datatype class ConnectorContainer(connectorName: String,
                                   assemblyEntry: ast.Connector,
                                   connectorTemplate: ConnectorTemplate)

@datatype class ConnectorTemplate(fromTemplateName: String,
                                  fromTemplate: Option[ST],
                                  toTemplateName: String,
                                  toTemplate: Option[ST])

object Connections {
  def useCaseEventDataPortConnector(experimentalOptions: IS[Z, String]): B = {
    return ExperimentalOptions.useCaseConnectors(experimentalOptions)
  }

  def isHandledConnection(c: ir.ConnectionInstance,
                          symbolTable: SymbolTable): B = {

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
    val src = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get
    val dst = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get

    val ret : B = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>

        val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(c.src.feature.get)).get
        val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(c.dst.feature.get)).get

        validFeature(srcFeature) && validFeature(dstFeature)

      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(c.dst.feature.get)).get
        validFeature(dstFeature)

      case _ =>
        F
    }

    return ret
  }


  def getPortConnectionNames(c: ir.ConnectionInstance,
                             symbolTable: SymbolTable): (String, String) = {
    val src = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get
    val dst = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get

    val ret: (String, String) = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>
        (CommonUtil.getName(c.src.feature.get), CommonUtil.getName(c.dst.feature.get))
      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        (CommonUtil.getName(c.src.component), CommonUtil.getName(c.dst.feature.get))

      case _ => halt(s"Unexpected connection: ${c}")
    }
    return ret
  }

  def createConnection(connectionCounter: Counter,
                       connectionType: Sel4ConnectorTypes.Type,
                       srcComponent: String, srcFeature: String,
                       dstComponent: String, dstFeature: String): ast.Connection = {
    val connectionName = Util.getConnectionName(connectionCounter.increment())
    return Util.createConnection(connectionName, connectionType, srcComponent, srcFeature, dstComponent, dstFeature)
  }
}
