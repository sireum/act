// #Sireum
package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.ast.{AstComment, LibraryComponent}
import org.sireum.hamr.act.proof.ProofContainer.CAmkESComponentCategory

object LibraryComponents {

  object SerialServer {
    val defaultSerialServerName: String = "serial"

    val getchar_Port: String = "getchar"
    val processed_batch_Port: String = "processed_batch"
    val timeout_Port: String = "timeout"

    val ports: ISZ[String] = ISZ(getchar_Port, processed_batch_Port, timeout_Port)

    def defaultSerialServerInstance(comments: ISZ[AstComment]): ast.Instance = {
      return createInstance(defaultSerialServerName, comments)
    }

    def libraryComponent: LibraryComponent = {
      return Util.createCAmkESLibraryComponent(
        componentCategory = CAmkESComponentCategory.SerialServer, name = "SerialServer", ports = ports)
    }

    def createInstance(instanceName: String, comments: ISZ[AstComment]): ast.Instance = {
      return Util.createCAmkESInstance(
        originAadl = None(),

        address_space = "",
        name = instanceName,
        component = libraryComponent,
        comments = comments)
    }
  }

  object TimeServer {
    val defaultTimeServerName: String = "time_server"

    val the_timer_port: String = "the_timer"

    val ports: ISZ[String] = ISZ(the_timer_port)

    def defaultTimeServerInstance(comments: ISZ[AstComment]): ast.Instance = {
      return createInstance(defaultTimeServerName, comments)
    }

    def libraryComponent: LibraryComponent = {
      return Util.createCAmkESLibraryComponent(
        componentCategory = CAmkESComponentCategory.TimeServer, name = "TimeServer", ports = ports)
    }

    def createInstance(instanceName: String, comments: ISZ[AstComment]): ast.Instance = {
      return Util.createCAmkESInstance(
        originAadl = None(),

        address_space = "",
        name = instanceName,
        component = libraryComponent,

        comments = comments)
    }
  }

  object FileServer {
    val defaultFileServerName: String = "fserv"

    val fs_ctrl_port: String = "fs_ctrl"

    val ports: ISZ[String] = ISZ(fs_ctrl_port)

    def defaultFileServerInstance(comments: ISZ[AstComment]): ast.Instance = {
      return createInstance(defaultFileServerName, comments)
    }

    def libraryComponent: LibraryComponent = {
      return Util.createCAmkESLibraryComponent(
        componentCategory = CAmkESComponentCategory.FileServer, name = "FileServer", ports = ports)
    }

    def createInstance(instanceName: String, comments: ISZ[AstComment]): ast.Instance = {
      return Util.createCAmkESInstance(
        originAadl = None(),

        address_space = "",
        name = instanceName,
        component = libraryComponent,
        comments = comments)
    }
  }
}
