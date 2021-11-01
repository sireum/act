// #Sireum
package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.ast.LibraryComponent

object LibraryComponents {

  object SerialServer {
    val defaultSerialServerName: String = "serial"

    val getchar_Port: String = "getchar"
    val processed_batch_Port: String = "processed_batch"
    val timeout_Port: String = "timeout"

    val ports: ISZ[String] = ISZ(getchar_Port, processed_batch_Port, timeout_Port)

    val defaultSerialServerInstance: ast.Instance = createInstance(defaultSerialServerName)

    def createInstance(instanceName: String): ast.Instance = {
      Util.createCAmkESInstance(
        originAadl = None(),

        address_space = "",
        name = instanceName,
        component = LibraryComponent(name = "SerialServer", ports = ports))
    }
  }

  object TimeServer {
    val the_timer_port: String = "the_timer"

    val ports: ISZ[String] = ISZ(the_timer_port)

    val defaultTimeServerInstance: ast.Instance = createInstance("time_server")

    def createInstance(instanceName: String): ast.Instance = {
      Util.createCAmkESInstance(
        originAadl = None(),

        address_space = "",
        name = instanceName,
        component = LibraryComponent(name = "TimeServer", ports = ports))
    }
  }
}
