// #Sireum
// @formatter:off

/*
 Copyright (c) 2018, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// This file is auto-generated from ActAst.scala

package org.sireum.aadl.act.ast

import org.sireum._
import org.sireum.Json.Printer._

object JSON {

  object Printer {

    @pure def printASTObject(o: ASTObject): ST = {
      o match {
        case o: Assembly => return printAssembly(o)
        case o: Composition => return printComposition(o)
        case o: Instance => return printInstance(o)
        case o: Component => return printComponent(o)
        case o: Connection => return printConnection(o)
        case o: ConnectionEnd => return printConnectionEnd(o)
        case o: Connector => return printConnector(o)
        case o: Procedure => return printProcedure(o)
        case o: Method => return printMethod(o)
        case o: Parameter => return printParameter(o)
        case o: BinarySemaphore => return printBinarySemaphore(o)
        case o: TODO => return printTODO(o)
      }
    }

    @pure def printAssembly(o: Assembly): ST = {
      return printObject(ISZ(
        ("type", st""""Assembly""""),
        ("configuration", printString(o.configuration)),
        ("composition", printComposition(o.composition))
      ))
    }

    @pure def printComposition(o: Composition): ST = {
      return printObject(ISZ(
        ("type", st""""Composition""""),
        ("groups", printISZ(F, o.groups, printTODO _)),
        ("exports", printISZ(F, o.exports, printTODO _)),
        ("instances", printISZ(F, o.instances, printInstance _)),
        ("connections", printISZ(F, o.connections, printConnection _))
      ))
    }

    @pure def printInstance(o: Instance): ST = {
      return printObject(ISZ(
        ("type", st""""Instance""""),
        ("address_space", printString(o.address_space)),
        ("name", printString(o.name)),
        ("component", printComponent(o.component))
      ))
    }

    @pure def printComponent(o: Component): ST = {
      return printObject(ISZ(
        ("type", st""""Component""""),
        ("control", printB(o.control)),
        ("hardware", printB(o.hardware)),
        ("name", printString(o.name)),
        ("mutexes", printISZ(F, o.mutexes, printTODO _)),
        ("binarySemaphores", printISZ(F, o.binarySemaphores, printBinarySemaphore _)),
        ("semaphores", printISZ(F, o.semaphores, printTODO _)),
        ("dataports", printISZ(F, o.dataports, printDataport _)),
        ("emits", printISZ(F, o.emits, printEmits _)),
        ("uses", printISZ(F, o.uses, printUses _)),
        ("consumes", printISZ(F, o.consumes, printConsumes _)),
        ("provides", printISZ(F, o.provides, printProvides _)),
        ("includes", printISZ(T, o.includes, printString _)),
        ("attributes", printISZ(F, o.attributes, printTODO _)),
        ("imports", printISZ(T, o.imports, printString _))
      ))
    }

    @pure def printUses(o: Uses): ST = {
      return printObject(ISZ(
        ("type", st""""Uses""""),
        ("name", printString(o.name)),
        ("typ", printString(o.typ)),
        ("optional", printB(o.optional))
      ))
    }

    @pure def printProvides(o: Provides): ST = {
      return printObject(ISZ(
        ("type", st""""Provides""""),
        ("name", printString(o.name)),
        ("typ", printString(o.typ))
      ))
    }

    @pure def printEmits(o: Emits): ST = {
      return printObject(ISZ(
        ("type", st""""Emits""""),
        ("name", printString(o.name)),
        ("typ", printString(o.typ))
      ))
    }

    @pure def printConsumes(o: Consumes): ST = {
      return printObject(ISZ(
        ("type", st""""Consumes""""),
        ("name", printString(o.name)),
        ("typ", printString(o.typ)),
        ("optional", printB(o.optional))
      ))
    }

    @pure def printDataport(o: Dataport): ST = {
      return printObject(ISZ(
        ("type", st""""Dataport""""),
        ("name", printString(o.name)),
        ("typ", printString(o.typ)),
        ("optional", printB(o.optional))
      ))
    }

    @pure def printConnection(o: Connection): ST = {
      return printObject(ISZ(
        ("type", st""""Connection""""),
        ("name", printString(o.name)),
        ("connectionType", printString(o.connectionType)),
        ("from_ends", printISZ(F, o.from_ends, printConnectionEnd _)),
        ("to_ends", printISZ(F, o.to_ends, printConnectionEnd _))
      ))
    }

    @pure def printConnectionEnd(o: ConnectionEnd): ST = {
      return printObject(ISZ(
        ("type", st""""ConnectionEnd""""),
        ("isFrom", printB(o.isFrom)),
        ("component", printString(o.component)),
        ("end", printString(o.end))
      ))
    }

    @pure def printConnectorTypeType(o: ConnectorType.Type): ST = {
      val value: String = o match {
        case ConnectorType.Event => "Event"
        case ConnectorType.Events => "Events"
        case ConnectorType.Procedure => "Procedure"
        case ConnectorType.Procedures => "Procedures"
        case ConnectorType.Dataport => "Dataport"
        case ConnectorType.Dataports => "Dataports"
      }
      return printObject(ISZ(
        ("type", printString("ConnectorType")),
        ("value", printString(value))
      ))
    }

    @pure def printConnector(o: Connector): ST = {
      return printObject(ISZ(
        ("type", st""""Connector""""),
        ("name", printString(o.name)),
        ("from_type", printConnectorTypeType(o.from_type)),
        ("from_template", printOption(T, o.from_template, printString _)),
        ("from_threads", printZ(o.from_threads)),
        ("from_hardware", printB(o.from_hardware)),
        ("to_type", printConnectorTypeType(o.to_type)),
        ("to_template", printOption(T, o.to_template, printString _)),
        ("to_threads", printZ(o.to_threads)),
        ("to_hardware", printB(o.to_hardware))
      ))
    }

    @pure def printProcedure(o: Procedure): ST = {
      return printObject(ISZ(
        ("type", st""""Procedure""""),
        ("name", printString(o.name)),
        ("methods", printISZ(F, o.methods, printMethod _)),
        ("includes", printISZ(T, o.includes, printString _))
      ))
    }

    @pure def printMethod(o: Method): ST = {
      return printObject(ISZ(
        ("type", st""""Method""""),
        ("name", printString(o.name)),
        ("parameters", printISZ(F, o.parameters, printParameter _)),
        ("returnType", printOption(T, o.returnType, printString _))
      ))
    }

    @pure def printParameter(o: Parameter): ST = {
      return printObject(ISZ(
        ("type", st""""Parameter""""),
        ("array", printB(o.array)),
        ("direction", printDirectionType(o.direction)),
        ("name", printString(o.name)),
        ("typ", printString(o.typ))
      ))
    }

    @pure def printDirectionType(o: Direction.Type): ST = {
      val value: String = o match {
        case Direction.In => "In"
        case Direction.Out => "Out"
        case Direction.Refin => "Refin"
      }
      return printObject(ISZ(
        ("type", printString("Direction")),
        ("value", printString(value))
      ))
    }

    @pure def printBinarySemaphore(o: BinarySemaphore): ST = {
      return printObject(ISZ(
        ("type", st""""BinarySemaphore""""),
        ("name", printString(o.name))
      ))
    }

    @pure def printTODO(o: TODO): ST = {
      return printObject(ISZ(
        ("type", st""""TODO"""")
      ))
    }

  }

  @record class Parser(input: String) {
    val parser: Json.Parser = Json.Parser.create(input)

    def errorOpt: Option[Json.ErrorMsg] = {
      return parser.errorOpt
    }

    def parseASTObject(): ASTObject = {
      val t = parser.parseObjectTypes(ISZ("Assembly", "Composition", "Instance", "Component", "Connection", "ConnectionEnd", "Connector", "Procedure", "Method", "Parameter", "BinarySemaphore", "TODO"))
      t.native match {
        case "Assembly" => val r = parseAssemblyT(T); return r
        case "Composition" => val r = parseCompositionT(T); return r
        case "Instance" => val r = parseInstanceT(T); return r
        case "Component" => val r = parseComponentT(T); return r
        case "Connection" => val r = parseConnectionT(T); return r
        case "ConnectionEnd" => val r = parseConnectionEndT(T); return r
        case "Connector" => val r = parseConnectorT(T); return r
        case "Procedure" => val r = parseProcedureT(T); return r
        case "Method" => val r = parseMethodT(T); return r
        case "Parameter" => val r = parseParameterT(T); return r
        case "BinarySemaphore" => val r = parseBinarySemaphoreT(T); return r
        case "TODO" => val r = parseTODOT(T); return r
        case _ => val r = parseTODOT(T); return r
      }
    }

    def parseAssembly(): Assembly = {
      val r = parseAssemblyT(F)
      return r
    }

    def parseAssemblyT(typeParsed: B): Assembly = {
      if (!typeParsed) {
        parser.parseObjectType("Assembly")
      }
      parser.parseObjectKey("configuration")
      val configuration = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("composition")
      val composition = parseComposition()
      parser.parseObjectNext()
      return Assembly(configuration, composition)
    }

    def parseComposition(): Composition = {
      val r = parseCompositionT(F)
      return r
    }

    def parseCompositionT(typeParsed: B): Composition = {
      if (!typeParsed) {
        parser.parseObjectType("Composition")
      }
      parser.parseObjectKey("groups")
      val groups = parser.parseISZ(parseTODO _)
      parser.parseObjectNext()
      parser.parseObjectKey("exports")
      val exports = parser.parseISZ(parseTODO _)
      parser.parseObjectNext()
      parser.parseObjectKey("instances")
      val instances = parser.parseISZ(parseInstance _)
      parser.parseObjectNext()
      parser.parseObjectKey("connections")
      val connections = parser.parseISZ(parseConnection _)
      parser.parseObjectNext()
      return Composition(groups, exports, instances, connections)
    }

    def parseInstance(): Instance = {
      val r = parseInstanceT(F)
      return r
    }

    def parseInstanceT(typeParsed: B): Instance = {
      if (!typeParsed) {
        parser.parseObjectType("Instance")
      }
      parser.parseObjectKey("address_space")
      val address_space = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("component")
      val component = parseComponent()
      parser.parseObjectNext()
      return Instance(address_space, name, component)
    }

    def parseComponent(): Component = {
      val r = parseComponentT(F)
      return r
    }

    def parseComponentT(typeParsed: B): Component = {
      if (!typeParsed) {
        parser.parseObjectType("Component")
      }
      parser.parseObjectKey("control")
      val control = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("hardware")
      val hardware = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("mutexes")
      val mutexes = parser.parseISZ(parseTODO _)
      parser.parseObjectNext()
      parser.parseObjectKey("binarySemaphores")
      val binarySemaphores = parser.parseISZ(parseBinarySemaphore _)
      parser.parseObjectNext()
      parser.parseObjectKey("semaphores")
      val semaphores = parser.parseISZ(parseTODO _)
      parser.parseObjectNext()
      parser.parseObjectKey("dataports")
      val dataports = parser.parseISZ(parseDataport _)
      parser.parseObjectNext()
      parser.parseObjectKey("emits")
      val emits = parser.parseISZ(parseEmits _)
      parser.parseObjectNext()
      parser.parseObjectKey("uses")
      val uses = parser.parseISZ(parseUses _)
      parser.parseObjectNext()
      parser.parseObjectKey("consumes")
      val consumes = parser.parseISZ(parseConsumes _)
      parser.parseObjectNext()
      parser.parseObjectKey("provides")
      val provides = parser.parseISZ(parseProvides _)
      parser.parseObjectNext()
      parser.parseObjectKey("includes")
      val includes = parser.parseISZ(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("attributes")
      val attributes = parser.parseISZ(parseTODO _)
      parser.parseObjectNext()
      parser.parseObjectKey("imports")
      val imports = parser.parseISZ(parser.parseString _)
      parser.parseObjectNext()
      return Component(control, hardware, name, mutexes, binarySemaphores, semaphores, dataports, emits, uses, consumes, provides, includes, attributes, imports)
    }

    def parseUses(): Uses = {
      val r = parseUsesT(F)
      return r
    }

    def parseUsesT(typeParsed: B): Uses = {
      if (!typeParsed) {
        parser.parseObjectType("Uses")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("typ")
      val typ = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("optional")
      val optional = parser.parseB()
      parser.parseObjectNext()
      return Uses(name, typ, optional)
    }

    def parseProvides(): Provides = {
      val r = parseProvidesT(F)
      return r
    }

    def parseProvidesT(typeParsed: B): Provides = {
      if (!typeParsed) {
        parser.parseObjectType("Provides")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("typ")
      val typ = parser.parseString()
      parser.parseObjectNext()
      return Provides(name, typ)
    }

    def parseEmits(): Emits = {
      val r = parseEmitsT(F)
      return r
    }

    def parseEmitsT(typeParsed: B): Emits = {
      if (!typeParsed) {
        parser.parseObjectType("Emits")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("typ")
      val typ = parser.parseString()
      parser.parseObjectNext()
      return Emits(name, typ)
    }

    def parseConsumes(): Consumes = {
      val r = parseConsumesT(F)
      return r
    }

    def parseConsumesT(typeParsed: B): Consumes = {
      if (!typeParsed) {
        parser.parseObjectType("Consumes")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("typ")
      val typ = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("optional")
      val optional = parser.parseB()
      parser.parseObjectNext()
      return Consumes(name, typ, optional)
    }

    def parseDataport(): Dataport = {
      val r = parseDataportT(F)
      return r
    }

    def parseDataportT(typeParsed: B): Dataport = {
      if (!typeParsed) {
        parser.parseObjectType("Dataport")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("typ")
      val typ = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("optional")
      val optional = parser.parseB()
      parser.parseObjectNext()
      return Dataport(name, typ, optional)
    }

    def parseConnection(): Connection = {
      val r = parseConnectionT(F)
      return r
    }

    def parseConnectionT(typeParsed: B): Connection = {
      if (!typeParsed) {
        parser.parseObjectType("Connection")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("connectionType")
      val connectionType = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("from_ends")
      val from_ends = parser.parseISZ(parseConnectionEnd _)
      parser.parseObjectNext()
      parser.parseObjectKey("to_ends")
      val to_ends = parser.parseISZ(parseConnectionEnd _)
      parser.parseObjectNext()
      return Connection(name, connectionType, from_ends, to_ends)
    }

    def parseConnectionEnd(): ConnectionEnd = {
      val r = parseConnectionEndT(F)
      return r
    }

    def parseConnectionEndT(typeParsed: B): ConnectionEnd = {
      if (!typeParsed) {
        parser.parseObjectType("ConnectionEnd")
      }
      parser.parseObjectKey("isFrom")
      val isFrom = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("component")
      val component = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("end")
      val end = parser.parseString()
      parser.parseObjectNext()
      return ConnectionEnd(isFrom, component, end)
    }

    def parseConnectorTypeType(): ConnectorType.Type = {
      val r = parseConnectorTypeT(F)
      return r
    }

    def parseConnectorTypeT(typeParsed: B): ConnectorType.Type = {
      if (!typeParsed) {
        parser.parseObjectType("ConnectorType")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      ConnectorType.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for ConnectorType.")
          return ConnectorType.byOrdinal(0).get
      }
    }

    def parseConnector(): Connector = {
      val r = parseConnectorT(F)
      return r
    }

    def parseConnectorT(typeParsed: B): Connector = {
      if (!typeParsed) {
        parser.parseObjectType("Connector")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("from_type")
      val from_type = parseConnectorTypeType()
      parser.parseObjectNext()
      parser.parseObjectKey("from_template")
      val from_template = parser.parseOption(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("from_threads")
      val from_threads = parser.parseZ()
      parser.parseObjectNext()
      parser.parseObjectKey("from_hardware")
      val from_hardware = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("to_type")
      val to_type = parseConnectorTypeType()
      parser.parseObjectNext()
      parser.parseObjectKey("to_template")
      val to_template = parser.parseOption(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("to_threads")
      val to_threads = parser.parseZ()
      parser.parseObjectNext()
      parser.parseObjectKey("to_hardware")
      val to_hardware = parser.parseB()
      parser.parseObjectNext()
      return Connector(name, from_type, from_template, from_threads, from_hardware, to_type, to_template, to_threads, to_hardware)
    }

    def parseProcedure(): Procedure = {
      val r = parseProcedureT(F)
      return r
    }

    def parseProcedureT(typeParsed: B): Procedure = {
      if (!typeParsed) {
        parser.parseObjectType("Procedure")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("methods")
      val methods = parser.parseISZ(parseMethod _)
      parser.parseObjectNext()
      parser.parseObjectKey("includes")
      val includes = parser.parseISZ(parser.parseString _)
      parser.parseObjectNext()
      return Procedure(name, methods, includes)
    }

    def parseMethod(): Method = {
      val r = parseMethodT(F)
      return r
    }

    def parseMethodT(typeParsed: B): Method = {
      if (!typeParsed) {
        parser.parseObjectType("Method")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("parameters")
      val parameters = parser.parseISZ(parseParameter _)
      parser.parseObjectNext()
      parser.parseObjectKey("returnType")
      val returnType = parser.parseOption(parser.parseString _)
      parser.parseObjectNext()
      return Method(name, parameters, returnType)
    }

    def parseParameter(): Parameter = {
      val r = parseParameterT(F)
      return r
    }

    def parseParameterT(typeParsed: B): Parameter = {
      if (!typeParsed) {
        parser.parseObjectType("Parameter")
      }
      parser.parseObjectKey("array")
      val array = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("direction")
      val direction = parseDirectionType()
      parser.parseObjectNext()
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("typ")
      val typ = parser.parseString()
      parser.parseObjectNext()
      return Parameter(array, direction, name, typ)
    }

    def parseDirectionType(): Direction.Type = {
      val r = parseDirectionT(F)
      return r
    }

    def parseDirectionT(typeParsed: B): Direction.Type = {
      if (!typeParsed) {
        parser.parseObjectType("Direction")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      Direction.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for Direction.")
          return Direction.byOrdinal(0).get
      }
    }

    def parseBinarySemaphore(): BinarySemaphore = {
      val r = parseBinarySemaphoreT(F)
      return r
    }

    def parseBinarySemaphoreT(typeParsed: B): BinarySemaphore = {
      if (!typeParsed) {
        parser.parseObjectType("BinarySemaphore")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      return BinarySemaphore(name)
    }

    def parseTODO(): TODO = {
      val r = parseTODOT(F)
      return r
    }

    def parseTODOT(typeParsed: B): TODO = {
      if (!typeParsed) {
        parser.parseObjectType("TODO")
      }
      return TODO()
    }

    def eof(): B = {
      val r = parser.eof()
      return r
    }

  }

  def to[T](s: String, f: Parser => T): Either[T, Json.ErrorMsg] = {
    val parser = Parser(s)
    val r = f(parser)
    parser.eof()
    parser.errorOpt match {
      case Some(e) => return Either.Right(e)
      case _ => return Either.Left(r)
    }
  }

  def fromASTObject(o: ASTObject, isCompact: B): String = {
    val st = Printer.printASTObject(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toASTObject(s: String): Either[ASTObject, Json.ErrorMsg] = {
    def fASTObject(parser: Parser): ASTObject = {
      val r = parser.parseASTObject()
      return r
    }
    val r = to(s, fASTObject _)
    return r
  }

  def fromAssembly(o: Assembly, isCompact: B): String = {
    val st = Printer.printAssembly(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toAssembly(s: String): Either[Assembly, Json.ErrorMsg] = {
    def fAssembly(parser: Parser): Assembly = {
      val r = parser.parseAssembly()
      return r
    }
    val r = to(s, fAssembly _)
    return r
  }

  def fromComposition(o: Composition, isCompact: B): String = {
    val st = Printer.printComposition(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toComposition(s: String): Either[Composition, Json.ErrorMsg] = {
    def fComposition(parser: Parser): Composition = {
      val r = parser.parseComposition()
      return r
    }
    val r = to(s, fComposition _)
    return r
  }

  def fromInstance(o: Instance, isCompact: B): String = {
    val st = Printer.printInstance(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toInstance(s: String): Either[Instance, Json.ErrorMsg] = {
    def fInstance(parser: Parser): Instance = {
      val r = parser.parseInstance()
      return r
    }
    val r = to(s, fInstance _)
    return r
  }

  def fromComponent(o: Component, isCompact: B): String = {
    val st = Printer.printComponent(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toComponent(s: String): Either[Component, Json.ErrorMsg] = {
    def fComponent(parser: Parser): Component = {
      val r = parser.parseComponent()
      return r
    }
    val r = to(s, fComponent _)
    return r
  }

  def fromUses(o: Uses, isCompact: B): String = {
    val st = Printer.printUses(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toUses(s: String): Either[Uses, Json.ErrorMsg] = {
    def fUses(parser: Parser): Uses = {
      val r = parser.parseUses()
      return r
    }
    val r = to(s, fUses _)
    return r
  }

  def fromProvides(o: Provides, isCompact: B): String = {
    val st = Printer.printProvides(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toProvides(s: String): Either[Provides, Json.ErrorMsg] = {
    def fProvides(parser: Parser): Provides = {
      val r = parser.parseProvides()
      return r
    }
    val r = to(s, fProvides _)
    return r
  }

  def fromEmits(o: Emits, isCompact: B): String = {
    val st = Printer.printEmits(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toEmits(s: String): Either[Emits, Json.ErrorMsg] = {
    def fEmits(parser: Parser): Emits = {
      val r = parser.parseEmits()
      return r
    }
    val r = to(s, fEmits _)
    return r
  }

  def fromConsumes(o: Consumes, isCompact: B): String = {
    val st = Printer.printConsumes(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toConsumes(s: String): Either[Consumes, Json.ErrorMsg] = {
    def fConsumes(parser: Parser): Consumes = {
      val r = parser.parseConsumes()
      return r
    }
    val r = to(s, fConsumes _)
    return r
  }

  def fromDataport(o: Dataport, isCompact: B): String = {
    val st = Printer.printDataport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toDataport(s: String): Either[Dataport, Json.ErrorMsg] = {
    def fDataport(parser: Parser): Dataport = {
      val r = parser.parseDataport()
      return r
    }
    val r = to(s, fDataport _)
    return r
  }

  def fromConnection(o: Connection, isCompact: B): String = {
    val st = Printer.printConnection(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toConnection(s: String): Either[Connection, Json.ErrorMsg] = {
    def fConnection(parser: Parser): Connection = {
      val r = parser.parseConnection()
      return r
    }
    val r = to(s, fConnection _)
    return r
  }

  def fromConnectionEnd(o: ConnectionEnd, isCompact: B): String = {
    val st = Printer.printConnectionEnd(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toConnectionEnd(s: String): Either[ConnectionEnd, Json.ErrorMsg] = {
    def fConnectionEnd(parser: Parser): ConnectionEnd = {
      val r = parser.parseConnectionEnd()
      return r
    }
    val r = to(s, fConnectionEnd _)
    return r
  }

  def fromConnector(o: Connector, isCompact: B): String = {
    val st = Printer.printConnector(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toConnector(s: String): Either[Connector, Json.ErrorMsg] = {
    def fConnector(parser: Parser): Connector = {
      val r = parser.parseConnector()
      return r
    }
    val r = to(s, fConnector _)
    return r
  }

  def fromProcedure(o: Procedure, isCompact: B): String = {
    val st = Printer.printProcedure(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toProcedure(s: String): Either[Procedure, Json.ErrorMsg] = {
    def fProcedure(parser: Parser): Procedure = {
      val r = parser.parseProcedure()
      return r
    }
    val r = to(s, fProcedure _)
    return r
  }

  def fromMethod(o: Method, isCompact: B): String = {
    val st = Printer.printMethod(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toMethod(s: String): Either[Method, Json.ErrorMsg] = {
    def fMethod(parser: Parser): Method = {
      val r = parser.parseMethod()
      return r
    }
    val r = to(s, fMethod _)
    return r
  }

  def fromParameter(o: Parameter, isCompact: B): String = {
    val st = Printer.printParameter(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toParameter(s: String): Either[Parameter, Json.ErrorMsg] = {
    def fParameter(parser: Parser): Parameter = {
      val r = parser.parseParameter()
      return r
    }
    val r = to(s, fParameter _)
    return r
  }

  def fromBinarySemaphore(o: BinarySemaphore, isCompact: B): String = {
    val st = Printer.printBinarySemaphore(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toBinarySemaphore(s: String): Either[BinarySemaphore, Json.ErrorMsg] = {
    def fBinarySemaphore(parser: Parser): BinarySemaphore = {
      val r = parser.parseBinarySemaphore()
      return r
    }
    val r = to(s, fBinarySemaphore _)
    return r
  }

  def fromTODO(o: TODO, isCompact: B): String = {
    val st = Printer.printTODO(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toTODO(s: String): Either[TODO, Json.ErrorMsg] = {
    def fTODO(parser: Parser): TODO = {
      val r = parser.parseTODO()
      return r
    }
    val r = to(s, fTODO _)
    return r
  }

}