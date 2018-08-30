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

package org.sireum.aadl.act

import org.sireum._

object MsgPack {

  object Constants {

    val Assembly: Z = -32

    val Composition: Z = -31

    val Instance: Z = -30

    val Component: Z = -29

    val Uses: Z = -28

    val Provides: Z = -27

    val Connection: Z = -26

    val ConnectionEnd: Z = -25

    val Connector: Z = -24

    val Procedure: Z = -23

    val Method: Z = -22

    val Parameter: Z = -21

    val TODO: Z = -20

  }

  object Writer {

    @record class Default(val writer: MessagePack.Writer.Impl) extends Writer

  }

  @msig trait Writer {

    def writer: MessagePack.Writer

    def writeASTObject(o: ASTObject): Unit = {
      o match {
        case o: Assembly => writeAssembly(o)
        case o: Composition => writeComposition(o)
        case o: Instance => writeInstance(o)
        case o: Component => writeComponent(o)
        case o: Connection => writeConnection(o)
        case o: ConnectionEnd => writeConnectionEnd(o)
        case o: Connector => writeConnector(o)
        case o: Procedure => writeProcedure(o)
        case o: Method => writeMethod(o)
        case o: Parameter => writeParameter(o)
        case o: TODO => writeTODO(o)
      }
    }

    def writeAssembly(o: Assembly): Unit = {
      writer.writeZ(Constants.Assembly)
      writer.writeString(o.configuration)
      writeComposition(o.composition)
    }

    def writeComposition(o: Composition): Unit = {
      writer.writeZ(Constants.Composition)
      writer.writeISZ(o.groups, writeTODO _)
      writer.writeISZ(o.exports, writeTODO _)
      writer.writeISZ(o.instances, writeInstance _)
      writer.writeISZ(o.connections, writeConnection _)
    }

    def writeInstance(o: Instance): Unit = {
      writer.writeZ(Constants.Instance)
      writer.writeString(o.address_space)
      writer.writeString(o.name)
      writeComponent(o.component)
    }

    def writeComponent(o: Component): Unit = {
      writer.writeZ(Constants.Component)
      writer.writeB(o.control)
      writer.writeB(o.hardware)
      writer.writeString(o.name)
      writer.writeISZ(o.mutexes, writeTODO _)
      writer.writeISZ(o.binarySimaphores, writeTODO _)
      writer.writeISZ(o.semaphores, writeTODO _)
      writer.writeISZ(o.dataports, writeTODO _)
      writer.writeISZ(o.emits, writeTODO _)
      writer.writeISZ(o.uses, writeUses _)
      writer.writeISZ(o.consumes, writeTODO _)
      writer.writeISZ(o.provides, writeProvides _)
      writer.writeISZ(o.includes, writeTODO _)
      writer.writeISZ(o.attributes, writeTODO _)
    }

    def writeUses(o: Uses): Unit = {
      writer.writeZ(Constants.Uses)
      writer.writeString(o.name)
      writer.writeB(o.optional)
      writer.writeString(o.procedure)
    }

    def writeProvides(o: Provides): Unit = {
      writer.writeZ(Constants.Provides)
      writer.writeString(o.name)
      writer.writeString(o.procedure)
    }

    def writeConnection(o: Connection): Unit = {
      writer.writeZ(Constants.Connection)
      writer.writeString(o.name)
      writeConnector(o.connector)
      writer.writeISZ(o.from_ends, writeConnectionEnd _)
      writer.writeISZ(o.to_ends, writeConnectionEnd _)
    }

    def writeConnectionEnd(o: ConnectionEnd): Unit = {
      writer.writeZ(Constants.ConnectionEnd)
      writer.writeB(o.isFrom)
      writer.writeString(o.component)
      writer.writeString(o.end)
    }

    def writeConnector(o: Connector): Unit = {
      writer.writeZ(Constants.Connector)
      writer.writeB(o.from_hardware)
      writer.writeB(o.from_multiple)
      writer.writeZ(o.from_threads)
      writer.writeString(o.from_type)
      writer.writeString(o.name)
      writer.writeB(o.to_hardware)
      writer.writeB(o.to_multiple)
      writer.writeZ(o.to_threads)
      writer.writeString(o.to_type)
    }

    def writeProcedure(o: Procedure): Unit = {
      writer.writeZ(Constants.Procedure)
      writer.writeString(o.name)
      writer.writeISZ(o.methods, writeMethod _)
    }

    def writeMethod(o: Method): Unit = {
      writer.writeZ(Constants.Method)
      writer.writeString(o.name)
      writer.writeISZ(o.parameters, writeParameter _)
    }

    def writeParameter(o: Parameter): Unit = {
      writer.writeZ(Constants.Parameter)
      writer.writeB(o.array)
      writeDirectionType(o.direction)
      writer.writeString(o.name)
      writer.writeString(o.typ)
    }

    def writeDirectionType(o: Direction.Type): Unit = {
      writer.writeZ(o.ordinal)
    }

    def writeTODO(o: TODO): Unit = {
      writer.writeZ(Constants.TODO)
    }

    def result: ISZ[U8] = {
      return writer.result
    }

  }

  object Reader {

    @record class Default(val reader: MessagePack.Reader.Impl) extends Reader {
      def errorOpt: Option[MessagePack.ErrorMsg] = {
        return reader.errorOpt
      }
    }

  }

  @msig trait Reader {

    def reader: MessagePack.Reader

    def readASTObject(): ASTObject = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.Assembly => val r = readAssemblyT(T); return r
        case Constants.Composition => val r = readCompositionT(T); return r
        case Constants.Instance => val r = readInstanceT(T); return r
        case Constants.Component => val r = readComponentT(T); return r
        case Constants.Connection => val r = readConnectionT(T); return r
        case Constants.ConnectionEnd => val r = readConnectionEndT(T); return r
        case Constants.Connector => val r = readConnectorT(T); return r
        case Constants.Procedure => val r = readProcedureT(T); return r
        case Constants.Method => val r = readMethodT(T); return r
        case Constants.Parameter => val r = readParameterT(T); return r
        case Constants.TODO => val r = readTODOT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of ASTObject.")
          val r = readTODOT(T)
          return r
      }
    }

    def readAssembly(): Assembly = {
      val r = readAssemblyT(F)
      return r
    }

    def readAssemblyT(typeParsed: B): Assembly = {
      if (!typeParsed) {
        reader.expectZ(Constants.Assembly)
      }
      val configuration = reader.readString()
      val composition = readComposition()
      return Assembly(configuration, composition)
    }

    def readComposition(): Composition = {
      val r = readCompositionT(F)
      return r
    }

    def readCompositionT(typeParsed: B): Composition = {
      if (!typeParsed) {
        reader.expectZ(Constants.Composition)
      }
      val groups = reader.readISZ(readTODO _)
      val exports = reader.readISZ(readTODO _)
      val instances = reader.readISZ(readInstance _)
      val connections = reader.readISZ(readConnection _)
      return Composition(groups, exports, instances, connections)
    }

    def readInstance(): Instance = {
      val r = readInstanceT(F)
      return r
    }

    def readInstanceT(typeParsed: B): Instance = {
      if (!typeParsed) {
        reader.expectZ(Constants.Instance)
      }
      val address_space = reader.readString()
      val name = reader.readString()
      val component = readComponent()
      return Instance(address_space, name, component)
    }

    def readComponent(): Component = {
      val r = readComponentT(F)
      return r
    }

    def readComponentT(typeParsed: B): Component = {
      if (!typeParsed) {
        reader.expectZ(Constants.Component)
      }
      val control = reader.readB()
      val hardware = reader.readB()
      val name = reader.readString()
      val mutexes = reader.readISZ(readTODO _)
      val binarySimaphores = reader.readISZ(readTODO _)
      val semaphores = reader.readISZ(readTODO _)
      val dataports = reader.readISZ(readTODO _)
      val emits = reader.readISZ(readTODO _)
      val uses = reader.readISZ(readUses _)
      val consumes = reader.readISZ(readTODO _)
      val provides = reader.readISZ(readProvides _)
      val includes = reader.readISZ(readTODO _)
      val attributes = reader.readISZ(readTODO _)
      return Component(control, hardware, name, mutexes, binarySimaphores, semaphores, dataports, emits, uses, consumes, provides, includes, attributes)
    }

    def readUses(): Uses = {
      val r = readUsesT(F)
      return r
    }

    def readUsesT(typeParsed: B): Uses = {
      if (!typeParsed) {
        reader.expectZ(Constants.Uses)
      }
      val name = reader.readString()
      val optional = reader.readB()
      val procedure = reader.readString()
      return Uses(name, optional, procedure)
    }

    def readProvides(): Provides = {
      val r = readProvidesT(F)
      return r
    }

    def readProvidesT(typeParsed: B): Provides = {
      if (!typeParsed) {
        reader.expectZ(Constants.Provides)
      }
      val name = reader.readString()
      val procedure = reader.readString()
      return Provides(name, procedure)
    }

    def readConnection(): Connection = {
      val r = readConnectionT(F)
      return r
    }

    def readConnectionT(typeParsed: B): Connection = {
      if (!typeParsed) {
        reader.expectZ(Constants.Connection)
      }
      val name = reader.readString()
      val connector = readConnector()
      val from_ends = reader.readISZ(readConnectionEnd _)
      val to_ends = reader.readISZ(readConnectionEnd _)
      return Connection(name, connector, from_ends, to_ends)
    }

    def readConnectionEnd(): ConnectionEnd = {
      val r = readConnectionEndT(F)
      return r
    }

    def readConnectionEndT(typeParsed: B): ConnectionEnd = {
      if (!typeParsed) {
        reader.expectZ(Constants.ConnectionEnd)
      }
      val isFrom = reader.readB()
      val component = reader.readString()
      val end = reader.readString()
      return ConnectionEnd(isFrom, component, end)
    }

    def readConnector(): Connector = {
      val r = readConnectorT(F)
      return r
    }

    def readConnectorT(typeParsed: B): Connector = {
      if (!typeParsed) {
        reader.expectZ(Constants.Connector)
      }
      val from_hardware = reader.readB()
      val from_multiple = reader.readB()
      val from_threads = reader.readZ()
      val from_type = reader.readString()
      val name = reader.readString()
      val to_hardware = reader.readB()
      val to_multiple = reader.readB()
      val to_threads = reader.readZ()
      val to_type = reader.readString()
      return Connector(from_hardware, from_multiple, from_threads, from_type, name, to_hardware, to_multiple, to_threads, to_type)
    }

    def readProcedure(): Procedure = {
      val r = readProcedureT(F)
      return r
    }

    def readProcedureT(typeParsed: B): Procedure = {
      if (!typeParsed) {
        reader.expectZ(Constants.Procedure)
      }
      val name = reader.readString()
      val methods = reader.readISZ(readMethod _)
      return Procedure(name, methods)
    }

    def readMethod(): Method = {
      val r = readMethodT(F)
      return r
    }

    def readMethodT(typeParsed: B): Method = {
      if (!typeParsed) {
        reader.expectZ(Constants.Method)
      }
      val name = reader.readString()
      val parameters = reader.readISZ(readParameter _)
      return Method(name, parameters)
    }

    def readParameter(): Parameter = {
      val r = readParameterT(F)
      return r
    }

    def readParameterT(typeParsed: B): Parameter = {
      if (!typeParsed) {
        reader.expectZ(Constants.Parameter)
      }
      val array = reader.readB()
      val direction = readDirectionType()
      val name = reader.readString()
      val typ = reader.readString()
      return Parameter(array, direction, name, typ)
    }

    def readDirectionType(): Direction.Type = {
      val r = reader.readZ()
      return Direction.byOrdinal(r).get
    }

    def readTODO(): TODO = {
      val r = readTODOT(F)
      return r
    }

    def readTODOT(typeParsed: B): TODO = {
      if (!typeParsed) {
        reader.expectZ(Constants.TODO)
      }
      return TODO()
    }

  }

  def to[T](data: ISZ[U8], f: Reader => T): Either[T, MessagePack.ErrorMsg] = {
    val rd = Reader.Default(MessagePack.reader(data))
    rd.reader.init()
    val r = f(rd)
    rd.errorOpt match {
      case Some(e) => return Either.Right(e)
      case _ => return Either.Left(r)
    }
  }

  def fromASTObject(o: ASTObject, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeASTObject(o)
    return w.result
  }

  def toASTObject(data: ISZ[U8]): Either[ASTObject, MessagePack.ErrorMsg] = {
    def fASTObject(reader: Reader): ASTObject = {
      val r = reader.readASTObject()
      return r
    }
    val r = to(data, fASTObject _)
    return r
  }

  def fromAssembly(o: Assembly, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeAssembly(o)
    return w.result
  }

  def toAssembly(data: ISZ[U8]): Either[Assembly, MessagePack.ErrorMsg] = {
    def fAssembly(reader: Reader): Assembly = {
      val r = reader.readAssembly()
      return r
    }
    val r = to(data, fAssembly _)
    return r
  }

  def fromComposition(o: Composition, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeComposition(o)
    return w.result
  }

  def toComposition(data: ISZ[U8]): Either[Composition, MessagePack.ErrorMsg] = {
    def fComposition(reader: Reader): Composition = {
      val r = reader.readComposition()
      return r
    }
    val r = to(data, fComposition _)
    return r
  }

  def fromInstance(o: Instance, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeInstance(o)
    return w.result
  }

  def toInstance(data: ISZ[U8]): Either[Instance, MessagePack.ErrorMsg] = {
    def fInstance(reader: Reader): Instance = {
      val r = reader.readInstance()
      return r
    }
    val r = to(data, fInstance _)
    return r
  }

  def fromComponent(o: Component, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeComponent(o)
    return w.result
  }

  def toComponent(data: ISZ[U8]): Either[Component, MessagePack.ErrorMsg] = {
    def fComponent(reader: Reader): Component = {
      val r = reader.readComponent()
      return r
    }
    val r = to(data, fComponent _)
    return r
  }

  def fromUses(o: Uses, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeUses(o)
    return w.result
  }

  def toUses(data: ISZ[U8]): Either[Uses, MessagePack.ErrorMsg] = {
    def fUses(reader: Reader): Uses = {
      val r = reader.readUses()
      return r
    }
    val r = to(data, fUses _)
    return r
  }

  def fromProvides(o: Provides, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeProvides(o)
    return w.result
  }

  def toProvides(data: ISZ[U8]): Either[Provides, MessagePack.ErrorMsg] = {
    def fProvides(reader: Reader): Provides = {
      val r = reader.readProvides()
      return r
    }
    val r = to(data, fProvides _)
    return r
  }

  def fromConnection(o: Connection, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeConnection(o)
    return w.result
  }

  def toConnection(data: ISZ[U8]): Either[Connection, MessagePack.ErrorMsg] = {
    def fConnection(reader: Reader): Connection = {
      val r = reader.readConnection()
      return r
    }
    val r = to(data, fConnection _)
    return r
  }

  def fromConnectionEnd(o: ConnectionEnd, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeConnectionEnd(o)
    return w.result
  }

  def toConnectionEnd(data: ISZ[U8]): Either[ConnectionEnd, MessagePack.ErrorMsg] = {
    def fConnectionEnd(reader: Reader): ConnectionEnd = {
      val r = reader.readConnectionEnd()
      return r
    }
    val r = to(data, fConnectionEnd _)
    return r
  }

  def fromConnector(o: Connector, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeConnector(o)
    return w.result
  }

  def toConnector(data: ISZ[U8]): Either[Connector, MessagePack.ErrorMsg] = {
    def fConnector(reader: Reader): Connector = {
      val r = reader.readConnector()
      return r
    }
    val r = to(data, fConnector _)
    return r
  }

  def fromProcedure(o: Procedure, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeProcedure(o)
    return w.result
  }

  def toProcedure(data: ISZ[U8]): Either[Procedure, MessagePack.ErrorMsg] = {
    def fProcedure(reader: Reader): Procedure = {
      val r = reader.readProcedure()
      return r
    }
    val r = to(data, fProcedure _)
    return r
  }

  def fromMethod(o: Method, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeMethod(o)
    return w.result
  }

  def toMethod(data: ISZ[U8]): Either[Method, MessagePack.ErrorMsg] = {
    def fMethod(reader: Reader): Method = {
      val r = reader.readMethod()
      return r
    }
    val r = to(data, fMethod _)
    return r
  }

  def fromParameter(o: Parameter, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeParameter(o)
    return w.result
  }

  def toParameter(data: ISZ[U8]): Either[Parameter, MessagePack.ErrorMsg] = {
    def fParameter(reader: Reader): Parameter = {
      val r = reader.readParameter()
      return r
    }
    val r = to(data, fParameter _)
    return r
  }

  def fromTODO(o: TODO, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeTODO(o)
    return w.result
  }

  def toTODO(data: ISZ[U8]): Either[TODO, MessagePack.ErrorMsg] = {
    def fTODO(reader: Reader): TODO = {
      val r = reader.readTODO()
      return r
    }
    val r = to(data, fTODO _)
    return r
  }

}