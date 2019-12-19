// #Sireum
// @formatter:off

/*
 Copyright (c) 2019, Robby, Kansas State University
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

package org.sireum.hamr.act.ast

import org.sireum._

object MsgPack {

  object Constants {

    val Assembly: Z = -32

    val Composition: Z = -31

    val Instance: Z = -30

    val Component: Z = -29

    val Uses: Z = -28

    val Provides: Z = -27

    val Emits: Z = -26

    val Consumes: Z = -25

    val Dataport: Z = -24

    val Connection: Z = -23

    val ConnectionEnd: Z = -22

    val Connector: Z = -21

    val Procedure: Z = -20

    val Method: Z = -19

    val Parameter: Z = -18

    val BinarySemaphore: Z = -17

    val Semaphore: Z = -16

    val Mutex: Z = -15

    val TODO: Z = -14

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
        case o: BinarySemaphore => writeBinarySemaphore(o)
        case o: Semaphore => writeSemaphore(o)
        case o: Mutex => writeMutex(o)
        case o: TODO => writeTODO(o)
      }
    }

    def writeAssembly(o: Assembly): Unit = {
      writer.writeZ(Constants.Assembly)
      writer.writeISZ(o.configuration, writer.writeString _)
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
      writer.writeISZ(o.mutexes, writeMutex _)
      writer.writeISZ(o.binarySemaphores, writeBinarySemaphore _)
      writer.writeISZ(o.semaphores, writeSemaphore _)
      writer.writeISZ(o.dataports, writeDataport _)
      writer.writeISZ(o.emits, writeEmits _)
      writer.writeISZ(o.uses, writeUses _)
      writer.writeISZ(o.consumes, writeConsumes _)
      writer.writeISZ(o.provides, writeProvides _)
      writer.writeISZ(o.includes, writer.writeString _)
      writer.writeISZ(o.attributes, writeTODO _)
      writer.writeISZ(o.imports, writer.writeString _)
    }

    def writeUses(o: Uses): Unit = {
      writer.writeZ(Constants.Uses)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeB(o.optional)
    }

    def writeProvides(o: Provides): Unit = {
      writer.writeZ(Constants.Provides)
      writer.writeString(o.name)
      writer.writeString(o.typ)
    }

    def writeEmits(o: Emits): Unit = {
      writer.writeZ(Constants.Emits)
      writer.writeString(o.name)
      writer.writeString(o.typ)
    }

    def writeConsumes(o: Consumes): Unit = {
      writer.writeZ(Constants.Consumes)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeB(o.optional)
    }

    def writeDataport(o: Dataport): Unit = {
      writer.writeZ(Constants.Dataport)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeB(o.optional)
    }

    def writeConnection(o: Connection): Unit = {
      writer.writeZ(Constants.Connection)
      writer.writeString(o.name)
      writer.writeString(o.connectionType)
      writer.writeISZ(o.from_ends, writeConnectionEnd _)
      writer.writeISZ(o.to_ends, writeConnectionEnd _)
    }

    def writeConnectionEnd(o: ConnectionEnd): Unit = {
      writer.writeZ(Constants.ConnectionEnd)
      writer.writeB(o.isFrom)
      writer.writeString(o.component)
      writer.writeString(o.end)
    }

    def writeConnectorTypeType(o: ConnectorType.Type): Unit = {
      writer.writeZ(o.ordinal)
    }

    def writeConnector(o: Connector): Unit = {
      writer.writeZ(Constants.Connector)
      writer.writeString(o.name)
      writeConnectorTypeType(o.from_type)
      writer.writeOption(o.from_template, writer.writeString _)
      writer.writeZ(o.from_threads)
      writer.writeB(o.from_hardware)
      writeConnectorTypeType(o.to_type)
      writer.writeOption(o.to_template, writer.writeString _)
      writer.writeZ(o.to_threads)
      writer.writeB(o.to_hardware)
    }

    def writeProcedure(o: Procedure): Unit = {
      writer.writeZ(Constants.Procedure)
      writer.writeString(o.name)
      writer.writeISZ(o.methods, writeMethod _)
      writer.writeISZ(o.includes, writer.writeString _)
    }

    def writeMethod(o: Method): Unit = {
      writer.writeZ(Constants.Method)
      writer.writeString(o.name)
      writer.writeISZ(o.parameters, writeParameter _)
      writer.writeOption(o.returnType, writer.writeString _)
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

    def writeBinarySemaphore(o: BinarySemaphore): Unit = {
      writer.writeZ(Constants.BinarySemaphore)
      writer.writeString(o.name)
    }

    def writeSemaphore(o: Semaphore): Unit = {
      writer.writeZ(Constants.Semaphore)
      writer.writeString(o.name)
    }

    def writeMutex(o: Mutex): Unit = {
      writer.writeZ(Constants.Mutex)
      writer.writeString(o.name)
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
        case Constants.BinarySemaphore => val r = readBinarySemaphoreT(T); return r
        case Constants.Semaphore => val r = readSemaphoreT(T); return r
        case Constants.Mutex => val r = readMutexT(T); return r
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
      val configuration = reader.readISZ(reader.readString _)
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
      val mutexes = reader.readISZ(readMutex _)
      val binarySemaphores = reader.readISZ(readBinarySemaphore _)
      val semaphores = reader.readISZ(readSemaphore _)
      val dataports = reader.readISZ(readDataport _)
      val emits = reader.readISZ(readEmits _)
      val uses = reader.readISZ(readUses _)
      val consumes = reader.readISZ(readConsumes _)
      val provides = reader.readISZ(readProvides _)
      val includes = reader.readISZ(reader.readString _)
      val attributes = reader.readISZ(readTODO _)
      val imports = reader.readISZ(reader.readString _)
      return Component(control, hardware, name, mutexes, binarySemaphores, semaphores, dataports, emits, uses, consumes, provides, includes, attributes, imports)
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
      val typ = reader.readString()
      val optional = reader.readB()
      return Uses(name, typ, optional)
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
      val typ = reader.readString()
      return Provides(name, typ)
    }

    def readEmits(): Emits = {
      val r = readEmitsT(F)
      return r
    }

    def readEmitsT(typeParsed: B): Emits = {
      if (!typeParsed) {
        reader.expectZ(Constants.Emits)
      }
      val name = reader.readString()
      val typ = reader.readString()
      return Emits(name, typ)
    }

    def readConsumes(): Consumes = {
      val r = readConsumesT(F)
      return r
    }

    def readConsumesT(typeParsed: B): Consumes = {
      if (!typeParsed) {
        reader.expectZ(Constants.Consumes)
      }
      val name = reader.readString()
      val typ = reader.readString()
      val optional = reader.readB()
      return Consumes(name, typ, optional)
    }

    def readDataport(): Dataport = {
      val r = readDataportT(F)
      return r
    }

    def readDataportT(typeParsed: B): Dataport = {
      if (!typeParsed) {
        reader.expectZ(Constants.Dataport)
      }
      val name = reader.readString()
      val typ = reader.readString()
      val optional = reader.readB()
      return Dataport(name, typ, optional)
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
      val connectionType = reader.readString()
      val from_ends = reader.readISZ(readConnectionEnd _)
      val to_ends = reader.readISZ(readConnectionEnd _)
      return Connection(name, connectionType, from_ends, to_ends)
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

    def readConnectorTypeType(): ConnectorType.Type = {
      val r = reader.readZ()
      return ConnectorType.byOrdinal(r).get
    }

    def readConnector(): Connector = {
      val r = readConnectorT(F)
      return r
    }

    def readConnectorT(typeParsed: B): Connector = {
      if (!typeParsed) {
        reader.expectZ(Constants.Connector)
      }
      val name = reader.readString()
      val from_type = readConnectorTypeType()
      val from_template = reader.readOption(reader.readString _)
      val from_threads = reader.readZ()
      val from_hardware = reader.readB()
      val to_type = readConnectorTypeType()
      val to_template = reader.readOption(reader.readString _)
      val to_threads = reader.readZ()
      val to_hardware = reader.readB()
      return Connector(name, from_type, from_template, from_threads, from_hardware, to_type, to_template, to_threads, to_hardware)
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
      val includes = reader.readISZ(reader.readString _)
      return Procedure(name, methods, includes)
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
      val returnType = reader.readOption(reader.readString _)
      return Method(name, parameters, returnType)
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

    def readBinarySemaphore(): BinarySemaphore = {
      val r = readBinarySemaphoreT(F)
      return r
    }

    def readBinarySemaphoreT(typeParsed: B): BinarySemaphore = {
      if (!typeParsed) {
        reader.expectZ(Constants.BinarySemaphore)
      }
      val name = reader.readString()
      return BinarySemaphore(name)
    }

    def readSemaphore(): Semaphore = {
      val r = readSemaphoreT(F)
      return r
    }

    def readSemaphoreT(typeParsed: B): Semaphore = {
      if (!typeParsed) {
        reader.expectZ(Constants.Semaphore)
      }
      val name = reader.readString()
      return Semaphore(name)
    }

    def readMutex(): Mutex = {
      val r = readMutexT(F)
      return r
    }

    def readMutexT(typeParsed: B): Mutex = {
      if (!typeParsed) {
        reader.expectZ(Constants.Mutex)
      }
      val name = reader.readString()
      return Mutex(name)
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

  def fromEmits(o: Emits, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeEmits(o)
    return w.result
  }

  def toEmits(data: ISZ[U8]): Either[Emits, MessagePack.ErrorMsg] = {
    def fEmits(reader: Reader): Emits = {
      val r = reader.readEmits()
      return r
    }
    val r = to(data, fEmits _)
    return r
  }

  def fromConsumes(o: Consumes, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeConsumes(o)
    return w.result
  }

  def toConsumes(data: ISZ[U8]): Either[Consumes, MessagePack.ErrorMsg] = {
    def fConsumes(reader: Reader): Consumes = {
      val r = reader.readConsumes()
      return r
    }
    val r = to(data, fConsumes _)
    return r
  }

  def fromDataport(o: Dataport, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeDataport(o)
    return w.result
  }

  def toDataport(data: ISZ[U8]): Either[Dataport, MessagePack.ErrorMsg] = {
    def fDataport(reader: Reader): Dataport = {
      val r = reader.readDataport()
      return r
    }
    val r = to(data, fDataport _)
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

  def fromBinarySemaphore(o: BinarySemaphore, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeBinarySemaphore(o)
    return w.result
  }

  def toBinarySemaphore(data: ISZ[U8]): Either[BinarySemaphore, MessagePack.ErrorMsg] = {
    def fBinarySemaphore(reader: Reader): BinarySemaphore = {
      val r = reader.readBinarySemaphore()
      return r
    }
    val r = to(data, fBinarySemaphore _)
    return r
  }

  def fromSemaphore(o: Semaphore, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeSemaphore(o)
    return w.result
  }

  def toSemaphore(data: ISZ[U8]): Either[Semaphore, MessagePack.ErrorMsg] = {
    def fSemaphore(reader: Reader): Semaphore = {
      val r = reader.readSemaphore()
      return r
    }
    val r = to(data, fSemaphore _)
    return r
  }

  def fromMutex(o: Mutex, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeMutex(o)
    return w.result
  }

  def toMutex(data: ISZ[U8]): Either[Mutex, MessagePack.ErrorMsg] = {
    def fMutex(reader: Reader): Mutex = {
      val r = reader.readMutex()
      return r
    }
    val r = to(data, fMutex _)
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