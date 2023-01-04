// #Sireum
// @formatter:off

/*
 Copyright (c) 2017-2023, Robby, Kansas State University
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

    val AstBasicComment: Z = -32

    val Assembly: Z = -31

    val Composition: Z = -30

    val Instance: Z = -29

    val Component: Z = -28

    val LibraryComponent: Z = -27

    val Uses: Z = -26

    val Provides: Z = -25

    val Emits: Z = -24

    val Consumes: Z = -23

    val Dataport: Z = -22

    val Connection: Z = -21

    val ConnectionEnd: Z = -20

    val Connector: Z = -19

    val Procedure: Z = -18

    val Method: Z = -17

    val Parameter: Z = -16

    val BinarySemaphore: Z = -15

    val Semaphore: Z = -14

    val Mutex: Z = -13

    val Attribute: Z = -12

    val GenericConfiguration: Z = -11

    val DataPortAccessRestriction: Z = -10

    val TODO: Z = -9

  }

  object Writer {

    @record class Default(val writer: MessagePack.Writer.Impl) extends Writer

  }

  @msig trait Writer {

    def writer: MessagePack.Writer

    def writeAstComment(o: AstComment): Unit = {
      o match {
        case o: AstBasicComment => writeAstBasicComment(o)
      }
    }

    def writeCommentLocationType(o: CommentLocation.Type): Unit = {
      writer.writeZ(o.ordinal)
    }

    def writeAstBasicComment(o: AstBasicComment): Unit = {
      writer.writeZ(Constants.AstBasicComment)
      writeCommentLocationType(o.location)
      writer.writeString(o.comment)
    }

    def writeCommentProvider(o: CommentProvider): Unit = {
      o match {
        case o: Assembly => writeAssembly(o)
        case o: Composition => writeComposition(o)
        case o: Instance => writeInstance(o)
        case o: Component => writeComponent(o)
        case o: LibraryComponent => writeLibraryComponent(o)
        case o: Uses => writeUses(o)
        case o: Provides => writeProvides(o)
        case o: Emits => writeEmits(o)
        case o: Consumes => writeConsumes(o)
        case o: Dataport => writeDataport(o)
        case o: Connection => writeConnection(o)
        case o: ConnectionEnd => writeConnectionEnd(o)
        case o: Connector => writeConnector(o)
        case o: Procedure => writeProcedure(o)
        case o: Method => writeMethod(o)
        case o: Parameter => writeParameter(o)
        case o: BinarySemaphore => writeBinarySemaphore(o)
        case o: Semaphore => writeSemaphore(o)
        case o: Mutex => writeMutex(o)
        case o: GenericConfiguration => writeGenericConfiguration(o)
        case o: DataPortAccessRestriction => writeDataPortAccessRestriction(o)
        case o: TODO => writeTODO(o)
      }
    }

    def writeASTObject(o: ASTObject): Unit = {
      o match {
        case o: Assembly => writeAssembly(o)
        case o: Composition => writeComposition(o)
        case o: Instance => writeInstance(o)
        case o: Component => writeComponent(o)
        case o: LibraryComponent => writeLibraryComponent(o)
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
      writer.writeISZ(o.configuration, writeConfiguration _)
      writer.writeISZ(o.configurationMacros, writer.writeString _)
      writeComposition(o.composition)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeComposition(o: Composition): Unit = {
      writer.writeZ(Constants.Composition)
      writer.writeISZ(o.groups, writeTODO _)
      writer.writeISZ(o.exports, writeTODO _)
      writer.writeISZ(o.instances, writeInstance _)
      writer.writeISZ(o.connections, writeConnection _)
      writer.writeISZ(o.externalEntities, writer.writeString _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeInstance(o: Instance): Unit = {
      writer.writeZ(Constants.Instance)
      writer.writeString(o.address_space)
      writer.writeString(o.name)
      writeCamkesComponent(o.component)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeCamkesComponent(o: CamkesComponent): Unit = {
      o match {
        case o: Component => writeComponent(o)
        case o: LibraryComponent => writeLibraryComponent(o)
      }
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
      writer.writeISZ(o.preprocessorIncludes, writer.writeString _)
      writer.writeISZ(o.externalEntities, writer.writeString _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeLibraryComponent(o: LibraryComponent): Unit = {
      writer.writeZ(Constants.LibraryComponent)
      writer.writeString(o.name)
      writer.writeISZ(o.ports, writer.writeString _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeCAmkESFeature(o: CAmkESFeature): Unit = {
      o match {
        case o: Uses => writeUses(o)
        case o: Provides => writeProvides(o)
        case o: Emits => writeEmits(o)
        case o: Consumes => writeConsumes(o)
        case o: Dataport => writeDataport(o)
      }
    }

    def writeUses(o: Uses): Unit = {
      writer.writeZ(Constants.Uses)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeB(o.optional)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeProvides(o: Provides): Unit = {
      writer.writeZ(Constants.Provides)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeEmits(o: Emits): Unit = {
      writer.writeZ(Constants.Emits)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeConsumes(o: Consumes): Unit = {
      writer.writeZ(Constants.Consumes)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeB(o.optional)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeDataport(o: Dataport): Unit = {
      writer.writeZ(Constants.Dataport)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeB(o.optional)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeConnection(o: Connection): Unit = {
      writer.writeZ(Constants.Connection)
      writer.writeString(o.name)
      writer.writeString(o.connectionType)
      writer.writeISZ(o.from_ends, writeConnectionEnd _)
      writer.writeISZ(o.to_ends, writeConnectionEnd _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeConnectionEnd(o: ConnectionEnd): Unit = {
      writer.writeZ(Constants.ConnectionEnd)
      writer.writeB(o.isFrom)
      writer.writeString(o.component)
      writer.writeString(o.end)
      writer.writeISZ(o.comments, writeAstComment _)
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
      writer.writeISZ(o.attributes, writeAttribute _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeProcedure(o: Procedure): Unit = {
      writer.writeZ(Constants.Procedure)
      writer.writeString(o.name)
      writer.writeISZ(o.methods, writeMethod _)
      writer.writeISZ(o.includes, writer.writeString _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeMethod(o: Method): Unit = {
      writer.writeZ(Constants.Method)
      writer.writeString(o.name)
      writer.writeISZ(o.parameters, writeParameter _)
      writer.writeOption(o.returnType, writer.writeString _)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeParameter(o: Parameter): Unit = {
      writer.writeZ(Constants.Parameter)
      writer.writeB(o.array)
      writeDirectionType(o.direction)
      writer.writeString(o.name)
      writer.writeString(o.typ)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeDirectionType(o: Direction.Type): Unit = {
      writer.writeZ(o.ordinal)
    }

    def writeBinarySemaphore(o: BinarySemaphore): Unit = {
      writer.writeZ(Constants.BinarySemaphore)
      writer.writeString(o.name)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeSemaphore(o: Semaphore): Unit = {
      writer.writeZ(Constants.Semaphore)
      writer.writeString(o.name)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeMutex(o: Mutex): Unit = {
      writer.writeZ(Constants.Mutex)
      writer.writeString(o.name)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeAttribute(o: Attribute): Unit = {
      writer.writeZ(Constants.Attribute)
      writer.writeString(o.typ)
      writer.writeString(o.name)
      writer.writeString(o.value)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeAccessTypeType(o: AccessType.Type): Unit = {
      writer.writeZ(o.ordinal)
    }

    def writeConfiguration(o: Configuration): Unit = {
      o match {
        case o: GenericConfiguration => writeGenericConfiguration(o)
        case o: DataPortAccessRestriction => writeDataPortAccessRestriction(o)
      }
    }

    def writeGenericConfiguration(o: GenericConfiguration): Unit = {
      writer.writeZ(Constants.GenericConfiguration)
      writer.writeString(o.e)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeDataPortAccessRestriction(o: DataPortAccessRestriction): Unit = {
      writer.writeZ(Constants.DataPortAccessRestriction)
      writer.writeString(o.component)
      writer.writeString(o.port)
      writeAccessTypeType(o.accessType)
      writer.writeISZ(o.comments, writeAstComment _)
    }

    def writeTODO(o: TODO): Unit = {
      writer.writeZ(Constants.TODO)
      writer.writeISZ(o.comments, writeAstComment _)
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

    def readAstComment(): AstComment = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.AstBasicComment => val r = readAstBasicCommentT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of AstComment.")
          val r = readAstBasicCommentT(T)
          return r
      }
    }

    def readCommentLocationType(): CommentLocation.Type = {
      val r = reader.readZ()
      return CommentLocation.byOrdinal(r).get
    }

    def readAstBasicComment(): AstBasicComment = {
      val r = readAstBasicCommentT(F)
      return r
    }

    def readAstBasicCommentT(typeParsed: B): AstBasicComment = {
      if (!typeParsed) {
        reader.expectZ(Constants.AstBasicComment)
      }
      val location = readCommentLocationType()
      val comment = reader.readString()
      return AstBasicComment(location, comment)
    }

    def readCommentProvider(): CommentProvider = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.Assembly => val r = readAssemblyT(T); return r
        case Constants.Composition => val r = readCompositionT(T); return r
        case Constants.Instance => val r = readInstanceT(T); return r
        case Constants.Component => val r = readComponentT(T); return r
        case Constants.LibraryComponent => val r = readLibraryComponentT(T); return r
        case Constants.Uses => val r = readUsesT(T); return r
        case Constants.Provides => val r = readProvidesT(T); return r
        case Constants.Emits => val r = readEmitsT(T); return r
        case Constants.Consumes => val r = readConsumesT(T); return r
        case Constants.Dataport => val r = readDataportT(T); return r
        case Constants.Connection => val r = readConnectionT(T); return r
        case Constants.ConnectionEnd => val r = readConnectionEndT(T); return r
        case Constants.Connector => val r = readConnectorT(T); return r
        case Constants.Procedure => val r = readProcedureT(T); return r
        case Constants.Method => val r = readMethodT(T); return r
        case Constants.Parameter => val r = readParameterT(T); return r
        case Constants.BinarySemaphore => val r = readBinarySemaphoreT(T); return r
        case Constants.Semaphore => val r = readSemaphoreT(T); return r
        case Constants.Mutex => val r = readMutexT(T); return r
        case Constants.GenericConfiguration => val r = readGenericConfigurationT(T); return r
        case Constants.DataPortAccessRestriction => val r = readDataPortAccessRestrictionT(T); return r
        case Constants.TODO => val r = readTODOT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of CommentProvider.")
          val r = readTODOT(T)
          return r
      }
    }

    def readASTObject(): ASTObject = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.Assembly => val r = readAssemblyT(T); return r
        case Constants.Composition => val r = readCompositionT(T); return r
        case Constants.Instance => val r = readInstanceT(T); return r
        case Constants.Component => val r = readComponentT(T); return r
        case Constants.LibraryComponent => val r = readLibraryComponentT(T); return r
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
      val configuration = reader.readISZ(readConfiguration _)
      val configurationMacros = reader.readISZ(reader.readString _)
      val composition = readComposition()
      val comments = reader.readISZ(readAstComment _)
      return Assembly(configuration, configurationMacros, composition, comments)
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
      val externalEntities = reader.readISZ(reader.readString _)
      val comments = reader.readISZ(readAstComment _)
      return Composition(groups, exports, instances, connections, externalEntities, comments)
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
      val component = readCamkesComponent()
      val comments = reader.readISZ(readAstComment _)
      return Instance(address_space, name, component, comments)
    }

    def readCamkesComponent(): CamkesComponent = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.Component => val r = readComponentT(T); return r
        case Constants.LibraryComponent => val r = readLibraryComponentT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of CamkesComponent.")
          val r = readLibraryComponentT(T)
          return r
      }
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
      val preprocessorIncludes = reader.readISZ(reader.readString _)
      val externalEntities = reader.readISZ(reader.readString _)
      val comments = reader.readISZ(readAstComment _)
      return Component(control, hardware, name, mutexes, binarySemaphores, semaphores, dataports, emits, uses, consumes, provides, includes, attributes, imports, preprocessorIncludes, externalEntities, comments)
    }

    def readLibraryComponent(): LibraryComponent = {
      val r = readLibraryComponentT(F)
      return r
    }

    def readLibraryComponentT(typeParsed: B): LibraryComponent = {
      if (!typeParsed) {
        reader.expectZ(Constants.LibraryComponent)
      }
      val name = reader.readString()
      val ports = reader.readISZ(reader.readString _)
      val comments = reader.readISZ(readAstComment _)
      return LibraryComponent(name, ports, comments)
    }

    def readCAmkESFeature(): CAmkESFeature = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.Uses => val r = readUsesT(T); return r
        case Constants.Provides => val r = readProvidesT(T); return r
        case Constants.Emits => val r = readEmitsT(T); return r
        case Constants.Consumes => val r = readConsumesT(T); return r
        case Constants.Dataport => val r = readDataportT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of CAmkESFeature.")
          val r = readDataportT(T)
          return r
      }
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
      val comments = reader.readISZ(readAstComment _)
      return Uses(name, typ, optional, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Provides(name, typ, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Emits(name, typ, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Consumes(name, typ, optional, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Dataport(name, typ, optional, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Connection(name, connectionType, from_ends, to_ends, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return ConnectionEnd(isFrom, component, end, comments)
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
      val attributes = reader.readISZ(readAttribute _)
      val comments = reader.readISZ(readAstComment _)
      return Connector(name, from_type, from_template, from_threads, from_hardware, to_type, to_template, to_threads, to_hardware, attributes, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Procedure(name, methods, includes, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Method(name, parameters, returnType, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Parameter(array, direction, name, typ, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return BinarySemaphore(name, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Semaphore(name, comments)
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
      val comments = reader.readISZ(readAstComment _)
      return Mutex(name, comments)
    }

    def readAttribute(): Attribute = {
      val r = readAttributeT(F)
      return r
    }

    def readAttributeT(typeParsed: B): Attribute = {
      if (!typeParsed) {
        reader.expectZ(Constants.Attribute)
      }
      val typ = reader.readString()
      val name = reader.readString()
      val value = reader.readString()
      val comments = reader.readISZ(readAstComment _)
      return Attribute(typ, name, value, comments)
    }

    def readAccessTypeType(): AccessType.Type = {
      val r = reader.readZ()
      return AccessType.byOrdinal(r).get
    }

    def readConfiguration(): Configuration = {
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.GenericConfiguration => val r = readGenericConfigurationT(T); return r
        case Constants.DataPortAccessRestriction => val r = readDataPortAccessRestrictionT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of Configuration.")
          val r = readDataPortAccessRestrictionT(T)
          return r
      }
    }

    def readGenericConfiguration(): GenericConfiguration = {
      val r = readGenericConfigurationT(F)
      return r
    }

    def readGenericConfigurationT(typeParsed: B): GenericConfiguration = {
      if (!typeParsed) {
        reader.expectZ(Constants.GenericConfiguration)
      }
      val e = reader.readString()
      val comments = reader.readISZ(readAstComment _)
      return GenericConfiguration(e, comments)
    }

    def readDataPortAccessRestriction(): DataPortAccessRestriction = {
      val r = readDataPortAccessRestrictionT(F)
      return r
    }

    def readDataPortAccessRestrictionT(typeParsed: B): DataPortAccessRestriction = {
      if (!typeParsed) {
        reader.expectZ(Constants.DataPortAccessRestriction)
      }
      val component = reader.readString()
      val port = reader.readString()
      val accessType = readAccessTypeType()
      val comments = reader.readISZ(readAstComment _)
      return DataPortAccessRestriction(component, port, accessType, comments)
    }

    def readTODO(): TODO = {
      val r = readTODOT(F)
      return r
    }

    def readTODOT(typeParsed: B): TODO = {
      if (!typeParsed) {
        reader.expectZ(Constants.TODO)
      }
      val comments = reader.readISZ(readAstComment _)
      return TODO(comments)
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

  def fromAstComment(o: AstComment, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeAstComment(o)
    return w.result
  }

  def toAstComment(data: ISZ[U8]): Either[AstComment, MessagePack.ErrorMsg] = {
    def fAstComment(reader: Reader): AstComment = {
      val r = reader.readAstComment()
      return r
    }
    val r = to(data, fAstComment _)
    return r
  }

  def fromAstBasicComment(o: AstBasicComment, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeAstBasicComment(o)
    return w.result
  }

  def toAstBasicComment(data: ISZ[U8]): Either[AstBasicComment, MessagePack.ErrorMsg] = {
    def fAstBasicComment(reader: Reader): AstBasicComment = {
      val r = reader.readAstBasicComment()
      return r
    }
    val r = to(data, fAstBasicComment _)
    return r
  }

  def fromCommentProvider(o: CommentProvider, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeCommentProvider(o)
    return w.result
  }

  def toCommentProvider(data: ISZ[U8]): Either[CommentProvider, MessagePack.ErrorMsg] = {
    def fCommentProvider(reader: Reader): CommentProvider = {
      val r = reader.readCommentProvider()
      return r
    }
    val r = to(data, fCommentProvider _)
    return r
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

  def fromCamkesComponent(o: CamkesComponent, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeCamkesComponent(o)
    return w.result
  }

  def toCamkesComponent(data: ISZ[U8]): Either[CamkesComponent, MessagePack.ErrorMsg] = {
    def fCamkesComponent(reader: Reader): CamkesComponent = {
      val r = reader.readCamkesComponent()
      return r
    }
    val r = to(data, fCamkesComponent _)
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

  def fromLibraryComponent(o: LibraryComponent, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeLibraryComponent(o)
    return w.result
  }

  def toLibraryComponent(data: ISZ[U8]): Either[LibraryComponent, MessagePack.ErrorMsg] = {
    def fLibraryComponent(reader: Reader): LibraryComponent = {
      val r = reader.readLibraryComponent()
      return r
    }
    val r = to(data, fLibraryComponent _)
    return r
  }

  def fromCAmkESFeature(o: CAmkESFeature, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeCAmkESFeature(o)
    return w.result
  }

  def toCAmkESFeature(data: ISZ[U8]): Either[CAmkESFeature, MessagePack.ErrorMsg] = {
    def fCAmkESFeature(reader: Reader): CAmkESFeature = {
      val r = reader.readCAmkESFeature()
      return r
    }
    val r = to(data, fCAmkESFeature _)
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

  def fromAttribute(o: Attribute, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeAttribute(o)
    return w.result
  }

  def toAttribute(data: ISZ[U8]): Either[Attribute, MessagePack.ErrorMsg] = {
    def fAttribute(reader: Reader): Attribute = {
      val r = reader.readAttribute()
      return r
    }
    val r = to(data, fAttribute _)
    return r
  }

  def fromConfiguration(o: Configuration, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeConfiguration(o)
    return w.result
  }

  def toConfiguration(data: ISZ[U8]): Either[Configuration, MessagePack.ErrorMsg] = {
    def fConfiguration(reader: Reader): Configuration = {
      val r = reader.readConfiguration()
      return r
    }
    val r = to(data, fConfiguration _)
    return r
  }

  def fromGenericConfiguration(o: GenericConfiguration, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeGenericConfiguration(o)
    return w.result
  }

  def toGenericConfiguration(data: ISZ[U8]): Either[GenericConfiguration, MessagePack.ErrorMsg] = {
    def fGenericConfiguration(reader: Reader): GenericConfiguration = {
      val r = reader.readGenericConfiguration()
      return r
    }
    val r = to(data, fGenericConfiguration _)
    return r
  }

  def fromDataPortAccessRestriction(o: DataPortAccessRestriction, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeDataPortAccessRestriction(o)
    return w.result
  }

  def toDataPortAccessRestriction(data: ISZ[U8]): Either[DataPortAccessRestriction, MessagePack.ErrorMsg] = {
    def fDataPortAccessRestriction(reader: Reader): DataPortAccessRestriction = {
      val r = reader.readDataPortAccessRestriction()
      return r
    }
    val r = to(data, fDataPortAccessRestriction _)
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