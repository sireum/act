// #Sireum

package org.sireum.hamr.act.ast

import org.sireum._

@sig trait AstComment

@enum object CommentLocation {
  "PRE"
  "INLINE"
  "POST"
}

@datatype class AstBasicComment(val location: CommentLocation.Type,
                                val comment: String) extends AstComment

@sig trait CommentProvider {
  def comments: ISZ[AstComment]
}

@sig trait ASTObject extends CommentProvider


@datatype class Assembly(val configuration: ISZ[Configuration],
                         val configurationMacros: ISZ[String],
                         val composition: Composition,

                         val comments: ISZ[AstComment]
                        ) extends ASTObject

@datatype class Composition(val groups: ISZ[TODO],
                            val exports: ISZ[TODO],
                            val instances: ISZ[Instance],
                            val connections: ISZ[Connection],

                            // inserted at end of component definition
                            val externalEntities: ISZ[String],

                            val comments: ISZ[AstComment]
                           ) extends ASTObject

@datatype class Instance(val address_space: String,
                         val name: String,
                         val component: CamkesComponent,
                         val comments: ISZ[AstComment]) extends ASTObject

@sig trait CamkesComponent extends ASTObject {
  def name: String
}

@datatype class Component(val control: B,
                          val hardware: B,
                          val name: String,

                          val mutexes: ISZ[Mutex],
                          val binarySemaphores: ISZ[BinarySemaphore],
                          val semaphores: ISZ[Semaphore],
                          val dataports: ISZ[Dataport],
                          val emits: ISZ[Emits],
                          val uses: ISZ[Uses],
                          val consumes: ISZ[Consumes],
                          val provides: ISZ[Provides],
                          val includes: ISZ[String],
                          val attributes: ISZ[TODO],
                          val imports: ISZ[String],

                          // inserted before component def
                          val preprocessorIncludes: ISZ[String],

                          // inserted at end of component definition
                          val externalEntities: ISZ[String],

                          val comments: ISZ[AstComment]
                         ) extends CamkesComponent


@datatype class LibraryComponent(val name: String,
                                 val ports: ISZ[String],

                                 val comments: ISZ[AstComment]) extends CamkesComponent

@sig trait CAmkESFeature extends CommentProvider {
  def name: String
  def typ: String
}

@datatype class Uses(val name: String,
                     val typ: String,
                     val optional: B,

                     val comments: ISZ[AstComment]) extends CAmkESFeature

@datatype class Provides(val name: String,
                         val typ: String,

                         val comments: ISZ[AstComment]) extends CAmkESFeature


@datatype class Emits(val name: String,
                      val typ: String,

                      val comments: ISZ[AstComment]) extends CAmkESFeature

@datatype class Consumes(val name: String,
                         val typ: String,
                         val optional: B,

                         val comments: ISZ[AstComment]) extends CAmkESFeature

@datatype class Dataport(val name: String,
                         val typ: String,
                         val optional: B,

                         val comments: ISZ[AstComment]) extends CAmkESFeature


@datatype class Connection(val name : String,
                           val connectionType: String,
                           val from_ends: ISZ[ConnectionEnd],
                           val to_ends: ISZ[ConnectionEnd],

                           val comments: ISZ[AstComment]) extends ASTObject

@datatype class ConnectionEnd(val isFrom : B,
                              val component: String,
                              val end: String,

                              val comments: ISZ[AstComment]) extends ASTObject

@enum object ConnectorType {
  'Event
  'Events
  'Procedure
  'Procedures
  'Dataport
  'Dataports
}

@datatype class Connector(val name: String,

                          val from_type: ConnectorType.Type,
                          val from_template: Option[String],
                          val from_threads: Z,
                          val from_hardware : B,

                          val to_type: ConnectorType.Type,
                          val to_template: Option[String],
                          val to_threads: Z,
                          val to_hardware: B,

                          val attributes: ISZ[Attribute],

                          val comments: ISZ[AstComment]) extends ASTObject

@datatype class Procedure(val name: String,
                          val methods: ISZ[Method],
                          val includes: ISZ[String],

                          val comments: ISZ[AstComment]) extends ASTObject

@datatype class Method(val name : String,
                       val parameters: ISZ[Parameter],
                       val returnType: Option[String],

                       val comments: ISZ[AstComment]) extends ASTObject

@datatype class Parameter(val array: B,
                          val direction: Direction.Type,
                          val name: String,
                          val typ: String,

                          val comments: ISZ[AstComment]) extends ASTObject

@enum object Direction {
  'In
  'Out
  'Refin
}

@datatype class BinarySemaphore(val name: String,
                                val comments: ISZ[AstComment]) extends ASTObject

@datatype class Semaphore(val name: String,
                          val comments: ISZ[AstComment]) extends ASTObject

@datatype class Mutex(val name: String,
                      val comments: ISZ[AstComment]) extends ASTObject

@datatype class Attribute(val typ: String,
                          val name: String,
                          val value: String,
                          val comments: ISZ[AstComment])

@enum object AccessType {
  "R"
  "W"
  "RW"
}

@sig trait Configuration extends CommentProvider

@datatype class GenericConfiguration(val e: String,
                                     val comments: ISZ[AstComment]) extends Configuration

@datatype class DataPortAccessRestriction (val component: String,
                                           val port: String,
                                           val accessType: AccessType.Type,
                                           val comments: ISZ[AstComment]) extends Configuration



@datatype class TODO (val comments: ISZ[AstComment]) extends ASTObject