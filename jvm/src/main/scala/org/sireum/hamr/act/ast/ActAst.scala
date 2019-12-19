// #Sireum

package org.sireum.hamr.act.ast

import org.sireum._

@sig trait ASTObject

@datatype class Assembly(configuration: ISZ[String],
                         composition: Composition) extends ASTObject

@datatype class Composition(groups: ISZ[TODO],
                            exports: ISZ[TODO],
                            instances: ISZ[Instance],
                            connections: ISZ[Connection]) extends ASTObject

@datatype class Instance(address_space: String,
                         name: String,
                         component: Component) extends ASTObject

@datatype class Component(control: B,
                          hardware: B,
                          name: String,

                          mutexes: ISZ[Mutex],
                          binarySemaphores: ISZ[BinarySemaphore],
                          semaphores: ISZ[Semaphore],
                          dataports: ISZ[Dataport],
                          emits: ISZ[Emits],
                          uses: ISZ[Uses],
                          consumes: ISZ[Consumes],
                          provides: ISZ[Provides],
                          includes: ISZ[String],
                          attributes: ISZ[TODO],

                          imports: ISZ[String]
                         ) extends ASTObject


@datatype class Uses(name: String,
                     typ: String,
                     optional: B)

@datatype class Provides(name: String,
                         typ: String)


@datatype class Emits(name: String,
                      typ: String)

@datatype class Consumes(name: String,
                         typ: String,
                         optional: B)

@datatype class Dataport(name: String,
                         typ: String,
                         optional: B)

@datatype class Connection(name : String,
                           connectionType: String,
                           from_ends: ISZ[ConnectionEnd],
                           to_ends: ISZ[ConnectionEnd]) extends ASTObject

@datatype class ConnectionEnd(isFrom : B,
                              component: String,
                              end: String) extends ASTObject

@enum object ConnectorType {
  'Event
  'Events
  'Procedure
  'Procedures
  'Dataport
  'Dataports
}
@datatype class Connector(name: String,

                          from_type: ConnectorType.Type,
                          from_template: Option[String],
                          from_threads: Z,
                          from_hardware : B,

                          to_type: ConnectorType.Type,
                          to_template: Option[String],
                          to_threads: Z,
                          to_hardware: B) extends ASTObject

@datatype class Procedure(name: String,
                          methods: ISZ[Method],
                          includes: ISZ[String]) extends ASTObject

@datatype class Method(name : String,
                       parameters: ISZ[Parameter],
                       returnType: Option[String]) extends ASTObject

@datatype class Parameter(array: B,
                          direction: Direction.Type,
                          name: String,
                          typ: String) extends ASTObject

@enum object Direction {
  'In
  'Out
  'Refin
}

@datatype class BinarySemaphore(name: String) extends ASTObject

@datatype class Semaphore(name: String) extends ASTObject

@datatype class Mutex(name: String) extends ASTObject

@datatype class TODO () extends ASTObject