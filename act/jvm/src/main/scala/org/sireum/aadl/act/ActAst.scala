// #Sireum

package org.sireum.aadl.act

import org.sireum._

@sig trait ASTObject

@datatype class Assembly(configuration: String,
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

                          mutexes: ISZ[TODO],
                          binarySemaphores: ISZ[BinarySemaphore],
                          semaphores: ISZ[TODO],
                          dataports: ISZ[TODO],
                          emits: ISZ[Emits],
                          uses: ISZ[Uses],
                          consumes: ISZ[Consumes],
                          provides: ISZ[Provides],
                          includes: ISZ[String],
                          attributes: ISZ[TODO]) extends ASTObject


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


@datatype class Connection(name : String,
                           connector: Connector,
                           from_ends: ISZ[ConnectionEnd],
                           to_ends: ISZ[ConnectionEnd]) extends ASTObject

@datatype class ConnectionEnd(isFrom : B,
                              component: String,
                              end: String) extends ASTObject

@datatype class Connector(from_hardware : B,
                          from_multiple: B,
                          from_threads: Z,
                          from_type: String,
                          name: String,
                          to_hardware: B,
                          to_multiple: B,
                          to_threads: Z,
                          to_type: String) extends ASTObject

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

@datatype class TODO () extends ASTObject