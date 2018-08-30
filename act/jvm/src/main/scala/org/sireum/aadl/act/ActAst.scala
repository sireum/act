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
                          binarySimaphores: ISZ[TODO],
                          semaphores: ISZ[TODO],
                          dataports: ISZ[TODO],
                          emits: ISZ[TODO],
                          uses: ISZ[Uses],
                          consumes: ISZ[TODO],
                          provides: ISZ[Provides],
                          includes: ISZ[TODO],
                          attributes: ISZ[TODO]) extends ASTObject

@datatype class Uses(name: String,
                     optional: B,
                     procedure: String)

@datatype class Provides(name: String,
                         procedure: String)

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
                          methods: ISZ[Method]) extends ASTObject

@datatype class Method(name : String,
                       parameters: ISZ[Parameter]) extends ASTObject

@datatype class Parameter(array: B,
                          direction: Direction.Type,
                          name: String,
                          typ: String) extends ASTObject

@enum object Direction {
  'In
  'Out
}

@datatype class TODO () extends ASTObject