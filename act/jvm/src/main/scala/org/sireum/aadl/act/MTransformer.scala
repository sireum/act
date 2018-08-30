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

object MTransformer {

  @record class PreResult[T](continu: B,
                             resultOpt: MOption[T])

  def transformISZ[T](s: IS[Z, T], f: T => MOption[T]): MOption[IS[Z, T]] = {
    val s2: MS[Z, T] = s.toMS
    var changed: B = F
    for (i <- s2.indices) {
      val e: T = s(i)
      val r: MOption[T] = f(e)
      changed = changed || r.nonEmpty
      s2(i) = r.getOrElse(e)
    }
    if (changed) {
      return MSome(s2.toIS)
    } else {
      return MNone()
    }
  }

  val PreResultAssembly: PreResult[Assembly] = PreResult(T, MNone())

  val PostResultAssembly: MOption[Assembly] = MNone()

  val PreResultComposition: PreResult[Composition] = PreResult(T, MNone())

  val PostResultComposition: MOption[Composition] = MNone()

  val PreResultInstance: PreResult[Instance] = PreResult(T, MNone())

  val PostResultInstance: MOption[Instance] = MNone()

  val PreResultComponent: PreResult[Component] = PreResult(T, MNone())

  val PostResultComponent: MOption[Component] = MNone()

  val PreResultUses: PreResult[Uses] = PreResult(T, MNone())

  val PostResultUses: MOption[Uses] = MNone()

  val PreResultProvides: PreResult[Provides] = PreResult(T, MNone())

  val PostResultProvides: MOption[Provides] = MNone()

  val PreResultConnection: PreResult[Connection] = PreResult(T, MNone())

  val PostResultConnection: MOption[Connection] = MNone()

  val PreResultConnectionEnd: PreResult[ConnectionEnd] = PreResult(T, MNone())

  val PostResultConnectionEnd: MOption[ConnectionEnd] = MNone()

  val PreResultConnector: PreResult[Connector] = PreResult(T, MNone())

  val PostResultConnector: MOption[Connector] = MNone()

  val PreResultProcedure: PreResult[Procedure] = PreResult(T, MNone())

  val PostResultProcedure: MOption[Procedure] = MNone()

  val PreResultMethod: PreResult[Method] = PreResult(T, MNone())

  val PostResultMethod: MOption[Method] = MNone()

  val PreResultParameter: PreResult[Parameter] = PreResult(T, MNone())

  val PostResultParameter: MOption[Parameter] = MNone()

}

import MTransformer._

@msig trait MTransformer {

  def preASTObject(o: ASTObject): PreResult[ASTObject] = {
    o match {
      case o: Assembly =>
        val r: PreResult[ASTObject] = preAssembly(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Composition =>
        val r: PreResult[ASTObject] = preComposition(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Instance =>
        val r: PreResult[ASTObject] = preInstance(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Component =>
        val r: PreResult[ASTObject] = preComponent(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Connection =>
        val r: PreResult[ASTObject] = preConnection(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: ConnectionEnd =>
        val r: PreResult[ASTObject] = preConnectionEnd(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Connector =>
        val r: PreResult[ASTObject] = preConnector(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Procedure =>
        val r: PreResult[ASTObject] = preProcedure(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Method =>
        val r: PreResult[ASTObject] = preMethod(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Parameter =>
        val r: PreResult[ASTObject] = preParameter(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
    }
  }

  def preAssembly(o: Assembly): PreResult[Assembly] = {
    return PreResultAssembly
  }

  def preComposition(o: Composition): PreResult[Composition] = {
    return PreResultComposition
  }

  def preInstance(o: Instance): PreResult[Instance] = {
    return PreResultInstance
  }

  def preComponent(o: Component): PreResult[Component] = {
    return PreResultComponent
  }

  def preUses(o: Uses): PreResult[Uses] = {
    return PreResultUses
  }

  def preProvides(o: Provides): PreResult[Provides] = {
    return PreResultProvides
  }

  def preConnection(o: Connection): PreResult[Connection] = {
    return PreResultConnection
  }

  def preConnectionEnd(o: ConnectionEnd): PreResult[ConnectionEnd] = {
    return PreResultConnectionEnd
  }

  def preConnector(o: Connector): PreResult[Connector] = {
    return PreResultConnector
  }

  def preProcedure(o: Procedure): PreResult[Procedure] = {
    return PreResultProcedure
  }

  def preMethod(o: Method): PreResult[Method] = {
    return PreResultMethod
  }

  def preParameter(o: Parameter): PreResult[Parameter] = {
    return PreResultParameter
  }

  def postASTObject(o: ASTObject): MOption[ASTObject] = {
    o match {
      case o: Assembly =>
        val r: MOption[ASTObject] = postAssembly(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Composition =>
        val r: MOption[ASTObject] = postComposition(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Instance =>
        val r: MOption[ASTObject] = postInstance(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Component =>
        val r: MOption[ASTObject] = postComponent(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Connection =>
        val r: MOption[ASTObject] = postConnection(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: ConnectionEnd =>
        val r: MOption[ASTObject] = postConnectionEnd(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Connector =>
        val r: MOption[ASTObject] = postConnector(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Procedure =>
        val r: MOption[ASTObject] = postProcedure(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Method =>
        val r: MOption[ASTObject] = postMethod(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Parameter =>
        val r: MOption[ASTObject] = postParameter(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
    }
  }

  def postAssembly(o: Assembly): MOption[Assembly] = {
    return PostResultAssembly
  }

  def postComposition(o: Composition): MOption[Composition] = {
    return PostResultComposition
  }

  def postInstance(o: Instance): MOption[Instance] = {
    return PostResultInstance
  }

  def postComponent(o: Component): MOption[Component] = {
    return PostResultComponent
  }

  def postUses(o: Uses): MOption[Uses] = {
    return PostResultUses
  }

  def postProvides(o: Provides): MOption[Provides] = {
    return PostResultProvides
  }

  def postConnection(o: Connection): MOption[Connection] = {
    return PostResultConnection
  }

  def postConnectionEnd(o: ConnectionEnd): MOption[ConnectionEnd] = {
    return PostResultConnectionEnd
  }

  def postConnector(o: Connector): MOption[Connector] = {
    return PostResultConnector
  }

  def postProcedure(o: Procedure): MOption[Procedure] = {
    return PostResultProcedure
  }

  def postMethod(o: Method): MOption[Method] = {
    return PostResultMethod
  }

  def postParameter(o: Parameter): MOption[Parameter] = {
    return PostResultParameter
  }

  def transformASTObject(o: ASTObject): MOption[ASTObject] = {
    val preR: PreResult[ASTObject] = preASTObject(o)
    val r: MOption[ASTObject] = if (preR.continu) {
      val o2: ASTObject = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[ASTObject] = o2 match {
        case o2: Assembly =>
          val r0: MOption[Composition] = transformComposition(o2.composition)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(composition = r0.getOrElse(o2.composition)))
          else
            MNone()
        case o2: Composition =>
          val r0: MOption[IS[Z, Instance]] = transformISZ(o2.instances, transformInstance _)
          val r1: MOption[IS[Z, Connection]] = transformISZ(o2.connections, transformConnection _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(instances = r0.getOrElse(o2.instances), connections = r1.getOrElse(o2.connections)))
          else
            MNone()
        case o2: Instance =>
          val r0: MOption[Component] = transformComponent(o2.component)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(component = r0.getOrElse(o2.component)))
          else
            MNone()
        case o2: Component =>
          val r0: MOption[IS[Z, Uses]] = transformISZ(o2.uses, transformUses _)
          val r1: MOption[IS[Z, Provides]] = transformISZ(o2.provides, transformProvides _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(uses = r0.getOrElse(o2.uses), provides = r1.getOrElse(o2.provides)))
          else
            MNone()
        case o2: Connection =>
          val r0: MOption[Connector] = transformConnector(o2.connector)
          val r1: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.from_ends, transformConnectionEnd _)
          val r2: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.to_ends, transformConnectionEnd _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
            MSome(o2(connector = r0.getOrElse(o2.connector), from_ends = r1.getOrElse(o2.from_ends), to_ends = r2.getOrElse(o2.to_ends)))
          else
            MNone()
        case o2: ConnectionEnd =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: Connector =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: Procedure =>
          val r0: MOption[IS[Z, Method]] = transformISZ(o2.methods, transformMethod _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(methods = r0.getOrElse(o2.methods)))
          else
            MNone()
        case o2: Method =>
          val r0: MOption[IS[Z, Parameter]] = transformISZ(o2.parameters, transformParameter _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(parameters = r0.getOrElse(o2.parameters)))
          else
            MNone()
        case o2: Parameter =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: ASTObject = r.getOrElse(o)
    val postR: MOption[ASTObject] = postASTObject(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAssembly(o: Assembly): MOption[Assembly] = {
    val preR: PreResult[Assembly] = preAssembly(o)
    val r: MOption[Assembly] = if (preR.continu) {
      val o2: Assembly = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[Composition] = transformComposition(o2.composition)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(composition = r0.getOrElse(o2.composition)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Assembly = r.getOrElse(o)
    val postR: MOption[Assembly] = postAssembly(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformComposition(o: Composition): MOption[Composition] = {
    val preR: PreResult[Composition] = preComposition(o)
    val r: MOption[Composition] = if (preR.continu) {
      val o2: Composition = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, Instance]] = transformISZ(o2.instances, transformInstance _)
      val r1: MOption[IS[Z, Connection]] = transformISZ(o2.connections, transformConnection _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(instances = r0.getOrElse(o2.instances), connections = r1.getOrElse(o2.connections)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Composition = r.getOrElse(o)
    val postR: MOption[Composition] = postComposition(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformInstance(o: Instance): MOption[Instance] = {
    val preR: PreResult[Instance] = preInstance(o)
    val r: MOption[Instance] = if (preR.continu) {
      val o2: Instance = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[Component] = transformComponent(o2.component)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(component = r0.getOrElse(o2.component)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Instance = r.getOrElse(o)
    val postR: MOption[Instance] = postInstance(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformComponent(o: Component): MOption[Component] = {
    val preR: PreResult[Component] = preComponent(o)
    val r: MOption[Component] = if (preR.continu) {
      val o2: Component = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, Uses]] = transformISZ(o2.uses, transformUses _)
      val r1: MOption[IS[Z, Provides]] = transformISZ(o2.provides, transformProvides _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(uses = r0.getOrElse(o2.uses), provides = r1.getOrElse(o2.provides)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Component = r.getOrElse(o)
    val postR: MOption[Component] = postComponent(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformUses(o: Uses): MOption[Uses] = {
    val preR: PreResult[Uses] = preUses(o)
    val r: MOption[Uses] = if (preR.continu) {
      val o2: Uses = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Uses = r.getOrElse(o)
    val postR: MOption[Uses] = postUses(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformProvides(o: Provides): MOption[Provides] = {
    val preR: PreResult[Provides] = preProvides(o)
    val r: MOption[Provides] = if (preR.continu) {
      val o2: Provides = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Provides = r.getOrElse(o)
    val postR: MOption[Provides] = postProvides(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformConnection(o: Connection): MOption[Connection] = {
    val preR: PreResult[Connection] = preConnection(o)
    val r: MOption[Connection] = if (preR.continu) {
      val o2: Connection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[Connector] = transformConnector(o2.connector)
      val r1: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.from_ends, transformConnectionEnd _)
      val r2: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.to_ends, transformConnectionEnd _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
        MSome(o2(connector = r0.getOrElse(o2.connector), from_ends = r1.getOrElse(o2.from_ends), to_ends = r2.getOrElse(o2.to_ends)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Connection = r.getOrElse(o)
    val postR: MOption[Connection] = postConnection(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformConnectionEnd(o: ConnectionEnd): MOption[ConnectionEnd] = {
    val preR: PreResult[ConnectionEnd] = preConnectionEnd(o)
    val r: MOption[ConnectionEnd] = if (preR.continu) {
      val o2: ConnectionEnd = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: ConnectionEnd = r.getOrElse(o)
    val postR: MOption[ConnectionEnd] = postConnectionEnd(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformConnector(o: Connector): MOption[Connector] = {
    val preR: PreResult[Connector] = preConnector(o)
    val r: MOption[Connector] = if (preR.continu) {
      val o2: Connector = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Connector = r.getOrElse(o)
    val postR: MOption[Connector] = postConnector(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformProcedure(o: Procedure): MOption[Procedure] = {
    val preR: PreResult[Procedure] = preProcedure(o)
    val r: MOption[Procedure] = if (preR.continu) {
      val o2: Procedure = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, Method]] = transformISZ(o2.methods, transformMethod _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(methods = r0.getOrElse(o2.methods)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Procedure = r.getOrElse(o)
    val postR: MOption[Procedure] = postProcedure(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformMethod(o: Method): MOption[Method] = {
    val preR: PreResult[Method] = preMethod(o)
    val r: MOption[Method] = if (preR.continu) {
      val o2: Method = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, Parameter]] = transformISZ(o2.parameters, transformParameter _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(parameters = r0.getOrElse(o2.parameters)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Method = r.getOrElse(o)
    val postR: MOption[Method] = postMethod(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformParameter(o: Parameter): MOption[Parameter] = {
    val preR: PreResult[Parameter] = preParameter(o)
    val r: MOption[Parameter] = if (preR.continu) {
      val o2: Parameter = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Parameter = r.getOrElse(o)
    val postR: MOption[Parameter] = postParameter(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

}