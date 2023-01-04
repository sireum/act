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

object MTransformer {

  @record class PreResult[T](val continu: B,
                             val resultOpt: MOption[T])

  val PreResultAstBasicComment: PreResult[AstBasicComment] = PreResult(T, MNone())

  val PostResultAstBasicComment: MOption[AstBasicComment] = MNone()

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

  val PreResultLibraryComponent: PreResult[LibraryComponent] = PreResult(T, MNone())

  val PostResultLibraryComponent: MOption[LibraryComponent] = MNone()

  val PreResultUses: PreResult[Uses] = PreResult(T, MNone())

  val PostResultUses: MOption[Uses] = MNone()

  val PreResultProvides: PreResult[Provides] = PreResult(T, MNone())

  val PostResultProvides: MOption[Provides] = MNone()

  val PreResultEmits: PreResult[Emits] = PreResult(T, MNone())

  val PostResultEmits: MOption[Emits] = MNone()

  val PreResultConsumes: PreResult[Consumes] = PreResult(T, MNone())

  val PostResultConsumes: MOption[Consumes] = MNone()

  val PreResultDataport: PreResult[Dataport] = PreResult(T, MNone())

  val PostResultDataport: MOption[Dataport] = MNone()

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

  val PreResultBinarySemaphore: PreResult[BinarySemaphore] = PreResult(T, MNone())

  val PostResultBinarySemaphore: MOption[BinarySemaphore] = MNone()

  val PreResultSemaphore: PreResult[Semaphore] = PreResult(T, MNone())

  val PostResultSemaphore: MOption[Semaphore] = MNone()

  val PreResultMutex: PreResult[Mutex] = PreResult(T, MNone())

  val PostResultMutex: MOption[Mutex] = MNone()

  val PreResultAttribute: PreResult[Attribute] = PreResult(T, MNone())

  val PostResultAttribute: MOption[Attribute] = MNone()

  val PreResultGenericConfiguration: PreResult[GenericConfiguration] = PreResult(T, MNone())

  val PostResultGenericConfiguration: MOption[GenericConfiguration] = MNone()

  val PreResultDataPortAccessRestriction: PreResult[DataPortAccessRestriction] = PreResult(T, MNone())

  val PostResultDataPortAccessRestriction: MOption[DataPortAccessRestriction] = MNone()

  val PreResultTODO: PreResult[TODO] = PreResult(T, MNone())

  val PostResultTODO: MOption[TODO] = MNone()

}

import org.sireum.hamr.act.ast.MTransformer._

@msig trait MTransformer {

  def preAstComment(o: AstComment): PreResult[AstComment] = {
    o match {
      case o: AstBasicComment =>
        val r: PreResult[AstComment] = preAstBasicComment(o) match {
         case PreResult(continu, MSome(r: AstComment)) => PreResult(continu, MSome[AstComment](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AstComment")
         case PreResult(continu, _) => PreResult(continu, MNone[AstComment]())
        }
        return r
    }
  }

  def preAstBasicComment(o: AstBasicComment): PreResult[AstBasicComment] = {
    return PreResultAstBasicComment
  }

  def preCommentProvider(o: CommentProvider): PreResult[CommentProvider] = {
    o match {
      case o: Assembly =>
        val r: PreResult[CommentProvider] = preAssembly(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Composition =>
        val r: PreResult[CommentProvider] = preComposition(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Instance =>
        val r: PreResult[CommentProvider] = preInstance(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Component =>
        val r: PreResult[CommentProvider] = preComponent(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: LibraryComponent =>
        val r: PreResult[CommentProvider] = preLibraryComponent(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Uses =>
        val r: PreResult[CommentProvider] = preUses(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Provides =>
        val r: PreResult[CommentProvider] = preProvides(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Emits =>
        val r: PreResult[CommentProvider] = preEmits(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Consumes =>
        val r: PreResult[CommentProvider] = preConsumes(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Dataport =>
        val r: PreResult[CommentProvider] = preDataport(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Connection =>
        val r: PreResult[CommentProvider] = preConnection(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: ConnectionEnd =>
        val r: PreResult[CommentProvider] = preConnectionEnd(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Connector =>
        val r: PreResult[CommentProvider] = preConnector(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Procedure =>
        val r: PreResult[CommentProvider] = preProcedure(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Method =>
        val r: PreResult[CommentProvider] = preMethod(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Parameter =>
        val r: PreResult[CommentProvider] = preParameter(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: BinarySemaphore =>
        val r: PreResult[CommentProvider] = preBinarySemaphore(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Semaphore =>
        val r: PreResult[CommentProvider] = preSemaphore(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: Mutex =>
        val r: PreResult[CommentProvider] = preMutex(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: GenericConfiguration =>
        val r: PreResult[CommentProvider] = preGenericConfiguration(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: DataPortAccessRestriction =>
        val r: PreResult[CommentProvider] = preDataPortAccessRestriction(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
      case o: TODO =>
        val r: PreResult[CommentProvider] = preTODO(o) match {
         case PreResult(continu, MSome(r: CommentProvider)) => PreResult(continu, MSome[CommentProvider](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CommentProvider")
         case PreResult(continu, _) => PreResult(continu, MNone[CommentProvider]())
        }
        return r
    }
  }

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
      case o: LibraryComponent =>
        val r: PreResult[ASTObject] = preLibraryComponent(o) match {
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
      case o: BinarySemaphore =>
        val r: PreResult[ASTObject] = preBinarySemaphore(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Semaphore =>
        val r: PreResult[ASTObject] = preSemaphore(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: Mutex =>
        val r: PreResult[ASTObject] = preMutex(o) match {
         case PreResult(continu, MSome(r: ASTObject)) => PreResult(continu, MSome[ASTObject](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type ASTObject")
         case PreResult(continu, _) => PreResult(continu, MNone[ASTObject]())
        }
        return r
      case o: TODO =>
        val r: PreResult[ASTObject] = preTODO(o) match {
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

  def preCamkesComponent(o: CamkesComponent): PreResult[CamkesComponent] = {
    o match {
      case o: Component =>
        val r: PreResult[CamkesComponent] = preComponent(o) match {
         case PreResult(continu, MSome(r: CamkesComponent)) => PreResult(continu, MSome[CamkesComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CamkesComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[CamkesComponent]())
        }
        return r
      case o: LibraryComponent =>
        val r: PreResult[CamkesComponent] = preLibraryComponent(o) match {
         case PreResult(continu, MSome(r: CamkesComponent)) => PreResult(continu, MSome[CamkesComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CamkesComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[CamkesComponent]())
        }
        return r
    }
  }

  def preComponent(o: Component): PreResult[Component] = {
    return PreResultComponent
  }

  def preLibraryComponent(o: LibraryComponent): PreResult[LibraryComponent] = {
    return PreResultLibraryComponent
  }

  def preCAmkESFeature(o: CAmkESFeature): PreResult[CAmkESFeature] = {
    o match {
      case o: Uses =>
        val r: PreResult[CAmkESFeature] = preUses(o) match {
         case PreResult(continu, MSome(r: CAmkESFeature)) => PreResult(continu, MSome[CAmkESFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CAmkESFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[CAmkESFeature]())
        }
        return r
      case o: Provides =>
        val r: PreResult[CAmkESFeature] = preProvides(o) match {
         case PreResult(continu, MSome(r: CAmkESFeature)) => PreResult(continu, MSome[CAmkESFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CAmkESFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[CAmkESFeature]())
        }
        return r
      case o: Emits =>
        val r: PreResult[CAmkESFeature] = preEmits(o) match {
         case PreResult(continu, MSome(r: CAmkESFeature)) => PreResult(continu, MSome[CAmkESFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CAmkESFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[CAmkESFeature]())
        }
        return r
      case o: Consumes =>
        val r: PreResult[CAmkESFeature] = preConsumes(o) match {
         case PreResult(continu, MSome(r: CAmkESFeature)) => PreResult(continu, MSome[CAmkESFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CAmkESFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[CAmkESFeature]())
        }
        return r
      case o: Dataport =>
        val r: PreResult[CAmkESFeature] = preDataport(o) match {
         case PreResult(continu, MSome(r: CAmkESFeature)) => PreResult(continu, MSome[CAmkESFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type CAmkESFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[CAmkESFeature]())
        }
        return r
    }
  }

  def preUses(o: Uses): PreResult[Uses] = {
    return PreResultUses
  }

  def preProvides(o: Provides): PreResult[Provides] = {
    return PreResultProvides
  }

  def preEmits(o: Emits): PreResult[Emits] = {
    return PreResultEmits
  }

  def preConsumes(o: Consumes): PreResult[Consumes] = {
    return PreResultConsumes
  }

  def preDataport(o: Dataport): PreResult[Dataport] = {
    return PreResultDataport
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

  def preBinarySemaphore(o: BinarySemaphore): PreResult[BinarySemaphore] = {
    return PreResultBinarySemaphore
  }

  def preSemaphore(o: Semaphore): PreResult[Semaphore] = {
    return PreResultSemaphore
  }

  def preMutex(o: Mutex): PreResult[Mutex] = {
    return PreResultMutex
  }

  def preAttribute(o: Attribute): PreResult[Attribute] = {
    return PreResultAttribute
  }

  def preConfiguration(o: Configuration): PreResult[Configuration] = {
    o match {
      case o: GenericConfiguration =>
        val r: PreResult[Configuration] = preGenericConfiguration(o) match {
         case PreResult(continu, MSome(r: Configuration)) => PreResult(continu, MSome[Configuration](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type Configuration")
         case PreResult(continu, _) => PreResult(continu, MNone[Configuration]())
        }
        return r
      case o: DataPortAccessRestriction =>
        val r: PreResult[Configuration] = preDataPortAccessRestriction(o) match {
         case PreResult(continu, MSome(r: Configuration)) => PreResult(continu, MSome[Configuration](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type Configuration")
         case PreResult(continu, _) => PreResult(continu, MNone[Configuration]())
        }
        return r
    }
  }

  def preGenericConfiguration(o: GenericConfiguration): PreResult[GenericConfiguration] = {
    return PreResultGenericConfiguration
  }

  def preDataPortAccessRestriction(o: DataPortAccessRestriction): PreResult[DataPortAccessRestriction] = {
    return PreResultDataPortAccessRestriction
  }

  def preTODO(o: TODO): PreResult[TODO] = {
    return PreResultTODO
  }

  def postAstComment(o: AstComment): MOption[AstComment] = {
    o match {
      case o: AstBasicComment =>
        val r: MOption[AstComment] = postAstBasicComment(o) match {
         case MSome(result: AstComment) => MSome[AstComment](result)
         case MSome(_) => halt("Can only produce object of type AstComment")
         case _ => MNone[AstComment]()
        }
        return r
    }
  }

  def postAstBasicComment(o: AstBasicComment): MOption[AstBasicComment] = {
    return PostResultAstBasicComment
  }

  def postCommentProvider(o: CommentProvider): MOption[CommentProvider] = {
    o match {
      case o: Assembly =>
        val r: MOption[CommentProvider] = postAssembly(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Composition =>
        val r: MOption[CommentProvider] = postComposition(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Instance =>
        val r: MOption[CommentProvider] = postInstance(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Component =>
        val r: MOption[CommentProvider] = postComponent(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: LibraryComponent =>
        val r: MOption[CommentProvider] = postLibraryComponent(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Uses =>
        val r: MOption[CommentProvider] = postUses(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Provides =>
        val r: MOption[CommentProvider] = postProvides(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Emits =>
        val r: MOption[CommentProvider] = postEmits(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Consumes =>
        val r: MOption[CommentProvider] = postConsumes(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Dataport =>
        val r: MOption[CommentProvider] = postDataport(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Connection =>
        val r: MOption[CommentProvider] = postConnection(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: ConnectionEnd =>
        val r: MOption[CommentProvider] = postConnectionEnd(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Connector =>
        val r: MOption[CommentProvider] = postConnector(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Procedure =>
        val r: MOption[CommentProvider] = postProcedure(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Method =>
        val r: MOption[CommentProvider] = postMethod(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Parameter =>
        val r: MOption[CommentProvider] = postParameter(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: BinarySemaphore =>
        val r: MOption[CommentProvider] = postBinarySemaphore(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Semaphore =>
        val r: MOption[CommentProvider] = postSemaphore(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: Mutex =>
        val r: MOption[CommentProvider] = postMutex(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: GenericConfiguration =>
        val r: MOption[CommentProvider] = postGenericConfiguration(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: DataPortAccessRestriction =>
        val r: MOption[CommentProvider] = postDataPortAccessRestriction(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
      case o: TODO =>
        val r: MOption[CommentProvider] = postTODO(o) match {
         case MSome(result: CommentProvider) => MSome[CommentProvider](result)
         case MSome(_) => halt("Can only produce object of type CommentProvider")
         case _ => MNone[CommentProvider]()
        }
        return r
    }
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
      case o: LibraryComponent =>
        val r: MOption[ASTObject] = postLibraryComponent(o) match {
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
      case o: BinarySemaphore =>
        val r: MOption[ASTObject] = postBinarySemaphore(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Semaphore =>
        val r: MOption[ASTObject] = postSemaphore(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: Mutex =>
        val r: MOption[ASTObject] = postMutex(o) match {
         case MSome(result: ASTObject) => MSome[ASTObject](result)
         case MSome(_) => halt("Can only produce object of type ASTObject")
         case _ => MNone[ASTObject]()
        }
        return r
      case o: TODO =>
        val r: MOption[ASTObject] = postTODO(o) match {
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

  def postCamkesComponent(o: CamkesComponent): MOption[CamkesComponent] = {
    o match {
      case o: Component =>
        val r: MOption[CamkesComponent] = postComponent(o) match {
         case MSome(result: CamkesComponent) => MSome[CamkesComponent](result)
         case MSome(_) => halt("Can only produce object of type CamkesComponent")
         case _ => MNone[CamkesComponent]()
        }
        return r
      case o: LibraryComponent =>
        val r: MOption[CamkesComponent] = postLibraryComponent(o) match {
         case MSome(result: CamkesComponent) => MSome[CamkesComponent](result)
         case MSome(_) => halt("Can only produce object of type CamkesComponent")
         case _ => MNone[CamkesComponent]()
        }
        return r
    }
  }

  def postComponent(o: Component): MOption[Component] = {
    return PostResultComponent
  }

  def postLibraryComponent(o: LibraryComponent): MOption[LibraryComponent] = {
    return PostResultLibraryComponent
  }

  def postCAmkESFeature(o: CAmkESFeature): MOption[CAmkESFeature] = {
    o match {
      case o: Uses =>
        val r: MOption[CAmkESFeature] = postUses(o) match {
         case MSome(result: CAmkESFeature) => MSome[CAmkESFeature](result)
         case MSome(_) => halt("Can only produce object of type CAmkESFeature")
         case _ => MNone[CAmkESFeature]()
        }
        return r
      case o: Provides =>
        val r: MOption[CAmkESFeature] = postProvides(o) match {
         case MSome(result: CAmkESFeature) => MSome[CAmkESFeature](result)
         case MSome(_) => halt("Can only produce object of type CAmkESFeature")
         case _ => MNone[CAmkESFeature]()
        }
        return r
      case o: Emits =>
        val r: MOption[CAmkESFeature] = postEmits(o) match {
         case MSome(result: CAmkESFeature) => MSome[CAmkESFeature](result)
         case MSome(_) => halt("Can only produce object of type CAmkESFeature")
         case _ => MNone[CAmkESFeature]()
        }
        return r
      case o: Consumes =>
        val r: MOption[CAmkESFeature] = postConsumes(o) match {
         case MSome(result: CAmkESFeature) => MSome[CAmkESFeature](result)
         case MSome(_) => halt("Can only produce object of type CAmkESFeature")
         case _ => MNone[CAmkESFeature]()
        }
        return r
      case o: Dataport =>
        val r: MOption[CAmkESFeature] = postDataport(o) match {
         case MSome(result: CAmkESFeature) => MSome[CAmkESFeature](result)
         case MSome(_) => halt("Can only produce object of type CAmkESFeature")
         case _ => MNone[CAmkESFeature]()
        }
        return r
    }
  }

  def postUses(o: Uses): MOption[Uses] = {
    return PostResultUses
  }

  def postProvides(o: Provides): MOption[Provides] = {
    return PostResultProvides
  }

  def postEmits(o: Emits): MOption[Emits] = {
    return PostResultEmits
  }

  def postConsumes(o: Consumes): MOption[Consumes] = {
    return PostResultConsumes
  }

  def postDataport(o: Dataport): MOption[Dataport] = {
    return PostResultDataport
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

  def postBinarySemaphore(o: BinarySemaphore): MOption[BinarySemaphore] = {
    return PostResultBinarySemaphore
  }

  def postSemaphore(o: Semaphore): MOption[Semaphore] = {
    return PostResultSemaphore
  }

  def postMutex(o: Mutex): MOption[Mutex] = {
    return PostResultMutex
  }

  def postAttribute(o: Attribute): MOption[Attribute] = {
    return PostResultAttribute
  }

  def postConfiguration(o: Configuration): MOption[Configuration] = {
    o match {
      case o: GenericConfiguration =>
        val r: MOption[Configuration] = postGenericConfiguration(o) match {
         case MSome(result: Configuration) => MSome[Configuration](result)
         case MSome(_) => halt("Can only produce object of type Configuration")
         case _ => MNone[Configuration]()
        }
        return r
      case o: DataPortAccessRestriction =>
        val r: MOption[Configuration] = postDataPortAccessRestriction(o) match {
         case MSome(result: Configuration) => MSome[Configuration](result)
         case MSome(_) => halt("Can only produce object of type Configuration")
         case _ => MNone[Configuration]()
        }
        return r
    }
  }

  def postGenericConfiguration(o: GenericConfiguration): MOption[GenericConfiguration] = {
    return PostResultGenericConfiguration
  }

  def postDataPortAccessRestriction(o: DataPortAccessRestriction): MOption[DataPortAccessRestriction] = {
    return PostResultDataPortAccessRestriction
  }

  def postTODO(o: TODO): MOption[TODO] = {
    return PostResultTODO
  }

  def transformAstComment(o: AstComment): MOption[AstComment] = {
    val preR: PreResult[AstComment] = preAstComment(o)
    val r: MOption[AstComment] = if (preR.continu) {
      val o2: AstComment = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AstComment] = o2 match {
        case o2: AstBasicComment =>
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
    val o2: AstComment = r.getOrElse(o)
    val postR: MOption[AstComment] = postAstComment(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAstBasicComment(o: AstBasicComment): MOption[AstBasicComment] = {
    val preR: PreResult[AstBasicComment] = preAstBasicComment(o)
    val r: MOption[AstBasicComment] = if (preR.continu) {
      val o2: AstBasicComment = preR.resultOpt.getOrElse(o)
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
    val o2: AstBasicComment = r.getOrElse(o)
    val postR: MOption[AstBasicComment] = postAstBasicComment(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformCommentProvider(o: CommentProvider): MOption[CommentProvider] = {
    val preR: PreResult[CommentProvider] = preCommentProvider(o)
    val r: MOption[CommentProvider] = if (preR.continu) {
      val o2: CommentProvider = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[CommentProvider] = o2 match {
        case o2: Assembly =>
          val r0: MOption[IS[Z, Configuration]] = transformISZ(o2.configuration, transformConfiguration _)
          val r1: MOption[Composition] = transformComposition(o2.composition)
          val r2: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
            MSome(o2(configuration = r0.getOrElse(o2.configuration), composition = r1.getOrElse(o2.composition), comments = r2.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Composition =>
          val r0: MOption[IS[Z, TODO]] = transformISZ(o2.groups, transformTODO _)
          val r1: MOption[IS[Z, TODO]] = transformISZ(o2.exports, transformTODO _)
          val r2: MOption[IS[Z, Instance]] = transformISZ(o2.instances, transformInstance _)
          val r3: MOption[IS[Z, Connection]] = transformISZ(o2.connections, transformConnection _)
          val r4: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty)
            MSome(o2(groups = r0.getOrElse(o2.groups), exports = r1.getOrElse(o2.exports), instances = r2.getOrElse(o2.instances), connections = r3.getOrElse(o2.connections), comments = r4.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Instance =>
          val r0: MOption[CamkesComponent] = transformCamkesComponent(o2.component)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(component = r0.getOrElse(o2.component), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Component =>
          val r0: MOption[IS[Z, Mutex]] = transformISZ(o2.mutexes, transformMutex _)
          val r1: MOption[IS[Z, BinarySemaphore]] = transformISZ(o2.binarySemaphores, transformBinarySemaphore _)
          val r2: MOption[IS[Z, Semaphore]] = transformISZ(o2.semaphores, transformSemaphore _)
          val r3: MOption[IS[Z, Dataport]] = transformISZ(o2.dataports, transformDataport _)
          val r4: MOption[IS[Z, Emits]] = transformISZ(o2.emits, transformEmits _)
          val r5: MOption[IS[Z, Uses]] = transformISZ(o2.uses, transformUses _)
          val r6: MOption[IS[Z, Consumes]] = transformISZ(o2.consumes, transformConsumes _)
          val r7: MOption[IS[Z, Provides]] = transformISZ(o2.provides, transformProvides _)
          val r8: MOption[IS[Z, TODO]] = transformISZ(o2.attributes, transformTODO _)
          val r9: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty || r5.nonEmpty || r6.nonEmpty || r7.nonEmpty || r8.nonEmpty || r9.nonEmpty)
            MSome(o2(mutexes = r0.getOrElse(o2.mutexes), binarySemaphores = r1.getOrElse(o2.binarySemaphores), semaphores = r2.getOrElse(o2.semaphores), dataports = r3.getOrElse(o2.dataports), emits = r4.getOrElse(o2.emits), uses = r5.getOrElse(o2.uses), consumes = r6.getOrElse(o2.consumes), provides = r7.getOrElse(o2.provides), attributes = r8.getOrElse(o2.attributes), comments = r9.getOrElse(o2.comments)))
          else
            MNone()
        case o2: LibraryComponent =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Uses =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Provides =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Emits =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Consumes =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Dataport =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Connection =>
          val r0: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.from_ends, transformConnectionEnd _)
          val r1: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.to_ends, transformConnectionEnd _)
          val r2: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
            MSome(o2(from_ends = r0.getOrElse(o2.from_ends), to_ends = r1.getOrElse(o2.to_ends), comments = r2.getOrElse(o2.comments)))
          else
            MNone()
        case o2: ConnectionEnd =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Connector =>
          val r0: MOption[IS[Z, Attribute]] = transformISZ(o2.attributes, transformAttribute _)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(attributes = r0.getOrElse(o2.attributes), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Procedure =>
          val r0: MOption[IS[Z, Method]] = transformISZ(o2.methods, transformMethod _)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(methods = r0.getOrElse(o2.methods), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Method =>
          val r0: MOption[IS[Z, Parameter]] = transformISZ(o2.parameters, transformParameter _)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(parameters = r0.getOrElse(o2.parameters), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Parameter =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: BinarySemaphore =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Semaphore =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Mutex =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: GenericConfiguration =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: DataPortAccessRestriction =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: TODO =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
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
    val o2: CommentProvider = r.getOrElse(o)
    val postR: MOption[CommentProvider] = postCommentProvider(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformASTObject(o: ASTObject): MOption[ASTObject] = {
    val preR: PreResult[ASTObject] = preASTObject(o)
    val r: MOption[ASTObject] = if (preR.continu) {
      val o2: ASTObject = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[ASTObject] = o2 match {
        case o2: Assembly =>
          val r0: MOption[IS[Z, Configuration]] = transformISZ(o2.configuration, transformConfiguration _)
          val r1: MOption[Composition] = transformComposition(o2.composition)
          val r2: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
            MSome(o2(configuration = r0.getOrElse(o2.configuration), composition = r1.getOrElse(o2.composition), comments = r2.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Composition =>
          val r0: MOption[IS[Z, TODO]] = transformISZ(o2.groups, transformTODO _)
          val r1: MOption[IS[Z, TODO]] = transformISZ(o2.exports, transformTODO _)
          val r2: MOption[IS[Z, Instance]] = transformISZ(o2.instances, transformInstance _)
          val r3: MOption[IS[Z, Connection]] = transformISZ(o2.connections, transformConnection _)
          val r4: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty)
            MSome(o2(groups = r0.getOrElse(o2.groups), exports = r1.getOrElse(o2.exports), instances = r2.getOrElse(o2.instances), connections = r3.getOrElse(o2.connections), comments = r4.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Instance =>
          val r0: MOption[CamkesComponent] = transformCamkesComponent(o2.component)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(component = r0.getOrElse(o2.component), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Component =>
          val r0: MOption[IS[Z, Mutex]] = transformISZ(o2.mutexes, transformMutex _)
          val r1: MOption[IS[Z, BinarySemaphore]] = transformISZ(o2.binarySemaphores, transformBinarySemaphore _)
          val r2: MOption[IS[Z, Semaphore]] = transformISZ(o2.semaphores, transformSemaphore _)
          val r3: MOption[IS[Z, Dataport]] = transformISZ(o2.dataports, transformDataport _)
          val r4: MOption[IS[Z, Emits]] = transformISZ(o2.emits, transformEmits _)
          val r5: MOption[IS[Z, Uses]] = transformISZ(o2.uses, transformUses _)
          val r6: MOption[IS[Z, Consumes]] = transformISZ(o2.consumes, transformConsumes _)
          val r7: MOption[IS[Z, Provides]] = transformISZ(o2.provides, transformProvides _)
          val r8: MOption[IS[Z, TODO]] = transformISZ(o2.attributes, transformTODO _)
          val r9: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty || r5.nonEmpty || r6.nonEmpty || r7.nonEmpty || r8.nonEmpty || r9.nonEmpty)
            MSome(o2(mutexes = r0.getOrElse(o2.mutexes), binarySemaphores = r1.getOrElse(o2.binarySemaphores), semaphores = r2.getOrElse(o2.semaphores), dataports = r3.getOrElse(o2.dataports), emits = r4.getOrElse(o2.emits), uses = r5.getOrElse(o2.uses), consumes = r6.getOrElse(o2.consumes), provides = r7.getOrElse(o2.provides), attributes = r8.getOrElse(o2.attributes), comments = r9.getOrElse(o2.comments)))
          else
            MNone()
        case o2: LibraryComponent =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Connection =>
          val r0: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.from_ends, transformConnectionEnd _)
          val r1: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.to_ends, transformConnectionEnd _)
          val r2: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
            MSome(o2(from_ends = r0.getOrElse(o2.from_ends), to_ends = r1.getOrElse(o2.to_ends), comments = r2.getOrElse(o2.comments)))
          else
            MNone()
        case o2: ConnectionEnd =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Connector =>
          val r0: MOption[IS[Z, Attribute]] = transformISZ(o2.attributes, transformAttribute _)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(attributes = r0.getOrElse(o2.attributes), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Procedure =>
          val r0: MOption[IS[Z, Method]] = transformISZ(o2.methods, transformMethod _)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(methods = r0.getOrElse(o2.methods), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Method =>
          val r0: MOption[IS[Z, Parameter]] = transformISZ(o2.parameters, transformParameter _)
          val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(parameters = r0.getOrElse(o2.parameters), comments = r1.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Parameter =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: BinarySemaphore =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Semaphore =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Mutex =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: TODO =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
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
      val r0: MOption[IS[Z, Configuration]] = transformISZ(o2.configuration, transformConfiguration _)
      val r1: MOption[Composition] = transformComposition(o2.composition)
      val r2: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
        MSome(o2(configuration = r0.getOrElse(o2.configuration), composition = r1.getOrElse(o2.composition), comments = r2.getOrElse(o2.comments)))
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
      val r0: MOption[IS[Z, TODO]] = transformISZ(o2.groups, transformTODO _)
      val r1: MOption[IS[Z, TODO]] = transformISZ(o2.exports, transformTODO _)
      val r2: MOption[IS[Z, Instance]] = transformISZ(o2.instances, transformInstance _)
      val r3: MOption[IS[Z, Connection]] = transformISZ(o2.connections, transformConnection _)
      val r4: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty)
        MSome(o2(groups = r0.getOrElse(o2.groups), exports = r1.getOrElse(o2.exports), instances = r2.getOrElse(o2.instances), connections = r3.getOrElse(o2.connections), comments = r4.getOrElse(o2.comments)))
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
      val r0: MOption[CamkesComponent] = transformCamkesComponent(o2.component)
      val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(component = r0.getOrElse(o2.component), comments = r1.getOrElse(o2.comments)))
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

  def transformCamkesComponent(o: CamkesComponent): MOption[CamkesComponent] = {
    val preR: PreResult[CamkesComponent] = preCamkesComponent(o)
    val r: MOption[CamkesComponent] = if (preR.continu) {
      val o2: CamkesComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[CamkesComponent] = o2 match {
        case o2: Component =>
          val r0: MOption[IS[Z, Mutex]] = transformISZ(o2.mutexes, transformMutex _)
          val r1: MOption[IS[Z, BinarySemaphore]] = transformISZ(o2.binarySemaphores, transformBinarySemaphore _)
          val r2: MOption[IS[Z, Semaphore]] = transformISZ(o2.semaphores, transformSemaphore _)
          val r3: MOption[IS[Z, Dataport]] = transformISZ(o2.dataports, transformDataport _)
          val r4: MOption[IS[Z, Emits]] = transformISZ(o2.emits, transformEmits _)
          val r5: MOption[IS[Z, Uses]] = transformISZ(o2.uses, transformUses _)
          val r6: MOption[IS[Z, Consumes]] = transformISZ(o2.consumes, transformConsumes _)
          val r7: MOption[IS[Z, Provides]] = transformISZ(o2.provides, transformProvides _)
          val r8: MOption[IS[Z, TODO]] = transformISZ(o2.attributes, transformTODO _)
          val r9: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty || r5.nonEmpty || r6.nonEmpty || r7.nonEmpty || r8.nonEmpty || r9.nonEmpty)
            MSome(o2(mutexes = r0.getOrElse(o2.mutexes), binarySemaphores = r1.getOrElse(o2.binarySemaphores), semaphores = r2.getOrElse(o2.semaphores), dataports = r3.getOrElse(o2.dataports), emits = r4.getOrElse(o2.emits), uses = r5.getOrElse(o2.uses), consumes = r6.getOrElse(o2.consumes), provides = r7.getOrElse(o2.provides), attributes = r8.getOrElse(o2.attributes), comments = r9.getOrElse(o2.comments)))
          else
            MNone()
        case o2: LibraryComponent =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
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
    val o2: CamkesComponent = r.getOrElse(o)
    val postR: MOption[CamkesComponent] = postCamkesComponent(o2)
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
      val r0: MOption[IS[Z, Mutex]] = transformISZ(o2.mutexes, transformMutex _)
      val r1: MOption[IS[Z, BinarySemaphore]] = transformISZ(o2.binarySemaphores, transformBinarySemaphore _)
      val r2: MOption[IS[Z, Semaphore]] = transformISZ(o2.semaphores, transformSemaphore _)
      val r3: MOption[IS[Z, Dataport]] = transformISZ(o2.dataports, transformDataport _)
      val r4: MOption[IS[Z, Emits]] = transformISZ(o2.emits, transformEmits _)
      val r5: MOption[IS[Z, Uses]] = transformISZ(o2.uses, transformUses _)
      val r6: MOption[IS[Z, Consumes]] = transformISZ(o2.consumes, transformConsumes _)
      val r7: MOption[IS[Z, Provides]] = transformISZ(o2.provides, transformProvides _)
      val r8: MOption[IS[Z, TODO]] = transformISZ(o2.attributes, transformTODO _)
      val r9: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty || r4.nonEmpty || r5.nonEmpty || r6.nonEmpty || r7.nonEmpty || r8.nonEmpty || r9.nonEmpty)
        MSome(o2(mutexes = r0.getOrElse(o2.mutexes), binarySemaphores = r1.getOrElse(o2.binarySemaphores), semaphores = r2.getOrElse(o2.semaphores), dataports = r3.getOrElse(o2.dataports), emits = r4.getOrElse(o2.emits), uses = r5.getOrElse(o2.uses), consumes = r6.getOrElse(o2.consumes), provides = r7.getOrElse(o2.provides), attributes = r8.getOrElse(o2.attributes), comments = r9.getOrElse(o2.comments)))
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

  def transformLibraryComponent(o: LibraryComponent): MOption[LibraryComponent] = {
    val preR: PreResult[LibraryComponent] = preLibraryComponent(o)
    val r: MOption[LibraryComponent] = if (preR.continu) {
      val o2: LibraryComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: LibraryComponent = r.getOrElse(o)
    val postR: MOption[LibraryComponent] = postLibraryComponent(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformCAmkESFeature(o: CAmkESFeature): MOption[CAmkESFeature] = {
    val preR: PreResult[CAmkESFeature] = preCAmkESFeature(o)
    val r: MOption[CAmkESFeature] = if (preR.continu) {
      val o2: CAmkESFeature = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[CAmkESFeature] = o2 match {
        case o2: Uses =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Provides =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Emits =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Consumes =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: Dataport =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
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
    val o2: CAmkESFeature = r.getOrElse(o)
    val postR: MOption[CAmkESFeature] = postCAmkESFeature(o2)
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
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
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
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
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

  def transformEmits(o: Emits): MOption[Emits] = {
    val preR: PreResult[Emits] = preEmits(o)
    val r: MOption[Emits] = if (preR.continu) {
      val o2: Emits = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Emits = r.getOrElse(o)
    val postR: MOption[Emits] = postEmits(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformConsumes(o: Consumes): MOption[Consumes] = {
    val preR: PreResult[Consumes] = preConsumes(o)
    val r: MOption[Consumes] = if (preR.continu) {
      val o2: Consumes = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Consumes = r.getOrElse(o)
    val postR: MOption[Consumes] = postConsumes(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformDataport(o: Dataport): MOption[Dataport] = {
    val preR: PreResult[Dataport] = preDataport(o)
    val r: MOption[Dataport] = if (preR.continu) {
      val o2: Dataport = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Dataport = r.getOrElse(o)
    val postR: MOption[Dataport] = postDataport(o2)
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
      val r0: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.from_ends, transformConnectionEnd _)
      val r1: MOption[IS[Z, ConnectionEnd]] = transformISZ(o2.to_ends, transformConnectionEnd _)
      val r2: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty)
        MSome(o2(from_ends = r0.getOrElse(o2.from_ends), to_ends = r1.getOrElse(o2.to_ends), comments = r2.getOrElse(o2.comments)))
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
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
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
      val r0: MOption[IS[Z, Attribute]] = transformISZ(o2.attributes, transformAttribute _)
      val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(attributes = r0.getOrElse(o2.attributes), comments = r1.getOrElse(o2.comments)))
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
      val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(methods = r0.getOrElse(o2.methods), comments = r1.getOrElse(o2.comments)))
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
      val r1: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(parameters = r0.getOrElse(o2.parameters), comments = r1.getOrElse(o2.comments)))
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
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
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

  def transformBinarySemaphore(o: BinarySemaphore): MOption[BinarySemaphore] = {
    val preR: PreResult[BinarySemaphore] = preBinarySemaphore(o)
    val r: MOption[BinarySemaphore] = if (preR.continu) {
      val o2: BinarySemaphore = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BinarySemaphore = r.getOrElse(o)
    val postR: MOption[BinarySemaphore] = postBinarySemaphore(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformSemaphore(o: Semaphore): MOption[Semaphore] = {
    val preR: PreResult[Semaphore] = preSemaphore(o)
    val r: MOption[Semaphore] = if (preR.continu) {
      val o2: Semaphore = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Semaphore = r.getOrElse(o)
    val postR: MOption[Semaphore] = postSemaphore(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformMutex(o: Mutex): MOption[Mutex] = {
    val preR: PreResult[Mutex] = preMutex(o)
    val r: MOption[Mutex] = if (preR.continu) {
      val o2: Mutex = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Mutex = r.getOrElse(o)
    val postR: MOption[Mutex] = postMutex(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAttribute(o: Attribute): MOption[Attribute] = {
    val preR: PreResult[Attribute] = preAttribute(o)
    val r: MOption[Attribute] = if (preR.continu) {
      val o2: Attribute = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Attribute = r.getOrElse(o)
    val postR: MOption[Attribute] = postAttribute(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformConfiguration(o: Configuration): MOption[Configuration] = {
    val preR: PreResult[Configuration] = preConfiguration(o)
    val r: MOption[Configuration] = if (preR.continu) {
      val o2: Configuration = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[Configuration] = o2 match {
        case o2: GenericConfiguration =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
          else
            MNone()
        case o2: DataPortAccessRestriction =>
          val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(comments = r0.getOrElse(o2.comments)))
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
    val o2: Configuration = r.getOrElse(o)
    val postR: MOption[Configuration] = postConfiguration(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformGenericConfiguration(o: GenericConfiguration): MOption[GenericConfiguration] = {
    val preR: PreResult[GenericConfiguration] = preGenericConfiguration(o)
    val r: MOption[GenericConfiguration] = if (preR.continu) {
      val o2: GenericConfiguration = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: GenericConfiguration = r.getOrElse(o)
    val postR: MOption[GenericConfiguration] = postGenericConfiguration(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformDataPortAccessRestriction(o: DataPortAccessRestriction): MOption[DataPortAccessRestriction] = {
    val preR: PreResult[DataPortAccessRestriction] = preDataPortAccessRestriction(o)
    val r: MOption[DataPortAccessRestriction] = if (preR.continu) {
      val o2: DataPortAccessRestriction = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: DataPortAccessRestriction = r.getOrElse(o)
    val postR: MOption[DataPortAccessRestriction] = postDataPortAccessRestriction(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformTODO(o: TODO): MOption[TODO] = {
    val preR: PreResult[TODO] = preTODO(o)
    val r: MOption[TODO] = if (preR.continu) {
      val o2: TODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AstComment]] = transformISZ(o2.comments, transformAstComment _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(comments = r0.getOrElse(o2.comments)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: TODO = r.getOrElse(o)
    val postR: MOption[TODO] = postTODO(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

}