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

object Transformer {

  @datatype class PreResult[Context, T](ctx: Context,
                                        continu: B,
                                        resultOpt: Option[T])

  @datatype class Result[Context, T](ctx: Context,
                                     resultOpt: Option[T])

  @sig trait PrePost[Context] {

    @pure def preASTObject(ctx: Context, o: ASTObject): PreResult[Context, ASTObject] = {
      o match {
        case o: Assembly =>
          val r: PreResult[Context, ASTObject] = preAssembly(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Composition =>
          val r: PreResult[Context, ASTObject] = preComposition(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Instance =>
          val r: PreResult[Context, ASTObject] = preInstance(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Component =>
          val r: PreResult[Context, ASTObject] = preComponent(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Connection =>
          val r: PreResult[Context, ASTObject] = preConnection(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: ConnectionEnd =>
          val r: PreResult[Context, ASTObject] = preConnectionEnd(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Connector =>
          val r: PreResult[Context, ASTObject] = preConnector(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Procedure =>
          val r: PreResult[Context, ASTObject] = preProcedure(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Method =>
          val r: PreResult[Context, ASTObject] = preMethod(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Parameter =>
          val r: PreResult[Context, ASTObject] = preParameter(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: BinarySemaphore =>
          val r: PreResult[Context, ASTObject] = preBinarySemaphore(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: Semaphore =>
          val r: PreResult[Context, ASTObject] = preSemaphore(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
        case o: TODO =>
          val r: PreResult[Context, ASTObject] = preTODO(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: ASTObject)) => PreResult(preCtx, continu, Some[ASTObject](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type ASTObject")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[ASTObject]())
          }
          return r
      }
    }

    @pure def preAssembly(ctx: Context, o: Assembly): PreResult[Context, Assembly] = {
      return PreResult(ctx, T, None())
    }

    @pure def preComposition(ctx: Context, o: Composition): PreResult[Context, Composition] = {
      return PreResult(ctx, T, None())
    }

    @pure def preInstance(ctx: Context, o: Instance): PreResult[Context, Instance] = {
      return PreResult(ctx, T, None())
    }

    @pure def preComponent(ctx: Context, o: Component): PreResult[Context, Component] = {
      return PreResult(ctx, T, None())
    }

    @pure def preUses(ctx: Context, o: Uses): PreResult[Context, Uses] = {
      return PreResult(ctx, T, None())
    }

    @pure def preProvides(ctx: Context, o: Provides): PreResult[Context, Provides] = {
      return PreResult(ctx, T, None())
    }

    @pure def preEmits(ctx: Context, o: Emits): PreResult[Context, Emits] = {
      return PreResult(ctx, T, None())
    }

    @pure def preConsumes(ctx: Context, o: Consumes): PreResult[Context, Consumes] = {
      return PreResult(ctx, T, None())
    }

    @pure def preConnection(ctx: Context, o: Connection): PreResult[Context, Connection] = {
      return PreResult(ctx, T, None())
    }

    @pure def preConnectionEnd(ctx: Context, o: ConnectionEnd): PreResult[Context, ConnectionEnd] = {
      return PreResult(ctx, T, None())
    }

    @pure def preConnector(ctx: Context, o: Connector): PreResult[Context, Connector] = {
      return PreResult(ctx, T, None())
    }

    @pure def preProcedure(ctx: Context, o: Procedure): PreResult[Context, Procedure] = {
      return PreResult(ctx, T, None())
    }

    @pure def preMethod(ctx: Context, o: Method): PreResult[Context, Method] = {
      return PreResult(ctx, T, None())
    }

    @pure def preParameter(ctx: Context, o: Parameter): PreResult[Context, Parameter] = {
      return PreResult(ctx, T, None())
    }

    @pure def preBinarySemaphore(ctx: Context, o: BinarySemaphore): PreResult[Context, BinarySemaphore] = {
      return PreResult(ctx, T, None())
    }

    @pure def preSemaphore(ctx: Context, o: Semaphore): PreResult[Context, Semaphore] = {
      return PreResult(ctx, T, None())
    }

    @pure def preTODO(ctx: Context, o: TODO): PreResult[Context, TODO] = {
      return PreResult(ctx, T, None())
    }

    @pure def postASTObject(ctx: Context, o: ASTObject): Result[Context, ASTObject] = {
      o match {
        case o: Assembly =>
          val r: Result[Context, ASTObject] = postAssembly(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Composition =>
          val r: Result[Context, ASTObject] = postComposition(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Instance =>
          val r: Result[Context, ASTObject] = postInstance(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Component =>
          val r: Result[Context, ASTObject] = postComponent(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Connection =>
          val r: Result[Context, ASTObject] = postConnection(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: ConnectionEnd =>
          val r: Result[Context, ASTObject] = postConnectionEnd(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Connector =>
          val r: Result[Context, ASTObject] = postConnector(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Procedure =>
          val r: Result[Context, ASTObject] = postProcedure(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Method =>
          val r: Result[Context, ASTObject] = postMethod(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Parameter =>
          val r: Result[Context, ASTObject] = postParameter(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: BinarySemaphore =>
          val r: Result[Context, ASTObject] = postBinarySemaphore(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: Semaphore =>
          val r: Result[Context, ASTObject] = postSemaphore(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
        case o: TODO =>
          val r: Result[Context, ASTObject] = postTODO(ctx, o) match {
           case Result(postCtx, Some(result: ASTObject)) => Result(postCtx, Some[ASTObject](result))
           case Result(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case Result(postCtx, _) => Result(postCtx, None[ASTObject]())
          }
          return r
      }
    }

    @pure def postAssembly(ctx: Context, o: Assembly): Result[Context, Assembly] = {
      return Result(ctx, None())
    }

    @pure def postComposition(ctx: Context, o: Composition): Result[Context, Composition] = {
      return Result(ctx, None())
    }

    @pure def postInstance(ctx: Context, o: Instance): Result[Context, Instance] = {
      return Result(ctx, None())
    }

    @pure def postComponent(ctx: Context, o: Component): Result[Context, Component] = {
      return Result(ctx, None())
    }

    @pure def postUses(ctx: Context, o: Uses): Result[Context, Uses] = {
      return Result(ctx, None())
    }

    @pure def postProvides(ctx: Context, o: Provides): Result[Context, Provides] = {
      return Result(ctx, None())
    }

    @pure def postEmits(ctx: Context, o: Emits): Result[Context, Emits] = {
      return Result(ctx, None())
    }

    @pure def postConsumes(ctx: Context, o: Consumes): Result[Context, Consumes] = {
      return Result(ctx, None())
    }

    @pure def postConnection(ctx: Context, o: Connection): Result[Context, Connection] = {
      return Result(ctx, None())
    }

    @pure def postConnectionEnd(ctx: Context, o: ConnectionEnd): Result[Context, ConnectionEnd] = {
      return Result(ctx, None())
    }

    @pure def postConnector(ctx: Context, o: Connector): Result[Context, Connector] = {
      return Result(ctx, None())
    }

    @pure def postProcedure(ctx: Context, o: Procedure): Result[Context, Procedure] = {
      return Result(ctx, None())
    }

    @pure def postMethod(ctx: Context, o: Method): Result[Context, Method] = {
      return Result(ctx, None())
    }

    @pure def postParameter(ctx: Context, o: Parameter): Result[Context, Parameter] = {
      return Result(ctx, None())
    }

    @pure def postBinarySemaphore(ctx: Context, o: BinarySemaphore): Result[Context, BinarySemaphore] = {
      return Result(ctx, None())
    }

    @pure def postSemaphore(ctx: Context, o: Semaphore): Result[Context, Semaphore] = {
      return Result(ctx, None())
    }

    @pure def postTODO(ctx: Context, o: TODO): Result[Context, TODO] = {
      return Result(ctx, None())
    }

  }

  @pure def transformISZ[Context, T](ctx: Context, s: IS[Z, T], f: (Context, T) => Result[Context, T] @pure): Result[Context, IS[Z, T]] = {
    val s2: MS[Z, T] = s.toMS
    var changed: B = F
    var ctxi = ctx
    for (i <- s2.indices) {
      val e: T = s(i)
      val r: Result[Context, T] = f(ctxi, e)
      ctxi = r.ctx
      changed = changed || r.resultOpt.nonEmpty
      s2(i) = r.resultOpt.getOrElse(e)
    }
    if (changed) {
      return Result(ctxi, Some(s2.toIS))
    } else {
      return Result[Context, IS[Z, T]](ctxi, None[IS[Z, T]]())
    }
  }

}

import Transformer._

@datatype class Transformer[Context](pp: PrePost[Context]) {

  @pure def transformASTObject(ctx: Context, o: ASTObject): Result[Context, ASTObject] = {
    val preR: PreResult[Context, ASTObject] = pp.preASTObject(ctx, o)
    val r: Result[Context, ASTObject] = if (preR.continu) {
      val o2: ASTObject = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: Result[Context, ASTObject] = o2 match {
        case o2: Assembly =>
          val r0: Result[Context, Composition] = transformComposition(ctx, o2.composition)
          if (hasChanged || r0.resultOpt.nonEmpty)
            Result(r0.ctx, Some(o2(composition = r0.resultOpt.getOrElse(o2.composition))))
          else
            Result(r0.ctx, None())
        case o2: Composition =>
          val r0: Result[Context, IS[Z, TODO]] = transformISZ(ctx, o2.groups, transformTODO _)
          val r1: Result[Context, IS[Z, TODO]] = transformISZ(r0.ctx, o2.exports, transformTODO _)
          val r2: Result[Context, IS[Z, Instance]] = transformISZ(r1.ctx, o2.instances, transformInstance _)
          val r3: Result[Context, IS[Z, Connection]] = transformISZ(r2.ctx, o2.connections, transformConnection _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
            Result(r3.ctx, Some(o2(groups = r0.resultOpt.getOrElse(o2.groups), exports = r1.resultOpt.getOrElse(o2.exports), instances = r2.resultOpt.getOrElse(o2.instances), connections = r3.resultOpt.getOrElse(o2.connections))))
          else
            Result(r3.ctx, None())
        case o2: Instance =>
          val r0: Result[Context, Component] = transformComponent(ctx, o2.component)
          if (hasChanged || r0.resultOpt.nonEmpty)
            Result(r0.ctx, Some(o2(component = r0.resultOpt.getOrElse(o2.component))))
          else
            Result(r0.ctx, None())
        case o2: Component =>
          val r0: Result[Context, IS[Z, TODO]] = transformISZ(ctx, o2.mutexes, transformTODO _)
          val r1: Result[Context, IS[Z, BinarySemaphore]] = transformISZ(r0.ctx, o2.binarySemaphores, transformBinarySemaphore _)
          val r2: Result[Context, IS[Z, Semaphore]] = transformISZ(r1.ctx, o2.semaphores, transformSemaphore _)
          val r3: Result[Context, IS[Z, TODO]] = transformISZ(r2.ctx, o2.dataports, transformTODO _)
          val r4: Result[Context, IS[Z, Emits]] = transformISZ(r3.ctx, o2.emits, transformEmits _)
          val r5: Result[Context, IS[Z, Uses]] = transformISZ(r4.ctx, o2.uses, transformUses _)
          val r6: Result[Context, IS[Z, Consumes]] = transformISZ(r5.ctx, o2.consumes, transformConsumes _)
          val r7: Result[Context, IS[Z, Provides]] = transformISZ(r6.ctx, o2.provides, transformProvides _)
          val r8: Result[Context, IS[Z, TODO]] = transformISZ(r7.ctx, o2.attributes, transformTODO _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty || r4.resultOpt.nonEmpty || r5.resultOpt.nonEmpty || r6.resultOpt.nonEmpty || r7.resultOpt.nonEmpty || r8.resultOpt.nonEmpty)
            Result(r8.ctx, Some(o2(mutexes = r0.resultOpt.getOrElse(o2.mutexes), binarySemaphores = r1.resultOpt.getOrElse(o2.binarySemaphores), semaphores = r2.resultOpt.getOrElse(o2.semaphores), dataports = r3.resultOpt.getOrElse(o2.dataports), emits = r4.resultOpt.getOrElse(o2.emits), uses = r5.resultOpt.getOrElse(o2.uses), consumes = r6.resultOpt.getOrElse(o2.consumes), provides = r7.resultOpt.getOrElse(o2.provides), attributes = r8.resultOpt.getOrElse(o2.attributes))))
          else
            Result(r8.ctx, None())
        case o2: Connection =>
          val r0: Result[Context, IS[Z, ConnectionEnd]] = transformISZ(ctx, o2.from_ends, transformConnectionEnd _)
          val r1: Result[Context, IS[Z, ConnectionEnd]] = transformISZ(r0.ctx, o2.to_ends, transformConnectionEnd _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            Result(r1.ctx, Some(o2(from_ends = r0.resultOpt.getOrElse(o2.from_ends), to_ends = r1.resultOpt.getOrElse(o2.to_ends))))
          else
            Result(r1.ctx, None())
        case o2: ConnectionEnd =>
          if (hasChanged)
            Result(ctx, Some(o2))
          else
            Result(ctx, None())
        case o2: Connector =>
          if (hasChanged)
            Result(ctx, Some(o2))
          else
            Result(ctx, None())
        case o2: Procedure =>
          val r0: Result[Context, IS[Z, Method]] = transformISZ(ctx, o2.methods, transformMethod _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            Result(r0.ctx, Some(o2(methods = r0.resultOpt.getOrElse(o2.methods))))
          else
            Result(r0.ctx, None())
        case o2: Method =>
          val r0: Result[Context, IS[Z, Parameter]] = transformISZ(ctx, o2.parameters, transformParameter _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            Result(r0.ctx, Some(o2(parameters = r0.resultOpt.getOrElse(o2.parameters))))
          else
            Result(r0.ctx, None())
        case o2: Parameter =>
          if (hasChanged)
            Result(ctx, Some(o2))
          else
            Result(ctx, None())
        case o2: BinarySemaphore =>
          if (hasChanged)
            Result(ctx, Some(o2))
          else
            Result(ctx, None())
        case o2: Semaphore =>
          if (hasChanged)
            Result(ctx, Some(o2))
          else
            Result(ctx, None())
        case o2: TODO =>
          if (hasChanged)
            Result(ctx, Some(o2))
          else
            Result(ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: ASTObject = r.resultOpt.getOrElse(o)
    val postR: Result[Context, ASTObject] = pp.postASTObject(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformAssembly(ctx: Context, o: Assembly): Result[Context, Assembly] = {
    val preR: PreResult[Context, Assembly] = pp.preAssembly(ctx, o)
    val r: Result[Context, Assembly] = if (preR.continu) {
      val o2: Assembly = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, Composition] = transformComposition(ctx, o2.composition)
      if (hasChanged || r0.resultOpt.nonEmpty)
        Result(r0.ctx, Some(o2(composition = r0.resultOpt.getOrElse(o2.composition))))
      else
        Result(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Assembly = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Assembly] = pp.postAssembly(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformComposition(ctx: Context, o: Composition): Result[Context, Composition] = {
    val preR: PreResult[Context, Composition] = pp.preComposition(ctx, o)
    val r: Result[Context, Composition] = if (preR.continu) {
      val o2: Composition = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, IS[Z, TODO]] = transformISZ(ctx, o2.groups, transformTODO _)
      val r1: Result[Context, IS[Z, TODO]] = transformISZ(r0.ctx, o2.exports, transformTODO _)
      val r2: Result[Context, IS[Z, Instance]] = transformISZ(r1.ctx, o2.instances, transformInstance _)
      val r3: Result[Context, IS[Z, Connection]] = transformISZ(r2.ctx, o2.connections, transformConnection _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
        Result(r3.ctx, Some(o2(groups = r0.resultOpt.getOrElse(o2.groups), exports = r1.resultOpt.getOrElse(o2.exports), instances = r2.resultOpt.getOrElse(o2.instances), connections = r3.resultOpt.getOrElse(o2.connections))))
      else
        Result(r3.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Composition = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Composition] = pp.postComposition(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformInstance(ctx: Context, o: Instance): Result[Context, Instance] = {
    val preR: PreResult[Context, Instance] = pp.preInstance(ctx, o)
    val r: Result[Context, Instance] = if (preR.continu) {
      val o2: Instance = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, Component] = transformComponent(ctx, o2.component)
      if (hasChanged || r0.resultOpt.nonEmpty)
        Result(r0.ctx, Some(o2(component = r0.resultOpt.getOrElse(o2.component))))
      else
        Result(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Instance = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Instance] = pp.postInstance(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformComponent(ctx: Context, o: Component): Result[Context, Component] = {
    val preR: PreResult[Context, Component] = pp.preComponent(ctx, o)
    val r: Result[Context, Component] = if (preR.continu) {
      val o2: Component = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, IS[Z, TODO]] = transformISZ(ctx, o2.mutexes, transformTODO _)
      val r1: Result[Context, IS[Z, BinarySemaphore]] = transformISZ(r0.ctx, o2.binarySemaphores, transformBinarySemaphore _)
      val r2: Result[Context, IS[Z, Semaphore]] = transformISZ(r1.ctx, o2.semaphores, transformSemaphore _)
      val r3: Result[Context, IS[Z, TODO]] = transformISZ(r2.ctx, o2.dataports, transformTODO _)
      val r4: Result[Context, IS[Z, Emits]] = transformISZ(r3.ctx, o2.emits, transformEmits _)
      val r5: Result[Context, IS[Z, Uses]] = transformISZ(r4.ctx, o2.uses, transformUses _)
      val r6: Result[Context, IS[Z, Consumes]] = transformISZ(r5.ctx, o2.consumes, transformConsumes _)
      val r7: Result[Context, IS[Z, Provides]] = transformISZ(r6.ctx, o2.provides, transformProvides _)
      val r8: Result[Context, IS[Z, TODO]] = transformISZ(r7.ctx, o2.attributes, transformTODO _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty || r4.resultOpt.nonEmpty || r5.resultOpt.nonEmpty || r6.resultOpt.nonEmpty || r7.resultOpt.nonEmpty || r8.resultOpt.nonEmpty)
        Result(r8.ctx, Some(o2(mutexes = r0.resultOpt.getOrElse(o2.mutexes), binarySemaphores = r1.resultOpt.getOrElse(o2.binarySemaphores), semaphores = r2.resultOpt.getOrElse(o2.semaphores), dataports = r3.resultOpt.getOrElse(o2.dataports), emits = r4.resultOpt.getOrElse(o2.emits), uses = r5.resultOpt.getOrElse(o2.uses), consumes = r6.resultOpt.getOrElse(o2.consumes), provides = r7.resultOpt.getOrElse(o2.provides), attributes = r8.resultOpt.getOrElse(o2.attributes))))
      else
        Result(r8.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Component = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Component] = pp.postComponent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformUses(ctx: Context, o: Uses): Result[Context, Uses] = {
    val preR: PreResult[Context, Uses] = pp.preUses(ctx, o)
    val r: Result[Context, Uses] = if (preR.continu) {
      val o2: Uses = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Uses = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Uses] = pp.postUses(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformProvides(ctx: Context, o: Provides): Result[Context, Provides] = {
    val preR: PreResult[Context, Provides] = pp.preProvides(ctx, o)
    val r: Result[Context, Provides] = if (preR.continu) {
      val o2: Provides = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Provides = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Provides] = pp.postProvides(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformEmits(ctx: Context, o: Emits): Result[Context, Emits] = {
    val preR: PreResult[Context, Emits] = pp.preEmits(ctx, o)
    val r: Result[Context, Emits] = if (preR.continu) {
      val o2: Emits = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Emits = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Emits] = pp.postEmits(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformConsumes(ctx: Context, o: Consumes): Result[Context, Consumes] = {
    val preR: PreResult[Context, Consumes] = pp.preConsumes(ctx, o)
    val r: Result[Context, Consumes] = if (preR.continu) {
      val o2: Consumes = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Consumes = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Consumes] = pp.postConsumes(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformConnection(ctx: Context, o: Connection): Result[Context, Connection] = {
    val preR: PreResult[Context, Connection] = pp.preConnection(ctx, o)
    val r: Result[Context, Connection] = if (preR.continu) {
      val o2: Connection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, IS[Z, ConnectionEnd]] = transformISZ(ctx, o2.from_ends, transformConnectionEnd _)
      val r1: Result[Context, IS[Z, ConnectionEnd]] = transformISZ(r0.ctx, o2.to_ends, transformConnectionEnd _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
        Result(r1.ctx, Some(o2(from_ends = r0.resultOpt.getOrElse(o2.from_ends), to_ends = r1.resultOpt.getOrElse(o2.to_ends))))
      else
        Result(r1.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Connection = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Connection] = pp.postConnection(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformConnectionEnd(ctx: Context, o: ConnectionEnd): Result[Context, ConnectionEnd] = {
    val preR: PreResult[Context, ConnectionEnd] = pp.preConnectionEnd(ctx, o)
    val r: Result[Context, ConnectionEnd] = if (preR.continu) {
      val o2: ConnectionEnd = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: ConnectionEnd = r.resultOpt.getOrElse(o)
    val postR: Result[Context, ConnectionEnd] = pp.postConnectionEnd(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformConnector(ctx: Context, o: Connector): Result[Context, Connector] = {
    val preR: PreResult[Context, Connector] = pp.preConnector(ctx, o)
    val r: Result[Context, Connector] = if (preR.continu) {
      val o2: Connector = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Connector = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Connector] = pp.postConnector(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformProcedure(ctx: Context, o: Procedure): Result[Context, Procedure] = {
    val preR: PreResult[Context, Procedure] = pp.preProcedure(ctx, o)
    val r: Result[Context, Procedure] = if (preR.continu) {
      val o2: Procedure = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, IS[Z, Method]] = transformISZ(ctx, o2.methods, transformMethod _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        Result(r0.ctx, Some(o2(methods = r0.resultOpt.getOrElse(o2.methods))))
      else
        Result(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Procedure = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Procedure] = pp.postProcedure(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformMethod(ctx: Context, o: Method): Result[Context, Method] = {
    val preR: PreResult[Context, Method] = pp.preMethod(ctx, o)
    val r: Result[Context, Method] = if (preR.continu) {
      val o2: Method = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: Result[Context, IS[Z, Parameter]] = transformISZ(ctx, o2.parameters, transformParameter _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        Result(r0.ctx, Some(o2(parameters = r0.resultOpt.getOrElse(o2.parameters))))
      else
        Result(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Method = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Method] = pp.postMethod(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformParameter(ctx: Context, o: Parameter): Result[Context, Parameter] = {
    val preR: PreResult[Context, Parameter] = pp.preParameter(ctx, o)
    val r: Result[Context, Parameter] = if (preR.continu) {
      val o2: Parameter = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Parameter = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Parameter] = pp.postParameter(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformBinarySemaphore(ctx: Context, o: BinarySemaphore): Result[Context, BinarySemaphore] = {
    val preR: PreResult[Context, BinarySemaphore] = pp.preBinarySemaphore(ctx, o)
    val r: Result[Context, BinarySemaphore] = if (preR.continu) {
      val o2: BinarySemaphore = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: BinarySemaphore = r.resultOpt.getOrElse(o)
    val postR: Result[Context, BinarySemaphore] = pp.postBinarySemaphore(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformSemaphore(ctx: Context, o: Semaphore): Result[Context, Semaphore] = {
    val preR: PreResult[Context, Semaphore] = pp.preSemaphore(ctx, o)
    val r: Result[Context, Semaphore] = if (preR.continu) {
      val o2: Semaphore = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Semaphore = r.resultOpt.getOrElse(o)
    val postR: Result[Context, Semaphore] = pp.postSemaphore(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

  @pure def transformTODO(ctx: Context, o: TODO): Result[Context, TODO] = {
    val preR: PreResult[Context, TODO] = pp.preTODO(ctx, o)
    val r: Result[Context, TODO] = if (preR.continu) {
      val o2: TODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        Result(ctx, Some(o2))
      else
        Result(ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      Result(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      Result(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: TODO = r.resultOpt.getOrElse(o)
    val postR: Result[Context, TODO] = pp.postTODO(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return Result(postR.ctx, Some(o2))
    } else {
      return Result(postR.ctx, None())
    }
  }

}