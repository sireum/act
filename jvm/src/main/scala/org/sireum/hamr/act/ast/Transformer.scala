// #Sireum
// @formatter:off

/*
 Copyright (c) 2020, Robby, Kansas State University
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

object Transformer {

  @datatype class PreResult[Context, T](ctx: Context,
                                        continu: B,
                                        resultOpt: Option[T])

  @datatype class TPostResult[Context, T](ctx: Context,
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
        case o: LibraryComponent =>
          val r: PreResult[Context, ASTObject] = preLibraryComponent(ctx, o) match {
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
        case o: Mutex =>
          val r: PreResult[Context, ASTObject] = preMutex(ctx, o) match {
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

    @pure def preCamkesComponent(ctx: Context, o: CamkesComponent): PreResult[Context, CamkesComponent] = {
      o match {
        case o: Component =>
          val r: PreResult[Context, CamkesComponent] = preComponent(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: CamkesComponent)) => PreResult(preCtx, continu, Some[CamkesComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type CamkesComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[CamkesComponent]())
          }
          return r
        case o: LibraryComponent =>
          val r: PreResult[Context, CamkesComponent] = preLibraryComponent(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: CamkesComponent)) => PreResult(preCtx, continu, Some[CamkesComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type CamkesComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[CamkesComponent]())
          }
          return r
      }
    }

    @pure def preComponent(ctx: Context, o: Component): PreResult[Context, Component] = {
      return PreResult(ctx, T, None())
    }

    @pure def preLibraryComponent(ctx: Context, o: LibraryComponent): PreResult[Context, LibraryComponent] = {
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

    @pure def preDataport(ctx: Context, o: Dataport): PreResult[Context, Dataport] = {
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

    @pure def preMutex(ctx: Context, o: Mutex): PreResult[Context, Mutex] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAttribute(ctx: Context, o: Attribute): PreResult[Context, Attribute] = {
      return PreResult(ctx, T, None())
    }

    @pure def preTODO(ctx: Context, o: TODO): PreResult[Context, TODO] = {
      return PreResult(ctx, T, None())
    }

    @pure def postASTObject(ctx: Context, o: ASTObject): TPostResult[Context, ASTObject] = {
      o match {
        case o: Assembly =>
          val r: TPostResult[Context, ASTObject] = postAssembly(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Composition =>
          val r: TPostResult[Context, ASTObject] = postComposition(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Instance =>
          val r: TPostResult[Context, ASTObject] = postInstance(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Component =>
          val r: TPostResult[Context, ASTObject] = postComponent(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: LibraryComponent =>
          val r: TPostResult[Context, ASTObject] = postLibraryComponent(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Connection =>
          val r: TPostResult[Context, ASTObject] = postConnection(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: ConnectionEnd =>
          val r: TPostResult[Context, ASTObject] = postConnectionEnd(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Connector =>
          val r: TPostResult[Context, ASTObject] = postConnector(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Procedure =>
          val r: TPostResult[Context, ASTObject] = postProcedure(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Method =>
          val r: TPostResult[Context, ASTObject] = postMethod(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Parameter =>
          val r: TPostResult[Context, ASTObject] = postParameter(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: BinarySemaphore =>
          val r: TPostResult[Context, ASTObject] = postBinarySemaphore(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Semaphore =>
          val r: TPostResult[Context, ASTObject] = postSemaphore(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: Mutex =>
          val r: TPostResult[Context, ASTObject] = postMutex(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
        case o: TODO =>
          val r: TPostResult[Context, ASTObject] = postTODO(ctx, o) match {
           case TPostResult(postCtx, Some(result: ASTObject)) => TPostResult(postCtx, Some[ASTObject](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type ASTObject")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[ASTObject]())
          }
          return r
      }
    }

    @pure def postAssembly(ctx: Context, o: Assembly): TPostResult[Context, Assembly] = {
      return TPostResult(ctx, None())
    }

    @pure def postComposition(ctx: Context, o: Composition): TPostResult[Context, Composition] = {
      return TPostResult(ctx, None())
    }

    @pure def postInstance(ctx: Context, o: Instance): TPostResult[Context, Instance] = {
      return TPostResult(ctx, None())
    }

    @pure def postCamkesComponent(ctx: Context, o: CamkesComponent): TPostResult[Context, CamkesComponent] = {
      o match {
        case o: Component =>
          val r: TPostResult[Context, CamkesComponent] = postComponent(ctx, o) match {
           case TPostResult(postCtx, Some(result: CamkesComponent)) => TPostResult(postCtx, Some[CamkesComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type CamkesComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[CamkesComponent]())
          }
          return r
        case o: LibraryComponent =>
          val r: TPostResult[Context, CamkesComponent] = postLibraryComponent(ctx, o) match {
           case TPostResult(postCtx, Some(result: CamkesComponent)) => TPostResult(postCtx, Some[CamkesComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type CamkesComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[CamkesComponent]())
          }
          return r
      }
    }

    @pure def postComponent(ctx: Context, o: Component): TPostResult[Context, Component] = {
      return TPostResult(ctx, None())
    }

    @pure def postLibraryComponent(ctx: Context, o: LibraryComponent): TPostResult[Context, LibraryComponent] = {
      return TPostResult(ctx, None())
    }

    @pure def postUses(ctx: Context, o: Uses): TPostResult[Context, Uses] = {
      return TPostResult(ctx, None())
    }

    @pure def postProvides(ctx: Context, o: Provides): TPostResult[Context, Provides] = {
      return TPostResult(ctx, None())
    }

    @pure def postEmits(ctx: Context, o: Emits): TPostResult[Context, Emits] = {
      return TPostResult(ctx, None())
    }

    @pure def postConsumes(ctx: Context, o: Consumes): TPostResult[Context, Consumes] = {
      return TPostResult(ctx, None())
    }

    @pure def postDataport(ctx: Context, o: Dataport): TPostResult[Context, Dataport] = {
      return TPostResult(ctx, None())
    }

    @pure def postConnection(ctx: Context, o: Connection): TPostResult[Context, Connection] = {
      return TPostResult(ctx, None())
    }

    @pure def postConnectionEnd(ctx: Context, o: ConnectionEnd): TPostResult[Context, ConnectionEnd] = {
      return TPostResult(ctx, None())
    }

    @pure def postConnector(ctx: Context, o: Connector): TPostResult[Context, Connector] = {
      return TPostResult(ctx, None())
    }

    @pure def postProcedure(ctx: Context, o: Procedure): TPostResult[Context, Procedure] = {
      return TPostResult(ctx, None())
    }

    @pure def postMethod(ctx: Context, o: Method): TPostResult[Context, Method] = {
      return TPostResult(ctx, None())
    }

    @pure def postParameter(ctx: Context, o: Parameter): TPostResult[Context, Parameter] = {
      return TPostResult(ctx, None())
    }

    @pure def postBinarySemaphore(ctx: Context, o: BinarySemaphore): TPostResult[Context, BinarySemaphore] = {
      return TPostResult(ctx, None())
    }

    @pure def postSemaphore(ctx: Context, o: Semaphore): TPostResult[Context, Semaphore] = {
      return TPostResult(ctx, None())
    }

    @pure def postMutex(ctx: Context, o: Mutex): TPostResult[Context, Mutex] = {
      return TPostResult(ctx, None())
    }

    @pure def postAttribute(ctx: Context, o: Attribute): TPostResult[Context, Attribute] = {
      return TPostResult(ctx, None())
    }

    @pure def postTODO(ctx: Context, o: TODO): TPostResult[Context, TODO] = {
      return TPostResult(ctx, None())
    }

  }

  @pure def transformISZ[Context, T](ctx: Context, s: IS[Z, T], f: (Context, T) => TPostResult[Context, T] @pure): TPostResult[Context, IS[Z, T]] = {
    val s2: MS[Z, T] = s.toMS
    var changed: B = F
    var ctxi = ctx
    for (i <- s2.indices) {
      val e: T = s(i)
      val r: TPostResult[Context, T] = f(ctxi, e)
      ctxi = r.ctx
      changed = changed || r.resultOpt.nonEmpty
      s2(i) = r.resultOpt.getOrElse(e)
    }
    if (changed) {
      return TPostResult(ctxi, Some(s2.toIS))
    } else {
      return TPostResult[Context, IS[Z, T]](ctxi, None[IS[Z, T]]())
    }
  }

}

import Transformer._

@datatype class Transformer[Context](pp: PrePost[Context]) {

  @pure def transformASTObject(ctx: Context, o: ASTObject): TPostResult[Context, ASTObject] = {
    val preR: PreResult[Context, ASTObject] = pp.preASTObject(ctx, o)
    val r: TPostResult[Context, ASTObject] = if (preR.continu) {
      val o2: ASTObject = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, ASTObject] = o2 match {
        case o2: Assembly =>
          val r0: TPostResult[Context, Composition] = transformComposition(preR.ctx, o2.composition)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(composition = r0.resultOpt.getOrElse(o2.composition))))
          else
            TPostResult(r0.ctx, None())
        case o2: Composition =>
          val r0: TPostResult[Context, IS[Z, TODO]] = transformISZ(preR.ctx, o2.groups, transformTODO _)
          val r1: TPostResult[Context, IS[Z, TODO]] = transformISZ(r0.ctx, o2.exports, transformTODO _)
          val r2: TPostResult[Context, IS[Z, Instance]] = transformISZ(r1.ctx, o2.instances, transformInstance _)
          val r3: TPostResult[Context, IS[Z, Connection]] = transformISZ(r2.ctx, o2.connections, transformConnection _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
            TPostResult(r3.ctx, Some(o2(groups = r0.resultOpt.getOrElse(o2.groups), exports = r1.resultOpt.getOrElse(o2.exports), instances = r2.resultOpt.getOrElse(o2.instances), connections = r3.resultOpt.getOrElse(o2.connections))))
          else
            TPostResult(r3.ctx, None())
        case o2: Instance =>
          val r0: TPostResult[Context, CamkesComponent] = transformCamkesComponent(preR.ctx, o2.component)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(component = r0.resultOpt.getOrElse(o2.component))))
          else
            TPostResult(r0.ctx, None())
        case o2: Component =>
          val r0: TPostResult[Context, IS[Z, Mutex]] = transformISZ(preR.ctx, o2.mutexes, transformMutex _)
          val r1: TPostResult[Context, IS[Z, BinarySemaphore]] = transformISZ(r0.ctx, o2.binarySemaphores, transformBinarySemaphore _)
          val r2: TPostResult[Context, IS[Z, Semaphore]] = transformISZ(r1.ctx, o2.semaphores, transformSemaphore _)
          val r3: TPostResult[Context, IS[Z, Dataport]] = transformISZ(r2.ctx, o2.dataports, transformDataport _)
          val r4: TPostResult[Context, IS[Z, Emits]] = transformISZ(r3.ctx, o2.emits, transformEmits _)
          val r5: TPostResult[Context, IS[Z, Uses]] = transformISZ(r4.ctx, o2.uses, transformUses _)
          val r6: TPostResult[Context, IS[Z, Consumes]] = transformISZ(r5.ctx, o2.consumes, transformConsumes _)
          val r7: TPostResult[Context, IS[Z, Provides]] = transformISZ(r6.ctx, o2.provides, transformProvides _)
          val r8: TPostResult[Context, IS[Z, TODO]] = transformISZ(r7.ctx, o2.attributes, transformTODO _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty || r4.resultOpt.nonEmpty || r5.resultOpt.nonEmpty || r6.resultOpt.nonEmpty || r7.resultOpt.nonEmpty || r8.resultOpt.nonEmpty)
            TPostResult(r8.ctx, Some(o2(mutexes = r0.resultOpt.getOrElse(o2.mutexes), binarySemaphores = r1.resultOpt.getOrElse(o2.binarySemaphores), semaphores = r2.resultOpt.getOrElse(o2.semaphores), dataports = r3.resultOpt.getOrElse(o2.dataports), emits = r4.resultOpt.getOrElse(o2.emits), uses = r5.resultOpt.getOrElse(o2.uses), consumes = r6.resultOpt.getOrElse(o2.consumes), provides = r7.resultOpt.getOrElse(o2.provides), attributes = r8.resultOpt.getOrElse(o2.attributes))))
          else
            TPostResult(r8.ctx, None())
        case o2: LibraryComponent =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: Connection =>
          val r0: TPostResult[Context, IS[Z, ConnectionEnd]] = transformISZ(preR.ctx, o2.from_ends, transformConnectionEnd _)
          val r1: TPostResult[Context, IS[Z, ConnectionEnd]] = transformISZ(r0.ctx, o2.to_ends, transformConnectionEnd _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(from_ends = r0.resultOpt.getOrElse(o2.from_ends), to_ends = r1.resultOpt.getOrElse(o2.to_ends))))
          else
            TPostResult(r1.ctx, None())
        case o2: ConnectionEnd =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: Connector =>
          val r0: TPostResult[Context, IS[Z, Attribute]] = transformISZ(preR.ctx, o2.attributes, transformAttribute _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(attributes = r0.resultOpt.getOrElse(o2.attributes))))
          else
            TPostResult(r0.ctx, None())
        case o2: Procedure =>
          val r0: TPostResult[Context, IS[Z, Method]] = transformISZ(preR.ctx, o2.methods, transformMethod _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(methods = r0.resultOpt.getOrElse(o2.methods))))
          else
            TPostResult(r0.ctx, None())
        case o2: Method =>
          val r0: TPostResult[Context, IS[Z, Parameter]] = transformISZ(preR.ctx, o2.parameters, transformParameter _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(parameters = r0.resultOpt.getOrElse(o2.parameters))))
          else
            TPostResult(r0.ctx, None())
        case o2: Parameter =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: BinarySemaphore =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: Semaphore =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: Mutex =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: TODO =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: ASTObject = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, ASTObject] = pp.postASTObject(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAssembly(ctx: Context, o: Assembly): TPostResult[Context, Assembly] = {
    val preR: PreResult[Context, Assembly] = pp.preAssembly(ctx, o)
    val r: TPostResult[Context, Assembly] = if (preR.continu) {
      val o2: Assembly = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, Composition] = transformComposition(preR.ctx, o2.composition)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(composition = r0.resultOpt.getOrElse(o2.composition))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Assembly = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Assembly] = pp.postAssembly(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformComposition(ctx: Context, o: Composition): TPostResult[Context, Composition] = {
    val preR: PreResult[Context, Composition] = pp.preComposition(ctx, o)
    val r: TPostResult[Context, Composition] = if (preR.continu) {
      val o2: Composition = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, TODO]] = transformISZ(preR.ctx, o2.groups, transformTODO _)
      val r1: TPostResult[Context, IS[Z, TODO]] = transformISZ(r0.ctx, o2.exports, transformTODO _)
      val r2: TPostResult[Context, IS[Z, Instance]] = transformISZ(r1.ctx, o2.instances, transformInstance _)
      val r3: TPostResult[Context, IS[Z, Connection]] = transformISZ(r2.ctx, o2.connections, transformConnection _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
        TPostResult(r3.ctx, Some(o2(groups = r0.resultOpt.getOrElse(o2.groups), exports = r1.resultOpt.getOrElse(o2.exports), instances = r2.resultOpt.getOrElse(o2.instances), connections = r3.resultOpt.getOrElse(o2.connections))))
      else
        TPostResult(r3.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Composition = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Composition] = pp.postComposition(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformInstance(ctx: Context, o: Instance): TPostResult[Context, Instance] = {
    val preR: PreResult[Context, Instance] = pp.preInstance(ctx, o)
    val r: TPostResult[Context, Instance] = if (preR.continu) {
      val o2: Instance = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, CamkesComponent] = transformCamkesComponent(preR.ctx, o2.component)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(component = r0.resultOpt.getOrElse(o2.component))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Instance = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Instance] = pp.postInstance(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformCamkesComponent(ctx: Context, o: CamkesComponent): TPostResult[Context, CamkesComponent] = {
    val preR: PreResult[Context, CamkesComponent] = pp.preCamkesComponent(ctx, o)
    val r: TPostResult[Context, CamkesComponent] = if (preR.continu) {
      val o2: CamkesComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, CamkesComponent] = o2 match {
        case o2: Component =>
          val r0: TPostResult[Context, IS[Z, Mutex]] = transformISZ(preR.ctx, o2.mutexes, transformMutex _)
          val r1: TPostResult[Context, IS[Z, BinarySemaphore]] = transformISZ(r0.ctx, o2.binarySemaphores, transformBinarySemaphore _)
          val r2: TPostResult[Context, IS[Z, Semaphore]] = transformISZ(r1.ctx, o2.semaphores, transformSemaphore _)
          val r3: TPostResult[Context, IS[Z, Dataport]] = transformISZ(r2.ctx, o2.dataports, transformDataport _)
          val r4: TPostResult[Context, IS[Z, Emits]] = transformISZ(r3.ctx, o2.emits, transformEmits _)
          val r5: TPostResult[Context, IS[Z, Uses]] = transformISZ(r4.ctx, o2.uses, transformUses _)
          val r6: TPostResult[Context, IS[Z, Consumes]] = transformISZ(r5.ctx, o2.consumes, transformConsumes _)
          val r7: TPostResult[Context, IS[Z, Provides]] = transformISZ(r6.ctx, o2.provides, transformProvides _)
          val r8: TPostResult[Context, IS[Z, TODO]] = transformISZ(r7.ctx, o2.attributes, transformTODO _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty || r4.resultOpt.nonEmpty || r5.resultOpt.nonEmpty || r6.resultOpt.nonEmpty || r7.resultOpt.nonEmpty || r8.resultOpt.nonEmpty)
            TPostResult(r8.ctx, Some(o2(mutexes = r0.resultOpt.getOrElse(o2.mutexes), binarySemaphores = r1.resultOpt.getOrElse(o2.binarySemaphores), semaphores = r2.resultOpt.getOrElse(o2.semaphores), dataports = r3.resultOpt.getOrElse(o2.dataports), emits = r4.resultOpt.getOrElse(o2.emits), uses = r5.resultOpt.getOrElse(o2.uses), consumes = r6.resultOpt.getOrElse(o2.consumes), provides = r7.resultOpt.getOrElse(o2.provides), attributes = r8.resultOpt.getOrElse(o2.attributes))))
          else
            TPostResult(r8.ctx, None())
        case o2: LibraryComponent =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: CamkesComponent = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, CamkesComponent] = pp.postCamkesComponent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformComponent(ctx: Context, o: Component): TPostResult[Context, Component] = {
    val preR: PreResult[Context, Component] = pp.preComponent(ctx, o)
    val r: TPostResult[Context, Component] = if (preR.continu) {
      val o2: Component = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, Mutex]] = transformISZ(preR.ctx, o2.mutexes, transformMutex _)
      val r1: TPostResult[Context, IS[Z, BinarySemaphore]] = transformISZ(r0.ctx, o2.binarySemaphores, transformBinarySemaphore _)
      val r2: TPostResult[Context, IS[Z, Semaphore]] = transformISZ(r1.ctx, o2.semaphores, transformSemaphore _)
      val r3: TPostResult[Context, IS[Z, Dataport]] = transformISZ(r2.ctx, o2.dataports, transformDataport _)
      val r4: TPostResult[Context, IS[Z, Emits]] = transformISZ(r3.ctx, o2.emits, transformEmits _)
      val r5: TPostResult[Context, IS[Z, Uses]] = transformISZ(r4.ctx, o2.uses, transformUses _)
      val r6: TPostResult[Context, IS[Z, Consumes]] = transformISZ(r5.ctx, o2.consumes, transformConsumes _)
      val r7: TPostResult[Context, IS[Z, Provides]] = transformISZ(r6.ctx, o2.provides, transformProvides _)
      val r8: TPostResult[Context, IS[Z, TODO]] = transformISZ(r7.ctx, o2.attributes, transformTODO _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty || r4.resultOpt.nonEmpty || r5.resultOpt.nonEmpty || r6.resultOpt.nonEmpty || r7.resultOpt.nonEmpty || r8.resultOpt.nonEmpty)
        TPostResult(r8.ctx, Some(o2(mutexes = r0.resultOpt.getOrElse(o2.mutexes), binarySemaphores = r1.resultOpt.getOrElse(o2.binarySemaphores), semaphores = r2.resultOpt.getOrElse(o2.semaphores), dataports = r3.resultOpt.getOrElse(o2.dataports), emits = r4.resultOpt.getOrElse(o2.emits), uses = r5.resultOpt.getOrElse(o2.uses), consumes = r6.resultOpt.getOrElse(o2.consumes), provides = r7.resultOpt.getOrElse(o2.provides), attributes = r8.resultOpt.getOrElse(o2.attributes))))
      else
        TPostResult(r8.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Component = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Component] = pp.postComponent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformLibraryComponent(ctx: Context, o: LibraryComponent): TPostResult[Context, LibraryComponent] = {
    val preR: PreResult[Context, LibraryComponent] = pp.preLibraryComponent(ctx, o)
    val r: TPostResult[Context, LibraryComponent] = if (preR.continu) {
      val o2: LibraryComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: LibraryComponent = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, LibraryComponent] = pp.postLibraryComponent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformUses(ctx: Context, o: Uses): TPostResult[Context, Uses] = {
    val preR: PreResult[Context, Uses] = pp.preUses(ctx, o)
    val r: TPostResult[Context, Uses] = if (preR.continu) {
      val o2: Uses = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Uses = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Uses] = pp.postUses(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformProvides(ctx: Context, o: Provides): TPostResult[Context, Provides] = {
    val preR: PreResult[Context, Provides] = pp.preProvides(ctx, o)
    val r: TPostResult[Context, Provides] = if (preR.continu) {
      val o2: Provides = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Provides = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Provides] = pp.postProvides(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformEmits(ctx: Context, o: Emits): TPostResult[Context, Emits] = {
    val preR: PreResult[Context, Emits] = pp.preEmits(ctx, o)
    val r: TPostResult[Context, Emits] = if (preR.continu) {
      val o2: Emits = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Emits = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Emits] = pp.postEmits(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformConsumes(ctx: Context, o: Consumes): TPostResult[Context, Consumes] = {
    val preR: PreResult[Context, Consumes] = pp.preConsumes(ctx, o)
    val r: TPostResult[Context, Consumes] = if (preR.continu) {
      val o2: Consumes = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Consumes = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Consumes] = pp.postConsumes(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformDataport(ctx: Context, o: Dataport): TPostResult[Context, Dataport] = {
    val preR: PreResult[Context, Dataport] = pp.preDataport(ctx, o)
    val r: TPostResult[Context, Dataport] = if (preR.continu) {
      val o2: Dataport = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Dataport = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Dataport] = pp.postDataport(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformConnection(ctx: Context, o: Connection): TPostResult[Context, Connection] = {
    val preR: PreResult[Context, Connection] = pp.preConnection(ctx, o)
    val r: TPostResult[Context, Connection] = if (preR.continu) {
      val o2: Connection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, ConnectionEnd]] = transformISZ(preR.ctx, o2.from_ends, transformConnectionEnd _)
      val r1: TPostResult[Context, IS[Z, ConnectionEnd]] = transformISZ(r0.ctx, o2.to_ends, transformConnectionEnd _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
        TPostResult(r1.ctx, Some(o2(from_ends = r0.resultOpt.getOrElse(o2.from_ends), to_ends = r1.resultOpt.getOrElse(o2.to_ends))))
      else
        TPostResult(r1.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Connection = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Connection] = pp.postConnection(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformConnectionEnd(ctx: Context, o: ConnectionEnd): TPostResult[Context, ConnectionEnd] = {
    val preR: PreResult[Context, ConnectionEnd] = pp.preConnectionEnd(ctx, o)
    val r: TPostResult[Context, ConnectionEnd] = if (preR.continu) {
      val o2: ConnectionEnd = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: ConnectionEnd = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, ConnectionEnd] = pp.postConnectionEnd(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformConnector(ctx: Context, o: Connector): TPostResult[Context, Connector] = {
    val preR: PreResult[Context, Connector] = pp.preConnector(ctx, o)
    val r: TPostResult[Context, Connector] = if (preR.continu) {
      val o2: Connector = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, Attribute]] = transformISZ(preR.ctx, o2.attributes, transformAttribute _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(attributes = r0.resultOpt.getOrElse(o2.attributes))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Connector = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Connector] = pp.postConnector(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformProcedure(ctx: Context, o: Procedure): TPostResult[Context, Procedure] = {
    val preR: PreResult[Context, Procedure] = pp.preProcedure(ctx, o)
    val r: TPostResult[Context, Procedure] = if (preR.continu) {
      val o2: Procedure = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, Method]] = transformISZ(preR.ctx, o2.methods, transformMethod _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(methods = r0.resultOpt.getOrElse(o2.methods))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Procedure = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Procedure] = pp.postProcedure(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformMethod(ctx: Context, o: Method): TPostResult[Context, Method] = {
    val preR: PreResult[Context, Method] = pp.preMethod(ctx, o)
    val r: TPostResult[Context, Method] = if (preR.continu) {
      val o2: Method = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, Parameter]] = transformISZ(preR.ctx, o2.parameters, transformParameter _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(parameters = r0.resultOpt.getOrElse(o2.parameters))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Method = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Method] = pp.postMethod(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformParameter(ctx: Context, o: Parameter): TPostResult[Context, Parameter] = {
    val preR: PreResult[Context, Parameter] = pp.preParameter(ctx, o)
    val r: TPostResult[Context, Parameter] = if (preR.continu) {
      val o2: Parameter = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Parameter = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Parameter] = pp.postParameter(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformBinarySemaphore(ctx: Context, o: BinarySemaphore): TPostResult[Context, BinarySemaphore] = {
    val preR: PreResult[Context, BinarySemaphore] = pp.preBinarySemaphore(ctx, o)
    val r: TPostResult[Context, BinarySemaphore] = if (preR.continu) {
      val o2: BinarySemaphore = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: BinarySemaphore = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, BinarySemaphore] = pp.postBinarySemaphore(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformSemaphore(ctx: Context, o: Semaphore): TPostResult[Context, Semaphore] = {
    val preR: PreResult[Context, Semaphore] = pp.preSemaphore(ctx, o)
    val r: TPostResult[Context, Semaphore] = if (preR.continu) {
      val o2: Semaphore = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Semaphore = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Semaphore] = pp.postSemaphore(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformMutex(ctx: Context, o: Mutex): TPostResult[Context, Mutex] = {
    val preR: PreResult[Context, Mutex] = pp.preMutex(ctx, o)
    val r: TPostResult[Context, Mutex] = if (preR.continu) {
      val o2: Mutex = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Mutex = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Mutex] = pp.postMutex(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAttribute(ctx: Context, o: Attribute): TPostResult[Context, Attribute] = {
    val preR: PreResult[Context, Attribute] = pp.preAttribute(ctx, o)
    val r: TPostResult[Context, Attribute] = if (preR.continu) {
      val o2: Attribute = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Attribute = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Attribute] = pp.postAttribute(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformTODO(ctx: Context, o: TODO): TPostResult[Context, TODO] = {
    val preR: PreResult[Context, TODO] = pp.preTODO(ctx, o)
    val r: TPostResult[Context, TODO] = if (preR.continu) {
      val o2: TODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: TODO = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, TODO] = pp.postTODO(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

}