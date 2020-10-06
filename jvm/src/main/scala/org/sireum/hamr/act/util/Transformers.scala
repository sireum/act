// #Sireum
package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, Component, FeatureEnd, Transformer}

object Transformers {

  @datatype class UnboundedIntegerRewriter extends ir.Transformer.PrePost[B] {
    val unboundInt: String = "Base_Types::Integer"
    val int32: String = "Base_Types::Integer_32"

    override def postClassifier(ctx: B, o: ir.Classifier): ir.Transformer.TPostResult[B, ir.Classifier] = {
      if(o.name == unboundInt) {
        Util.reporter.warn(None(), Util.toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(T, Some(ir.Classifier(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }

    override def postClassifierProp(ctx: B, o: ir.ClassifierProp): ir.Transformer.TPostResult[B, ir.PropertyValue] = {
      if(o.name == unboundInt) {
        Util.reporter.warn(None(), Util.toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(T, Some(ir.ClassifierProp(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }
  }



  @datatype class CTX(requiresMissingType: B,
                      hasErrors: B)

  @datatype class MissingTypeRewriter extends ir.Transformer.PrePost[CTX] {

    val missingType: ir.Component = ir.Component(
      identifier = ir.Name(ISZ(), None()),
      category = ir.ComponentCategory.Data,
      classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)),
      features = ISZ(),
      subComponents = ISZ(),
      connections = ISZ(),
      connectionInstances = ISZ(),
      properties = ISZ(),
      flows = ISZ(),
      modes = ISZ(),
      annexes = ISZ(),
      uriFrag = ""
    )

    val missingArrayBaseType: ir.Property = ir.Property(
      name = ir.Name(ISZ(OsateProperties.DATA_MODEL__BASE_TYPE), None()),
      propertyValues = ISZ(ir.ClassifierProp(Util.MISSING_AADL_TYPE)),
      appliesTo = ISZ())

    val sporadicProp: ir.Property = ir.Property(
      name = ir.Name(ISZ(OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL), None()),
      propertyValues = ISZ(ir.ValueProp("Sporadic")),
      appliesTo = ISZ())


    override def postAadl(ctx: CTX, o: Aadl): Transformer.TPostResult[CTX, Aadl] = {
      if(ctx.requiresMissingType) {
        ir.Transformer.TPostResult(ctx, Some(o(dataComponents = o.dataComponents :+ missingType)))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.Aadl]())
      }
    }

    override def postComponent(ctx: CTX, o: Component): Transformer.TPostResult[CTX, Component] = {

      o.category match {
        case ir.ComponentCategory.Data =>
          if(o.classifier.isEmpty) {
            Util.reporter.warn(None(), Util.toolName, s"Classifier not specified for ${CommonUtil.getName(o.identifier)}.  Substituting ${Util.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)))))
          } else if (TypeUtil.isArrayDef(o) && TypeUtil.getArrayBaseType(o).isEmpty) {
            Util.reporter.warn(None(), Util.toolName, s"Base type not specified for ${o.classifier.get.name}.  Substituting ${Util.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(properties = o.properties :+ missingArrayBaseType)))
          } else {
            ir.Transformer.TPostResult(ctx, None[ir.Component]())
          }

        case ir.ComponentCategory.Thread =>
          PropertyUtil.getDiscreetPropertyValue(o.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL) match {
            case Some(ir.ValueProp(x)) =>
              if(x != "Periodic" && x != "Sporadic") {
                Util.reporter.error(None(), Util.toolName, s"${o.classifier.get.name} has unsupported dispatch protocol ${x}.")

                ir.Transformer.TPostResult(ctx(hasErrors = T), None[ir.Component]())
              } else {
                ir.Transformer.TPostResult(ctx, None[ir.Component]())
              }
            case _ =>
              Util.reporter.warn(None(), Util.toolName, s"${OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL} not specified for thread ${o.classifier.get.name}.  Treating it as sporadic.")

              ir.Transformer.TPostResult(ctx, Some(o(properties =  o.properties :+ sporadicProp)))
          }
        case _ => ir.Transformer.TPostResult(ctx, None[ir.Component]())
      }
    }

    override def postFeatureEnd(ctx: CTX, o: FeatureEnd): Transformer.TPostResult[CTX, FeatureEnd] = {
      if (CommonUtil.isDataPort(o) && o.classifier.isEmpty) {
        Util.reporter.warn(None(), Util.toolName, s"No datatype specified for data port ${CommonUtil.getName(o.identifier)}.  Substituting ${Util.MISSING_AADL_TYPE} ")

        ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)))))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.FeatureEnd]())
      }
    }
  }
}
