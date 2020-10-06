// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.util.{CMakeOption, CMakePreprocessorOption}
import org.sireum.hamr.codegen.common.templates.StackFrameTemplate
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.ir

object SlangEmbeddedTemplate {

  val TRANSPILER_OPTIONS: ISZ[CMakeOption] = ISZ(
    CMakePreprocessorOption("BOUND_CHECK", "SIREUM_BOUND_CHECK", F, "Build the program with sequence bound checking."),
    CMakePreprocessorOption("NO_PRINT", "SIREUM_NO_PRINT", F,"Build the program without console output."),
    CMakePreprocessorOption("RANGE_CHECK", "SIREUM_RANGE_CHECK", F, "Build the program with range checking."),
    CMakePreprocessorOption("WITH_LOC", "SIREUM_LOC", F, "Build the program with Slang location info.")
  )

  def hamrGetInstanceName(basePackageName: String, c: ir.Component): ST = {
    return st"${basePackageName}_${CommonUtil.getName(c.identifier)}"
  }

  def hamrIntialiseArchitecture(appName: String): ST = {
    return st"""// initialise slang-embedded components/ports
               |${appName}_initialiseArchitecture(${StackFrameTemplate.SF_LAST});"""
  }

  def hamrInitialiseEntrypoint(appName: String): ST = {
    return st"""// call the component's initialise entrypoint
               |${appName}_initialiseEntryPoint(${StackFrameTemplate.SF_LAST});"""
  }

  def hamrRunLoopEntries(appName: String): ISZ[ST] = {
    return ISZ(st"""// call the component's compute entrypoint
                   |${appName}_computeEntryPoint(${StackFrameTemplate.SF_LAST});""")
  }

  def hamrSlangType(c : ir.Classifier, base: String) : String = {
    val r = StringUtil.replaceAll(StringUtil.replaceAll(c.name, "::", "_"), ".", "_")
    return s"${base}_${r}"
  }

  def hamrSlangPayloadType(c : ir.Classifier, base: String) : String = {
    val r = StringUtil.replaceAll(StringUtil.replaceAll(c.name, "::", "_"), ".", "_")
    return s"${base}_${r}_Payload"
  }

  def hamrIsEmptyUnconnected(methodName: String,
                             sel4IsEmptyMethodName: String): ST = {
    val ret: ST = st"""// FIXME: dummy implementation for unconnected incoming port
                      |B ${methodName}(${StackFrameTemplate.STACK_FRAME_ONLY}) {
                      |  return T;
                      |}"""
    return ret
  }

  def hamrIsEmpty(methodName: String,
                  sel4IsEmptyMethodName: String,
                  srcPort: ir.FeatureEnd): ST = {
    val ret: ST = st"""// is_empty ${CommonUtil.getLastName(srcPort.identifier)}: ${srcPort.direction.name} ${srcPort.category.name}
                      |B ${methodName}(${StackFrameTemplate.STACK_FRAME_ONLY}) {
                      |  return ${sel4IsEmptyMethodName}();
                      |}"""
    return ret
  }

  def hamrReceiveUnconnectedIncomingEventPort(methodName: String, declNewStackFrame: ST): ST = {
    val ret: ST = st"""// FIXME: dummy implementation for unconnected incoming port
                      |Unit ${methodName}(
                      |  ${StackFrameTemplate.STACK_FRAME}
                      |  Option_8E9F45 result) {
                      |  ${declNewStackFrame};
                      |
                      |  // FIXME: dummy implementation
                      |
                      |  // put None in result
                      |  DeclNewNone_964667(none);
                      |  Type_assign(result, &none, sizeof(union Option_8E9F45));
                      |}"""
    return ret
  }

  def hamrReceiveIncomingEventPort(comment: ST,
                                   methodName: String,
                                   sel4ReadMethod: String,
                                   declNewStackFrame: ST): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(STACK_FRAME
                      |  Option_8E9F45 result) {
                      |  ${declNewStackFrame};
                      |
                      |  if(${sel4ReadMethod}()) {
                      |    // event port - ART requires an Empty payload be sent
                      |    DeclNewart_Empty(payload);
                      |
                      |    // wrap it in Some and place in result
                      |    DeclNewSome_D29615(some);
                      |    Some_D29615_apply(${StackFrameTemplate.SF} &some, (art_DataContent) &payload);
                      |    Type_assign(result, &some, sizeof(union Option_8E9F45));
                      |  } else {
                      |    // put None in result
                      |    DeclNewNone_964667(none);
                      |    Type_assign(result, &none, sizeof(union Option_8E9F45));
                      |  }
                      |} """
    return ret
  }

  def hamrReceiveIncomingDataPort(comment: ST,
                                  methodName: String,
                                  sel4Type: String,
                                  slangPayloadType: String,
                                  sel4ReadMethod: String,
                                  declNewStackFrame: ST): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(
                      |  ${StackFrameTemplate.STACK_FRAME}
                      |  Option_8E9F45 result) {
                      |  ${declNewStackFrame};
                      |
                      |  ${sel4Type} val;
                      |  if(${sel4ReadMethod}((${sel4Type} *) &val)) {
                      |    // wrap payload in Some and place in result
                      |    DeclNewSome_D29615(some);
                      |    Some_D29615_apply(${StackFrameTemplate.SF} &some, (art_DataContent) &val);
                      |    Type_assign(result, &some, sizeof(union Option_8E9F45));
                      |  } else {
                      |    // put None in result
                      |    DeclNewNone_964667(none);
                      |    Type_assign(result, &none, sizeof(union Option_8E9F45));
                      |  }
                      |}
                      |"""
    return ret
  }

  def hamrSendUnconnectedOutgoingDataPort(methodName: String,
                                          declNewStackFrame: ST): ST = {
    val ret: ST = st"""// FIXME: dummy implementation for unconnected outgoing port
                      |Unit ${methodName}(
                      |  ${StackFrameTemplate.STACK_FRAME}
                      |  art_DataContent d) {
                      |  ${declNewStackFrame};
                      |  // FIXME: dummy implementation
                      |}"""
    return ret
  }

  def hamrSendOutgoingDataPort(comment: ST,
                               methodName: String,
                               sel4Type: String,
                               slangPayloadType: String,
                               srcEnqueue: String,
                               declNewStackFrame: ST): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(
                      |  ${StackFrameTemplate.STACK_FRAME}
                      |  art_DataContent d) {
                      |  ${declNewStackFrame};
                      |
                      |  ${srcEnqueue}(d);
                      |}"""
    return ret
  }

  def hamrSendOutgoingEventPort(comment: ST,
                                methodName: String,
                                srcEnqueue: String,
                                declNewStackFrame: ST): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(
                      |  ${StackFrameTemplate.STACK_FRAME}
                      |  art_DataContent d) {
                      |  ${declNewStackFrame};
                      |
                      |  // event port - can ignore the Slang Empty payload
                      |  art_Empty payload = (art_Empty) d;
                      |
                      |  // send event via CAmkES
                      |  ${srcEnqueue}();
                      |}"""
    return ret
  }

}
