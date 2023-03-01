// #Sireum
package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.util.{CMakeOption, CMakePreprocessorOption}
import org.sireum.hamr.codegen.common.NixSeL4NameUtil
import org.sireum.hamr.codegen.common.templates._
import org.sireum.hamr.ir.FeatureCategory

object CakeMLTemplate {

  val GLOBAL_VAR__ENTRY_POINTS: String = "entryPoints"
  val GLOBAL_VAR__INITIALIZED: String = "initialized"
  val GLOBAL_VAR__EVENT_IN_PORT_IDS: String = "event_in_port_ids"
  val GLOBAL_VAR__DATA_IN_PORT_IDS: String = "data_in_port_ids"
  val GLOBAL_VAR__EVENT_OUT_PORT_IDS: String = "event_out_port_ids"
  val GLOBAL_VAR__DATA_OUT_PORT_IDS: String = "data_out_port_ids"

  val PREPROCESSOR_CAKEML_DUMP_BUFFERS: String = "CAKEML_DUMP_BUFFERS"
  val PREPROCESSOR_CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS: String = "CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS"
  val PREPROCESSOR_CAKEML_ASSEMBLIES_PRESENT: String = "CAKEML_ASSEMBLIES_PRESENT"

  val defaultArgs: String = "unsigned char *parameter, long parameterSizeBytes"
  val defaultArgs2: String = s"${defaultArgs}, unsigned char *output, long outputSizeBytes"

  val METHOD_NAME_CHECK_AND_REPORT_BUFFER_OVERRUN: String = "checkAndReportBufferOverrun"
  val METHOD_NAME_DUMP_BUFFER: String = "dumpBuffer"

  val CAKEML_OPTIONS: ISZ[CMakeOption] = ISZ(
    CMakePreprocessorOption(PREPROCESSOR_CAKEML_DUMP_BUFFERS, PREPROCESSOR_CAKEML_DUMP_BUFFERS,
      F, "Print the contents of byte-arrays being sent to CakeML"),
    CMakePreprocessorOption(PREPROCESSOR_CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS, PREPROCESSOR_CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS,
      F, "Print warning messages if byte-arrays being sent to CakeML are larger than expected"),
    CMakePreprocessorOption(PREPROCESSOR_CAKEML_ASSEMBLIES_PRESENT, PREPROCESSOR_CAKEML_ASSEMBLIES_PRESENT,
      F, "Enable if CakeML assembly files have been included")
  )

  def entryPointGlobalVar(typeName: String, cEntryPointAdapterQualifiedName: String): ST = {
    return st"${typeName} ${GLOBAL_VAR__ENTRY_POINTS};"
  }

  def initializedGlobalVar(): ST = {
    return st"bool ${GLOBAL_VAR__INITIALIZED} = false;"
  }

  def portIdsGlobalVars(): ISZ[ST] = {
    return ISZ(
      st"IS_D10119 ${GLOBAL_VAR__EVENT_IN_PORT_IDS};",
      st"IS_D10119 ${GLOBAL_VAR__DATA_IN_PORT_IDS};",
      st"IS_D10119 ${GLOBAL_VAR__EVENT_OUT_PORT_IDS};",
      st"IS_D10119 ${GLOBAL_VAR__DATA_OUT_PORT_IDS};")
  }

  def initializePortIds(entryPoints: String): ST = {
    val ret: ST =
      st"""${GLOBAL_VAR__EVENT_IN_PORT_IDS} = (IS_D10119) ${entryPoints}_eventInPortIds_(${GLOBAL_VAR__ENTRY_POINTS});
          |${GLOBAL_VAR__DATA_IN_PORT_IDS} = (IS_D10119) ${entryPoints}_dataInPortIds_(${GLOBAL_VAR__ENTRY_POINTS});
          |${GLOBAL_VAR__EVENT_OUT_PORT_IDS} = (IS_D10119) ${entryPoints}_eventOutPortIds_(${GLOBAL_VAR__ENTRY_POINTS});
          |${GLOBAL_VAR__DATA_OUT_PORT_IDS} = (IS_D10119) ${entryPoints}_dataOutPortIds_(${GLOBAL_VAR__ENTRY_POINTS});"""
    return ret
  }

  def ffi_initializeEntryPoints(typeName: String, cEntryPointAdapterQualifiedName: String): ST = {
    return st"${GLOBAL_VAR__ENTRY_POINTS} = (${typeName}) ${cEntryPointAdapterQualifiedName}_entryPoints(${StackFrameTemplate.SF_LAST});"
  }

  def initMethod(statements: ISZ[ST], fileUri: String): ST = {
    val methodName = "init"
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(T, fileUri, "", methodName, 0)

    val ret: ST =
      st"""void ${methodName}(${StackFrameTemplate.STACK_FRAME_ONLY}) {
          |  if(!initialized) {
          |    ${declNewStackFrame};
          |
          |    ${(statements, "\n")}
          |    initialized = true;
          |  }
          |}"""
    return ret
  }

  def ffi_initialization(statements: ISZ[ST], fileUri: String): ST = {
    val methodName = "ffiinitializeComponent"
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)

    val ret: ST =
      st"""void ${methodName}(${defaultArgs}) {
          |  ${declNewStackFrame};
          |
          |  ${(statements, "\n")}
          |}"""
    return ret
  }

  def ffi_artReceiveInput(entryPoints: String, fileUri: String): ST = {
    val methodName = "ffiapi_receiveInput"
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)

    val ret: ST =
      st"""void ${methodName}(${defaultArgs2}) {
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  art_Art_receiveInput(SF ${GLOBAL_VAR__EVENT_IN_PORT_IDS}, ${GLOBAL_VAR__DATA_IN_PORT_IDS});
          |}"""
    return ret
  }

  def ffi_artSendOutput(entryPoints: String, fileUri: String): ST = {
    val methodName = "ffiapi_sendOutput"
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)

    val ret: ST =
      st"""void ${methodName}(${defaultArgs2}) {
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  art_Art_sendOutput(SF ${GLOBAL_VAR__EVENT_OUT_PORT_IDS}, ${GLOBAL_VAR__DATA_OUT_PORT_IDS});
          |}"""
    return ret
  }

  def logSignature(bridgeApi: String): ST = {
    return st"${bridgeApi}(${StackFrameTemplate.SF} str)"
  }

  def ffi_artLoggers(componentType: String, fileUri: String): ISZ[ST] = {
    val ret: ISZ[ST] = ISZ("Info", "Debug", "Error").map(name => {

      val methodName = s"log${name}"
      val ffiMethodName = s"ffiapi_${methodName}"
      val apiHelper = NixSeL4NameUtil.apiHelperLoggerMethodName(methodName, componentType)

      val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", ffiMethodName, 0)

      st"""void ${ffiMethodName}(${defaultArgs2}){
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  DeclNewString(_str);
          |  String str = (String)&_str;
          |  str->size = parameterSizeBytes;
          |  memcpy(str->value, parameter, parameterSizeBytes);
          |
          |  ${logSignature(apiHelper)};
          |} """
    })
    return ret
  }

  def ffi_getterMethodName(port: String): String = {
    return s"ffiapi_get_${port}"
  }

  def ffi_setterMethodName(port: String): String = {
    return s"ffiapi_send_${port}"
  }

  def ffi_get(ffiMethodName: String, slangMethodName: String, fileUri: String, featureType: FeatureCategory.Type): ST = {
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", ffiMethodName, 0)

    val actions: ST = featureType match {
      case FeatureCategory.DataPort =>
        st"""size_t numBits = 0;
            |${slangMethodName}(${StackFrameTemplate.SF} &numBits, (U8 *)(output));
            |${METHOD_NAME_CHECK_AND_REPORT_BUFFER_OVERRUN}(${StackFrameTemplate.SF} numBits / 8, (outputSizeBytes));
            |${METHOD_NAME_DUMP_BUFFER}(${StackFrameTemplate.SF} numBits, output);"""
      case FeatureCategory.EventPort =>
        st"output[0] = ${slangMethodName}(${StackFrameTemplate.SF_LAST})"
      case FeatureCategory.EventDataPort =>
        st"""size_t numBits = 0;
            |output[0] = ${slangMethodName}(${StackFrameTemplate.SF} &numBits, (U8 *)(output + 1));
            |${METHOD_NAME_CHECK_AND_REPORT_BUFFER_OVERRUN}(${StackFrameTemplate.SF} numBits / 8, (outputSizeBytes - 1));
            |${METHOD_NAME_DUMP_BUFFER}(${StackFrameTemplate.SF} numBits, output);"""
      case _ => halt(s"Unexpected feature type ${featureType}")
    }

    val ret: ST =
      st"""void ${ffiMethodName}(${defaultArgs2}) {
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  ${actions}
          |}"""
    return ret
  }

  def ffi_send(ffiMethodName: String, slangMethodName: String, isDataPort: B, fileUri: String): ST = {
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", ffiMethodName, 0)

    val args: String = if (isDataPort) "parameterSizeBytes*8, (U8 *)parameter" else ""
    val sf: String = if (isDataPort) StackFrameTemplate.SF else StackFrameTemplate.SF_LAST

    val ret: ST =
      st"""void ${ffiMethodName}(${defaultArgs2}) {
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  ${slangMethodName}(${sf} ${args});
          |}"""
    return ret
  }

  def checkAndReportBufferOverrun(logInfo: String, fileUri: String): ST = {
    val methodName = METHOD_NAME_CHECK_AND_REPORT_BUFFER_OVERRUN
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)

    val ret: ST =
      st"""void ${methodName}(${StackFrameTemplate.STACK_FRAME} long bytesWritten, long bufferSizeBytes) {
          |  #ifdef ${PREPROCESSOR_CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS}
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  if (bytesWritten > bufferSizeBytes) {
          |    DeclNewString(_str);
          |    String str = (String)&_str;
          |    String__append(${StackFrameTemplate.SF} str, string("Wrote too many bytes to buffer"));
          |    ${logSignature(logInfo)};
          |  }
          |  #endif
          |}"""
    return ret
  }

  def dumpBuffer(logInfo: String, fileUri: String): ST = {
    val methodName = METHOD_NAME_DUMP_BUFFER
    val declNewStackFrame = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)

    val ret: ST =
      st"""void dumpBuffer(${StackFrameTemplate.STACK_FRAME} size_t numBits, U8* buffer) {
          |  #ifdef ${PREPROCESSOR_CAKEML_DUMP_BUFFERS}
          |  ${declNewStackFrame};
          |
          |  ${callInit()}
          |  DeclNewString(_str);
          |  String str = (String)&_str;
          |  String__append(${StackFrameTemplate.SF} str, string("["));
          |  size_t end = ((numBits / 8) > 80) ? 80 : (numBits / 8);
          |  for (int i = 0 ; i < end ; ++i) {
          |    U8_string_(${StackFrameTemplate.SF} str, buffer[i]);
          |  }
          |  String__append(${StackFrameTemplate.SF} str, string("]"));
          |  ${logSignature(logInfo)};
          |  #endif
          |}"""
    return ret
  }

  def callInit(): ST = {
    return st"init(${StackFrameTemplate.SF_LAST});"
  }

  def genMethodSignature(methodName: String,
                         returnType: String,
                         parameters: ISZ[(String, String)]): ST = {
    val params = parameters.map((m: ((String, String))) => st"${m._1} ${m._2}")
    val ret: ST = st"""${returnType} ${methodName}(${(params, ",\n")})"""
    return ret
  }

  def externMethod(methodName: String,
                   returnType: String,
                   parameters: ISZ[(String, String)]): ST = {

    val methodSignature = genMethodSignature(methodName, returnType, parameters)

    return st"extern ${methodSignature}"
  }

  def postlude(selfPacing: B): ST = {
    val notifName: String =
      if (selfPacing) {
        "sb_self_pacer_tock_wait();"
      }
      else {
        "sb_pacer_notification_wait();"
      }

    val emitName: String =
      if (selfPacing) {
        "sb_self_pacer_tick_emit();"
      }
      else {
        "// non self-pacing so do nothing"
      }

    val ret: ST =
      st"""void ffisb_pacer_notification_wait(${defaultArgs2}) {
          |  ${notifName}
          |  output[0] = 1;
          |}
          |
          |void ffisb_pacer_notification_emit(${defaultArgs2}) {
          |  ${emitName}
          |  output[0] = 1;
          |}
          |
          |void ffiraw_print_buffer(unsigned char *parameter, long parameterSizeBytes, unsigned char *output, long outputSizeBytes) {
          |
          |  int elideAfter = 20; // stop printing after this many consecutive 0's seen
          |  int stopAfter = 50; // exit after this many consecutive 0's seen, lessens 'busy-wait'
          |
          |  U8* buffer = (U8 *) parameter;
          |
          |  printf("%i bytes\n", parameterSizeBytes); // doesn't include the bool byte
          |
          |  if(parameterSizeBytes > 0) {
          |    int numZerosSeen = 0;
          |    printf("[%02x\n", buffer[0]); // first byte is a bool
          |    for(int i = 1; i < parameterSizeBytes + 1; i++) {
          |      numZerosSeen = buffer[i] == 0 ? (numZerosSeen + 1) : 0;
          |      if(numZerosSeen >= elideAfter) {
          |        if(numZerosSeen % stopAfter == 0) { printf("..."); break; }
          |      } else {
          |      	printf("%02x ", buffer[i]);
          |      	if(i % 16 == 0) printf("\n");
          |      }
          |    }
          |    printf("]\n");
          |  }
          |}
          |"""
    return ret
  }

  def ffiTemplate(includes: ISZ[String],
                  globals: ISZ[ST],
                  methods: ISZ[ST]): ST = {
    val ret: ST =
      st"""${StringTemplate.doNotEditComment()}
          |
          |${(includes, "\n")}
          |
          |${(globals, "\n")}
          |
          |${(methods, "\n\n")}
          |"""
    return ret
  }

  def emptyAssemblyFile(): ST = {
    val ret: ST =
      st"""${StringTemplate.safeToEditComment()}
          |
          |// placeholder for CakeML assembly"""
    return ret
  }
}
