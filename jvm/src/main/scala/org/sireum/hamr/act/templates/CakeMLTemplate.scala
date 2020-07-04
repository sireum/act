// #Sireum
package org.sireum.hamr.act.templates

import org.sireum._

object CakeMLTemplate {

  val GLOBAL_VAR__ENTRY_POINTS: String = "entryPoints"
  val GLOBAL_VAR__THIS: String = "this"
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

  def cmakeAddDefinitions(): ISZ[ST] = {
    val ret: ISZ[ST] = ISZ(
      CMakeTemplate.cmake_add_option(
        PREPROCESSOR_CAKEML_DUMP_BUFFERS,
        "Print the contents of byte-arrays being sent to CakeML",
        F),
      CMakeTemplate.cmake_add_option(
        PREPROCESSOR_CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS,
        "Print warning messages if byte-arrays being sent to CakeML are larger than expected",
        F),
      CMakeTemplate.cmake_add_option(
        PREPROCESSOR_CAKEML_ASSEMBLIES_PRESENT,
        "Enable if CakeML assembly files have been included",
        F)
    )
    return ret
  }

  def entryPointGlobalVar(typeName: String, cEntryPointAdapterQualifiedName: String): ST = {
    return st"${typeName} ${GLOBAL_VAR__ENTRY_POINTS};"
  }

  def thisGlobalVar(typeName: String): ST = {
    return st"${typeName} this;"
  }

  def initializedGlobalVar(): ST = {
    return st"bool ${GLOBAL_VAR__INITIALIZED} = false;"
  }

  def portIdsGlobalVars(): ISZ[ST] = {
    return ISZ(
      st"IS_82ABD8 ${GLOBAL_VAR__EVENT_IN_PORT_IDS};",
      st"IS_82ABD8 ${GLOBAL_VAR__DATA_IN_PORT_IDS};",
      st"IS_82ABD8 ${GLOBAL_VAR__EVENT_OUT_PORT_IDS};",
      st"IS_82ABD8 ${GLOBAL_VAR__DATA_OUT_PORT_IDS};")
  }

  def initializePortIds(entryPoints: String): ST = {
    val ret: ST = st"""${GLOBAL_VAR__EVENT_IN_PORT_IDS} = (IS_82ABD8) ${entryPoints}_eventInPortIds_(${GLOBAL_VAR__ENTRY_POINTS});
                      |${GLOBAL_VAR__DATA_IN_PORT_IDS} = (IS_82ABD8) ${entryPoints}_dataInPortIds_(${GLOBAL_VAR__ENTRY_POINTS});
                      |${GLOBAL_VAR__EVENT_OUT_PORT_IDS} = (IS_82ABD8) ${entryPoints}_eventOutPortIds_(${GLOBAL_VAR__ENTRY_POINTS});
                      |${GLOBAL_VAR__DATA_OUT_PORT_IDS} = (IS_82ABD8) ${entryPoints}_dataOutPortIds_(${GLOBAL_VAR__ENTRY_POINTS});"""
    return ret
  }

  def ffi_initializeEntryPoints(typeName: String, cEntryPointAdapterQualifiedName: String): ST = {
    return st"${GLOBAL_VAR__ENTRY_POINTS} = (${typeName}) ${cEntryPointAdapterQualifiedName}_entryPoints();"
  }

  def ffi_initializeThis(cComponentType: String): ST = {
    return st"${GLOBAL_VAR__THIS} = (${cComponentType}) &entryPoints->component;"
  }

  def initMethod(statements: ISZ[ST]): ST = {
    val ret: ST = st"""void init() {
                      |  if(!initialized) {
                      |    ${(statements, "\n")}
                      |    initialized = true;
                      |  }
                      |}"""
    return ret
  }

  def ffi_initialization(statements: ISZ[ST]): ST = {
    val ret: ST = st"""void ffiinitializeComponent(${defaultArgs}) {
                      |  ${(statements, "\n")}
                      |}"""
    return ret
  }

  def ffi_artReceiveInput(entryPoints: String): ST = {
    val ret: ST = st"""void ffiapi_receiveInput(${defaultArgs2}) {
                      |  init();
                      |  art_Art_receiveInput(SF ${GLOBAL_VAR__EVENT_IN_PORT_IDS}, ${GLOBAL_VAR__DATA_IN_PORT_IDS});
                      |}"""
    return ret
  }

  def ffi_artSendOutput(entryPoints: String): ST = {
    val ret: ST = st"""void ffiapi_sendOutput(${defaultArgs2}) {
                      |  init();
                      |  art_Art_sendOutput(SF ${GLOBAL_VAR__EVENT_OUT_PORT_IDS}, ${GLOBAL_VAR__DATA_OUT_PORT_IDS});
                      |}"""
    return ret
  }

  def logSignature(bridgeApi: String, loggerName: String, cThisApi: String): ST = {
    return st"${bridgeApi}_log${loggerName}_(${cThisApi}(${GLOBAL_VAR__THIS}), str)"
  }

  def ffi_artLoggers(bridgeApi: String, cThisApi: String): ISZ[ST] = {
    val ret: ISZ[ST] = ISZ("Info", "Debug", "Error").map(name => {
      st"""void ffiapi_log${name}(${defaultArgs2}){
          |  init();
          |  DeclNewString(_str);
          |  String str = (String)&_str;
          |  str->size = parameterSizeBytes;
          |  memcpy(str->value, parameter, parameterSizeBytes);
          |
          |  ${logSignature(bridgeApi, name, cThisApi)};
          |} """
    })
    return ret
  }

  def ffi_getterMethodName(port: String): String = { return s"ffiapi_get_${port}_in" }

  def ffi_setterMethodName(port: String): String = { return s"ffiapi_send_${port}_out" }

  def ffi_get(ffiMethodName: String, slangMethodName: String): ST = {
    val ret: ST = st"""void ${ffiMethodName}(${defaultArgs2}) {
                      |  init();
                      |  size_t numBits = 0;
                      |  output[0] = ${slangMethodName}(this, &numBits, (U8 *)(output + 1));
                      |  checkAndReportBufferOverrun(numBits / 8, (outputSizeBytes-1));
                      |  dumpBuffer(numBits, output);
                      |}"""
    return ret
  }

  def ffi_send(ffiMethodName: String, slangMethodName: String, isDataPort: B): ST = {
    val args: String = if(isDataPort) ", parameterSizeBytes*8, (U8 *)parameter" else ""
    val ret: ST = st"""void ${ffiMethodName}(${defaultArgs2}) {
                      |  init();
                      |  ${slangMethodName}(this${args});
                      |}"""
    return ret
  }

  def checkAndReportBufferOverrun(bridgeApi: String, cThisApi: String): ST = {
    val ret: ST = st"""void checkAndReportBufferOverrun(long bytesWritten, long bufferSizeBytes) {
                      |  #ifdef ${PREPROCESSOR_CAKEML_CHECK_AND_REPORT_BUFFER_OVERRUNS}
                      |  init();
                      |  if (bytesWritten > bufferSizeBytes) {
                      |    DeclNewString(_str);
                      |    String str = (String)&_str;
                      |    String__append(str, string("Wrote too many bytes to buffer"));
                      |    ${logSignature(bridgeApi, "Info", cThisApi)};
                      |  }
                      |  #endif
                      |}"""
    return ret
  }

  def dumpBuffer(bridgeApi: String, cThisApi: String): ST = {
    val ret: ST = st"""void dumpBuffer(size_t numBits, U8* buffer) {
                      |  #ifdef ${PREPROCESSOR_CAKEML_DUMP_BUFFERS}
                      |  init();
                      |  DeclNewString(_str);
                      |  String str = (String)&_str;
                      |  String__append(str, string("["));
                      |  size_t end = ((numBits / 8) > 80) ? 80 : (numBits / 8);
                      |  for (int i = 0 ; i < end ; ++i) {
                      |    U8_string_(str, buffer[i]);
                      |  }
                      |  String__append(str, string("]"));
                      |  ${logSignature(bridgeApi, "Info", cThisApi)};
                      |  #endif
                      |}"""
    return ret
  }

  def genMethodSignature(methodName: String,
                         returnType: String,
                         parameters: ISZ[(String, String)]): ST = {
    val params = parameters.map(m => st"${m._1} ${m._2}")
    val ret: ST = st"""${returnType} ${methodName}(${(params, ",\n")})"""
    return ret
  }

  def externMethod(methodName: String,
                   returnType: String,
                   parameters: ISZ[(String, String)]): ST = {

    val methodSignature = genMethodSignature(methodName, returnType, parameters)

    return st"extern ${methodSignature}"
  }

  def postlude(): ST = {
    val ret: ST = st"""void ffisb_pacer_notification_wait(${defaultArgs2}) {
                      |  sb_pacer_notification_wait();
                      |  output[0] = 1;
                      |}
                      |
                      |/**
                      | * Required by the FFI framework
                      | */
                      |
                      |void ffiwrite (${defaultArgs2}){
                      |}
                      |
                      |void cml_exit(int arg) {
                      |  #ifdef DEBUG_FFI
                      |  {
                      |    fprintf(stderr,"GCNum: %d, GCTime(us): %ld\n",numGC,microsecs);
                      |  }
                      |  #endif
                      |  exit(arg);
                      |}"""
    return ret
  }
}
