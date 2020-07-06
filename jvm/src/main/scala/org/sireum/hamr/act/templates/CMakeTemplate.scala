// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.{StringTemplate, Util}
import org.sireum.hamr.act.vm.VMGen
import org.sireum.hamr.codegen.common.StringUtil

object CMakeTemplate {

  val CMAKE_VERSION: String = "3.8.2"

  val CMAKE_MINIMUM_REQUIRED_VERSION : String = s"cmake_minimum_required(VERSION ${CMAKE_VERSION})"

  val CMAKE_SET_CMAKE_C_STANDARD: String = "set(CMAKE_C_STANDARD 99)"

  def addLibrary(target: String, isTargetInterface: B, filenames: ISZ[String]): ST = {
    val INTERFACE: Option[String] = if(isTargetInterface) Some("INTERFACE") else None()

    val fopt: Option[ST] =
      if(filenames.isEmpty) None()
      else Some(st"${(filenames, "\n")}")

    val ret: ST =
      st"""add_library(${target}
          |            ${INTERFACE}
          |            ${fopt})"""

    return ret
  }

  def target_link_libraries(target: String, isTargetInterface: B, filenames: ISZ[String]): ST = {
    val INTERFACE: Option[String] = if(isTargetInterface) Some("INTERFACE") else None()
    val fopt: Option[ST] =
      if(filenames.isEmpty) None()
      else Some(st"${(filenames, "\n")}")

    val ret: ST = st"""target_link_libraries(${target}
                      |                      ${INTERFACE}
                      |                      ${fopt})"""
    return ret
  }

  def target_include_directories(target: String, isTargetInterface: B, paths: ISZ[String]): ST = {
    val INTERFACE: Option[String] = if(isTargetInterface) Some("INTERFACE") else None()
    val fopt: Option[ST] =
      if(paths.isEmpty) { None() }
      else {
        val public: String = if(isTargetInterface) "" else "PUBLIC "
        Some(st"${(paths.map((m: String) => st"${public}${m}"), "\n")}") }

    val ret: ST = st"""target_include_directories(${target}
                      |                           ${INTERFACE}
                      |                           ${fopt})"""
    return ret
  }


  val AUX_C_SOURCES: String = "AUX_C_SOURCES"
  val AUX_C_INCLUDES: String = "AUX_C_INCLUDES"

  def cmakeHamrIncludesName(instanceName: String): String = {
    return s"${Util.HAMR_INCLUDES_NAME}_${instanceName}"
  }

  def cmakeHamrIncludes(instanceName: String, hamrIncludeDirs: ISZ[String]): ST = {
    val includesName = cmakeHamrIncludesName(instanceName)
    return st"""set(${includesName}
               |  ${(hamrIncludeDirs, "\n")}
               |)"""
  }

  def cmakeAuxSources(auxCSources: ISZ[String], auxHDirectories: ISZ[String]): ST = {
    return st"""set(${AUX_C_SOURCES} ${(auxCSources, " ")})
               |set(${AUX_C_INCLUDES} ${(auxHDirectories, " ")})"""
  }

  def cmakeHamrLibName(instanceName: String): String = {
    return s"${Util.HAMR_LIB_NAME}_${instanceName}"
  }

  def cmakeHamrLib(instanceName: String,
                   hamrStaticLib: String): ST = {
    val libName = cmakeHamrLibName(instanceName)
    return st"set(${libName} ${hamrStaticLib})"
  }

  def cmake_DeclareCamkesComponent(componentName: String,
                                   sources: ISZ[String],
                                   includes: ISZ[String],
                                   libs: ISZ[String],
                                   hasAux: B,
                                   slangLib: Option[String]): ST = {
    var srcs: ISZ[ST] = ISZ()
    if(hasAux) { srcs = srcs :+ st"$${${AUX_C_SOURCES}} " }
    if(sources.nonEmpty) { srcs = srcs :+ st"""${(sources, " ")}""" }

    val _includes: Option[ST] = {
      var incls: ISZ[String] = ISZ()
      if (hasAux) {
        incls = incls :+ st"$${${AUX_C_INCLUDES}}".render
      }
      incls = incls ++ includes

      if(incls.nonEmpty) Some(st"INCLUDES ${(incls, " ")}") else None()
    }

    val _libs: Option[ST] = {
      var candidates: ISZ[String] = libs
      if(slangLib.nonEmpty) {
        candidates = candidates :+ slangLib.get
      }

      if(candidates.nonEmpty) {
        Some(st"LIBS ${(candidates, " ")}")
      } else {
        None()
      }
    }

    val ret: ST = st"""DeclareCAmkESComponent(${componentName}
                      |  SOURCES $srcs
                      |  ${_includes}
                      |  ${_libs}
                      |)"""

    return ret
  }


  def cmake_generateTypeCmakeLists(filenames: ISZ[String]): ST = {
    val includes: ISZ[String] = ISZ("includes")

    val filtered = Set.empty[String] ++ filenames // remove duplicates
    val isInterfaceTypeLib = filtered.isEmpty

    val ret: ST = st"""${StringTemplate.doNotEditCmakeComment()}
                      |
                      |${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
                      |
                      |project(${Util.SBTypeLibrary})
                      |
                      |${CMakeTemplate.CMAKE_SET_CMAKE_C_STANDARD}
                      |
                      |add_compile_options(-Werror)
                      |
                      |${CMakeTemplate.addStackUsageOption()}
                      |
                      |${CMakeTemplate.addLibrary(Util.SBTypeLibrary, isInterfaceTypeLib, filtered.elements)}
                      |
                      |# Assume that if the muslc target exists then this project is in an seL4 native
                      |# component build environment, otherwise it is in a linux userlevel environment.
                      |# In the linux userlevel environment, the C library will be linked automatically.
                      |if(TARGET muslc)
                      |  ${CMakeTemplate.target_link_libraries(Util.SBTypeLibrary, isInterfaceTypeLib, ISZ("muslc"))}
                      |endif()
                      |
                      |add_definitions(-DCAMKES)
                      |
                      |if(TARGET ${Util.SlangTypeLibrary})
                      |  ${CMakeTemplate.target_link_libraries(Util.SBTypeLibrary, isInterfaceTypeLib, ISZ(Util.SlangTypeLibrary))}
                      |endif()
                      |
                      |${CMakeTemplate.target_include_directories(Util.SBTypeLibrary, isInterfaceTypeLib, includes)}
                      |"""
    return ret
  }

  def cmakeLists(rootServer: String,
                 entries: ISZ[ST],
                 definitions: ISZ[ST]): ST = {

    return st"""${StringTemplate.doNotEditCmakeComment()}
               |
               |${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
               |
               |project (${rootServer} C)
               |
               |${(definitions, "\n\n")}
               |
               |${CMakeTemplate.addStackUsageOption()}
               |
               |${(entries, "\n\n")}
               |
               |DeclareCAmkESRootserver(${rootServer}.camkes)
               |"""
  }

  def genSettingsCmake(settingsCmakeEntries: ISZ[ST]): ST = {
    val ret: ST = st"""${StringTemplate.safeToEditCamkeComment()}
                      |
                      |${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
                      |
                      |${(settingsCmakeEntries, "\n")}"""
    return ret
  }

  def cmake_add_subdirectory_binned(path: String, binDir: Option[String]): ST = {
    val bin: ST = if(binDir.nonEmpty) st" ${binDir.get}"else st""
    return st"add_subdirectory(${path}${bin})"
  }

  def cmake_add_subdirectory(path: String): ST = {
    return cmake_add_subdirectory_binned(path, None())
  }

  def cmake_addSubDir_TypeLibrary(): ST = {
    return cmake_add_subdirectory(s"$${CMAKE_CURRENT_LIST_DIR}/${Util.getTypeRootPath()}")
  }

  def cmake_addSubDir_VM(): ST = {
    return cmake_add_subdirectory(s"$${CMAKE_CURRENT_LIST_DIR}/${VMGen.getRootVMDir()}")
  }

  def cmake_add_definitions(defs: ISZ[String]): ST = {
    val _defs = defs.map((m: String) => s"-D${m}")
    return st"add_definitions(${(_defs, "\n")})"
  }

  def cmake_add_option(name: String, description: String, defaultValue: B): ST = {
    val _default: String = if(defaultValue) "ON" else "OFF"
    val ret: ST = st"""option(${name}
                      |       "${description}"
                      |       ${_default})
                      |
                      |if(${name} OR "$$ENV{${name}}" STREQUAL "ON")
                      |   add_definitions(-D${name})
                      |endif()"""
    return ret
  }

  def addStackUsageOption(): ST = {
    return st"""if ("$${CMAKE_CXX_COMPILER_ID}" MATCHES "(C|c?)lang")
               |  add_compile_options("$$<$$<CONFIG:Release>:-Oz>")
               |elseif ("$${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
               |  add_compile_options(-fstack-usage)
               |  add_compile_options("$$<$$<CONFIG:Release>:-Os>")
               |endif()"""
  }
}
