// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.Util
import org.sireum.hamr.act.vm.VMGen

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
        Some(st"${(paths.map(m => st"${public}${m}"), "\n")}") }

    val ret: ST = st"""target_include_directories(${target}
                      |                           ${INTERFACE}
                      |                           ${fopt})"""
    return ret
  }
  def addSubDir_TypeLibrary(): ST = {
    return st"add_subdirectory($${CMAKE_CURRENT_LIST_DIR}/${Util.getTypeRootPath()})"
  }

  def addSubDir_VM(): ST = {
    return st"add_subdirectory($${CMAKE_CURRENT_LIST_DIR}/${VMGen.getRootVMDir()})"
  }
}
