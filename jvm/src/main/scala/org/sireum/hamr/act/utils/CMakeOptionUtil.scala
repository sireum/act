// #Sireum

package org.sireum.hamr.act.utils

import org.sireum._

@sig trait CMakeOption {
  def name: String

  def defaultValue: B

  def description: String
}

@datatype class CMakeStandardOption (val name: String,
                                     val defaultValue: B,
                                     val description: String) extends CMakeOption

@datatype class CMakePreprocessorOption(val name: String,
                                        preprocessorName: String,
                                        val defaultValue: B,
                                        val description: String) extends CMakeOption
