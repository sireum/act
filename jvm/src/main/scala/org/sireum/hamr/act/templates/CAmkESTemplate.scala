// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._

object CAmkESTemplate {

  def externGetInstanceName(): ST = {
    return st"extern const char *get_instance_name(void);"
  }
}
