// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._

object CAmkESTemplate {

  val DOMAIN_FIELD: String = "_domain"

  def externGetInstanceName(): ST = {
    return st"extern const char *get_instance_name(void);"
  }

  def domainConfiguration(identifier: String, domain: Z): ST = {
    return st"${identifier}.${DOMAIN_FIELD} = ${domain};"
  }
}
