// #Sireum
package org.sireum.hamr.act.templates

import org.sireum._

object AlloyTemplate {

  def als(base: ST,
          proof: ST): ST = {
    val ret: ST =
      st"""${base}
          |
          |${proof}"""

    return ret
  }

  def base(): ST = {
    val ret: ST =
      st"""//==============================================
          |//
          |//   A A D L    I n s t a n c e    M o d e l      R e p r e s e n t a t i o n
          |//
          |//   Alloy Schema
          |//
          |//   These are "framework" definitions that  apply for every system
          |//
          |//==============================================
          |
          |
          |
          |//-------------------
          |//  D a t a  T a g s
          |//-------------------
          |//
          |// port direction
          |
          |abstract sig Direction { }
          |one sig In, Out extends Direction {}
          |
          |// Note: Technically, I don't think modeling the port direction is needed, since
          |//  directionality is implied by the connection modeling, but including prevents
          |//  the generation of unrealizable system when asking Alloy to generate example
          |//  instances
          |
          |//------------------------------
          |//  A A D L   C o m p o n e n t  /  P o r t    D a t a    S t r u c t u r e s
          |//------------------------------
          |
          |sig AADLComponent {
          |// Note: It's natural to have a component have a set attribute indicating
          |//  its  ports, but the visualization is more messy.  So instead, model each
          |//  port with a component attribute indicating the component to which it belongs.
          |//
          |}
          |
          |sig AADLPort {
          | // each port belongs to one component
          | component: one AADLComponent,
          | // each component has a single direction indicator associated with it
          | direction : one Direction,
          |}
          |
          |//------------------------------
          |//  A A D L   C o n n e c t i o n s
          |//------------------------------
          |
          |one sig AADLConnections {
          | // many-to-many connection relation between ports
          | flowsTo: AADLPort -> AADLPort
          |}
          |
          |//------------------------------------------------
          |//  A A D L   W e l l - f o r m e d n e s s    P r e d i c a t e s
          |//------------------------------------------------
          |
          |pred AADLFlowDirectionality () {
          |  // each connections connects an output port to an inport port
          |  (AADLPort.(AADLConnections.flowsTo).direction = In) and
          |  (((AADLConnections.flowsTo).AADLPort).direction = Out)
          |}
          |
          |pred AADLFlowNoSelfConnection () {
          |  // no port is connected to itself
          |  all p: AADLPort | (p !in p.(AADLConnections.flowsTo))
          |}
          |
          |pred AADLWellFormedness () {
          |    AADLFlowDirectionality &&
          |    AADLFlowNoSelfConnection
          |}
          |
          |//  Note:
          |//    For auditing purposes, we might want to
          |//    have some property saying that each port is connected
          |//    unless is is explicitly marked as unconnected
          |
          |//==============================================
          |//
          |//   C A m k E S    S y s t e m     R e p r e s e n t a t i o n
          |//
          |//     These are "framework" definitions that  apply for every system
          |//
          |//==============================================
          |
          |
          |//------------------------------
          |//  C A m k E S   C o m p o n e n t  /  P o r t    D a t a    S t r u c t u r e s
          |//------------------------------
          |
          |
          |sig CAMKESComponent {
          |}
          |
          |sig CAMKESPort {
          | component: one CAMKESComponent,
          | direction : one Direction
          |}
          |
          |//------------------------------
          |//  CAMKES  C o n n e c t i o n s
          |//------------------------------
          |
          |one sig CAMKESConnections {
          | // connection relation
          | flowsTo: CAMKESPort -> CAMKESPort
          |}
          |
          |//----------------------------------------------------
          |//  C A M K E S    W e l l - f o r m e d n e s s    P r e d i c a t e s
          |//----------------------------------------------------
          |
          |pred CAMKESFlowDirectionality () {
          |  (CAMKESPort.(CAMKESConnections.flowsTo).direction = In) and
          |  (((CAMKESConnections.flowsTo).CAMKESPort).direction = Out)
          |}
          |
          |pred CAMKESFlowNoSelfConnection () {
          |  all p: CAMKESPort | (p !in p.(CAMKESConnections.flowsTo))
          |}
          |
          |pred CAMKESWellFormedness () {
          |    CAMKESFlowDirectionality &&
          |    CAMKESFlowNoSelfConnection
          |}
          |
          |
          |//====================================
          |//
          |//   R e f i n e m e n t      R e l a t i o n   and   P r o o f    C l a i m s
          |//
          |//       These are "framework" definitions that  apply for every system
          |//
          |//====================================
          |
          |one sig Refinement {
          |    component: AADLComponent -> CAMKESComponent,
          |    port: AADLPort -> CAMKESPort
          |}
          |
          |// Property: Every AADL connection has a corresponding CAMKES connection
          |pred ConnectionPreservation () {
          |    //  for each AADL port AADLsource
          |    //   Note: we could limit this to Out ports
          |    all AADLsource: AADLPort |
          |       // for each AADL port AADLdest that AADLsource is connected to
          |       all AADLdest: AADLsource.(AADLConnections.flowsTo) |
          |          // there exists a corresponding CAMKES source and destinition port such that
          |          some CAMKESsource, CAMKESdest: CAMKESPort |
          |              //  the AADLsource is translated to the CAMKESsource
          |             (AADLsource.(Refinement.port) = CAMKESsource  &&
          |              //  the AADLdest is translated to the CAMKESdest
          |              AADLdest.(Refinement.port) = CAMKESdest  &&
          |              //  the AADLsource's component is translated to the CAMKESsource's component
          |              AADLsource.component.(Refinement.component) = CAMKESsource.component &&
          |              //  the AADLdest's component is translated to the CAMKESdest's component
          |              AADLdest.component.(Refinement.component) = CAMKESdest.component &&
          |              // there is a flow between the CAMKESsource and CAMKESdest ports
          |              CAMKESdest in CAMKESsource.(CAMKESConnections.flowsTo)
          |              )
          |}
          |
          |// Property: Every CAMKES connection has a corresponding AADL connection
          |pred NoNewConnections () {
          |    //  for each CAMKES port CAMKESsource
          |    //   Note: we could limit this to Out ports
          |    all CAMKESsource: CAMKESPort |
          |       // for each CAMKES port CAMKESdest that CAMKESsource is connected to
          |       all CAMKESdest: CAMKESsource.(CAMKESConnections.flowsTo) |
          |          // there exists a corresponding AADL source and destinition port such that
          |          some AADLsource, AADLdest: AADLPort |
          |              //  the CAMKESsource was generated from an AADLsource
          |             (CAMKESsource.~(Refinement.port) = AADLsource  &&
          |              //  the CAMKESdest was generated from an AADLdest
          |             CAMKESdest.~(Refinement.port) = AADLdest  &&
          |              //  the CAMKESsource's component is generated from the AADLsource's component
          |             CAMKESsource.component.~(Refinement.component) = AADLsource.component &&
          |              //  the CAMKESdest's component is generated from the AADLdest's component
          |             CAMKESdest.component.~(Refinement.component) = AADLdest.component &&
          |              // there is a flow between the CAMKESsource and CAMKESdest ports
          |             AADLdest in AADLsource.(AADLConnections.flowsTo)
          |           )
          |}
          |
          |
          |//=================================================
          |//
          |//  D e b u g g i n g
          |//
          |//   Used to autogenerate various model instances for debugging
          |//   This can be removed in the deployed framework
          |//=================================================
          |
          |pred debugAADL () {
          |  AADLWellFormedness
          |  #AADLComponent >= 2
          |  #AADLPort >= 2
          |}
          |
          |run debugAADL for  4
          |
          |pred debugCAMKES () {
          |  CAMKESWellFormedness
          |  #CAMKESComponent >= 2
          |  #CAMKESPort >= 4
          |}
          |
          |run debugCAMKES for 4
          |"""
    return ret
  }

  def proof(aadlComponents: ISZ[String],
            aadlPorts: ISZ[String],
            aadlPortConstraints: ISZ[ST],
            aadlConnections: ISZ[ST],
            numAadlComponents: Z,
            numAadlPorts: Z,

            camkesComponents: ISZ[ST],
            camkesPorts: ISZ[ST],
            camkesPortConstraints: ISZ[ST],
            camkesConnections: ISZ[ST],
            numCamkesComponents: Z,
            numCamkesPorts: Z,

            componentRefinements: ISZ[ST],
            portRefinements: ISZ[ST]): ST = {
    val ret =
      st"""// open alloy_case_framework // TODO: this breaks themes
          |
          |// ---------------------------------------------
          |//
          |//    A A D L    I n s t a n c e
          |//
          |// ---------------------------------------------
          |
          |//  For better modularity create a separate abstract signature to hold elements
          |//  of the representation AADL instance model.   By declaring the signature abstract
          |//  below, we do not have a have a separate  (cumbersome) spec that says that the
          |//  only elements of AADLComponent are the elements (which would have to be
          |//  explicitly named) that come from the instance model.
          |//
          |//   This could be included in the boilerplate spec.  That is, it is the same for all
          |//   system instance models.
          |//
          |abstract sig AADLComponentInstance extends AADLComponent {}
          |abstract sig AADLPortInstance extends AADLPort {}
          |
          |//
          |// HAMR needs to generate a unique name for each AADL component
          |//  (e.g., based on fully qualified name) to go into the declaration below.
          |// The manually created system illustrated below has four components.
          |one sig
          |    ${(aadlComponents, ",\n")}
          | extends AADLComponentInstance {}
          |
          |//
          |// HAMR needs to generate a unique name for each AADL port.
          |// The manually created system illustrated below has four ports.
          |one sig
          |    ${(aadlPorts, ",\n")}
          |extends AADLPortInstance {}
          |
          |//
          |//  HAMR needs to generate the following constraints that indicate the
          |//  relationship between each port and its associated component
          |//  (effectively recreating the AADL model topology as Allow relations).
          |//  I have put the constraints in predicates so that I could more easily
          |//  enable and disable the constraints when debugging and show
          |//  how the  approach automated detects HAMR translation errors.
          |//
          |pred AADLInstanceComponentsPorts () {
          |   // State that there are no components and ports except those explicitly stated in the instance
          |   // This is boilerplate.  Its the same for all systems
          |   AADLComponent = AADLComponentInstance
          |   AADLPort = AADLPortInstance
          |
          |   // ACT generate constraints
          |   //   For each port, generate a constraints indicating...
          |   //    ... the component to which it belongs
          |   ${(aadlPortConstraints, "\n\n")}
          |}
          |
          |pred AADLInstanceConnections () {
          |   // I'm using Alloy's tuple syntax below for better readability
          |   AADLConnections.flowsTo =
          |     {
          |        ${(aadlConnections, " + \n")}
          |     }
          |}
          |
          |// ACT should generate the following predicate and run command
          |// that be used to verify that an AADL system instance can be generated by Alloy
          |
          |pred AADLInstance () {
          |    AADLWellFormedness
          |    AADLInstanceComponentsPorts
          |    AADLInstanceConnections
          |}
          |
          |run AADLInstance for 2 but
          |  // the exact scope used in the line above is not important since it is applying to the CAMKES model
          |  // which we are not concerned about in this debugging action
          |  ${numAadlComponents} AADLComponent,
          |  ${numAadlPorts} AADLPort
          |
          |//  **NOTE**: The bitwidths of the integers in the scoping above are set by Alloy.  I believe the default is
          |//   three bits so the scope can go up to 7 with the default bit with.  As the size of the instance model
          |//  increases, you will need to have ACT insert a declaration that appropriate sets the bitwidth for integers.
          |
          |
          |// ---------------------------------------------
          |//
          |//    C A M K E S    I n s t a n c e
          |//
          |// ---------------------------------------------
          |
          |//   This could be included in the boilerplate spec.  That is, it is the same for all
          |//   system instance models.
          |
          |abstract sig CAMKESComponentInstance extends CAMKESComponent {}
          |abstract sig CAMKESPortInstance extends CAMKESPort {}
          |
          |one sig
          |   ${(camkesComponents, ",\n")}
          | extends CAMKESComponentInstance {}
          |
          |one sig
          |   ${(camkesPorts, ",\n")}
          |extends CAMKESPortInstance {}
          |
          |
          |// HAMR needs to generate the following contraints that indicate the
          |//  relationship between each port and its associated component.
          |//  I have put the contraints in predicates so that I could more easily
          |//  enable and disable the constraints when debugging and show
          |//  how the  approach automated detects HAMR translation errors.
          |//
          |pred CAMKESInstanceComponentsPorts () {
          |   // State that there are no components and ports except those explicitly stated in the instance
          |   // This is boilerplate.  Its the same for all systems
          |   CAMKESComponent = CAMKESComponentInstance
          |   CAMKESPort = CAMKESPortInstance
          |
          |   ${(camkesPortConstraints, "\n\n")}
          |}
          |
          |pred CAMKESInstanceConnections () {
          |   // I'm using Alloy's tuple syntax below for better readability
          |   CAMKESConnections.flowsTo =
          |     {
          |        ${(camkesConnections, " + \n")}
          |     }
          |}
          |
          |// HAMR should generate the following predicate and run command
          |// that be used to verify that an AADL system instance can be generated by Alloy
          |
          |pred CAMKESInstance () {
          |    CAMKESWellFormedness
          |    CAMKESInstanceComponentsPorts
          |    CAMKESInstanceConnections
          |}
          |
          |run CAMKESInstance for 2 but
          |  // the exact scope used in the line above is not important since it is applying to the CAMKES model
          |  // which we are not concerned about in this debugging action
          |  ${numCamkesComponents} CAMKESComponent,
          |  ${numCamkesPorts} CAMKESPort
          |
          |
          |
          |//====================================================
          |//
          |//   R e f i n e m e n t      R e l a t i o n
          |//
          |//====================================================
          |
          |//
          |//   HAMR needs to generate the following to represent the tracebility between the
          |//  AADL system elements and the CAMKES system elements
          |
          |pred HAMRRefinement () {
          |   Refinement.component =
          |   {
          |      ${(componentRefinements, " + \n")}
          |   }
          |
          |   Refinement.port =
          |   {
          |      ${(portRefinements, " + \n")}
          |   }
          |}
          |
          |// HAMR should generate the following predicate and run command
          |// that be used to verify that an AADL and CAMKES system instances plus refinement
          |// mapping can be generated by Alloy
          |
          |pred HAMRTranslationScenario () {
          |    AADLWellFormedness
          |    AADLInstanceComponentsPorts
          |    AADLInstanceConnections
          |    CAMKESWellFormedness
          |    CAMKESInstanceComponentsPorts
          |    CAMKESInstanceConnections
          |    //
          |    HAMRRefinement
          |}
          |
          |run HAMRTranslationScenario for
          |  ${numAadlComponents} AADLComponent,
          |  ${numAadlPorts} AADLPort,
          |  ${numCamkesComponents} CAMKESComponent,
          |  ${numCamkesPorts} CAMKESPort
          |
          |
          |//====================================================
          |//
          |//   R e f i n e m e n t      V e r i f i c a t i o n
          |//
          |//====================================================
          |
          |assert RefinementVerification {
          | HAMRTranslationScenario
          | implies
          | (ConnectionPreservation &&
          |  NoNewConnections)
          |}
          |
          |check RefinementVerification for
          |  ${numAadlComponents} AADLComponent,
          |  ${numAadlPorts} AADLPort,
          |  ${numCamkesComponents} CAMKESComponent,
          |  ${numCamkesPorts} CAMKESPort
        """
    return ret
  }

  def theme(): ST = {
    val ret: ST =
      st"""<?xml version="1.0"?>
          |<alloy>
          |
          |<view>
          |
          |<projection> <type name="AADLConnections"/> <type name="CAMKESConnections"/> <type name="Refinement"/> </projection>
          |
          |<defaultnode/>
          |
          |<defaultedge/>
          |
          |<node>
          |   <type name="AADLComponentInstance"/>
          |   <type name="AADLConnections"/>
          |   <type name="AADLPort"/>
          |   <type name="AADLPortInstance"/>
          |   <type name="CAMKESComponentInstance"/>
          |   <type name="CAMKESConnections"/>
          |   <type name="CAMKESPortInstance"/>
          |   <type name="In"/>
          |   <type name="Int"/>
          |   <type name="Out"/>
          |   <type name="Refinement"/>
          |   <type name="String"/>
          |   <type name="univ"/>
          |   <type name="seq/Int"/>
          |</node>
          |
          |<node color="Blue">
          |   <type name="CAMKESPort"/>
          |</node>
          |
          |<node visible="no">
          |   <type name="AADLComponent"/>
          |   <type name="Direction"/>
          |</node>
          |
          |<node visible="no" color="Blue">
          |   <type name="CAMKESComponent"/>
          |</node>
          |
          |<edge visible="no" attribute="yes">
          |   <relation name="component"> <type name="AADLPort"/> <type name="AADLComponent"/> </relation>
          |   <relation name="direction"> <type name="AADLPort"/> <type name="Direction"/> </relation>
          |   <relation name="direction"> <type name="CAMKESPort"/> <type name="Direction"/> </relation>
          |</edge>
          |
          |</view>
          |
          |</alloy>"""

    return ret
  }
}
