# AADL-to-CAmkES Translator (ACT)

[![Actions Status](https://github.com/sireum/act/workflows/CI/badge.svg)](https://github.com/sireum/act/actions)
| :--: |
| <sub><sup>amd64: mac, linux, windows</sup></sub> | 
## Installation

### OSATE Version
* OSATE installation and usage instructions are available on ACT's [update site](https://github.com/sireum/act-plugin-update-site).

### Command Line Version
* Use the following to build ACT's CLI

   ```bash
   git clone --recursive https://github.com/sireum/kekinian.git
   cd kekinian
   bin/build.cmd
   ```
* Run ``bin/sireum aadl act`` (or ``bin/sireum.bat aadl act`` under Windows) to view the available command line options

## Example CLI Usage
The following uses the JSON serialized AIR [file](https://github.com/loonwerks/CASE/blob/7b05fa2916b276d13374624c800ffe6af523dabd/CASE_simple_example_Build/.slang/MC_MissionComputer_Impl_Instance.json) that was generated from the [MissionComputer.Impl](https://github.com/loonwerks/CASE/blob/7b05fa2916b276d13374624c800ffe6af523dabd/CASE_simple_example_Build/MC.aadl#L96) system implementation.

  ```bash
  bin/sireum.bat aadl act -o ./act-output/ CASE/CASE_Simple_Example_V2_Build/.slang/MC_MissionComputer_Impl_Instance.json
  ```
