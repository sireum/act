# AADL-to-CAmkES Translator (ACT)

| Branch | macOS | Linux | Windows |  
| :----: | :---: | :---: | :---: | 
| master | [![Travis CI Status](https://travis-ci.org/sireum/act.svg?branch=master)](https://travis-ci.org/sireum/act) | [![Shippable Status](https://api.shippable.com/projects/5b95b754fb783206009ba34e/badge?branch=master)](https://app.shippable.com/projects/5b95b754fb783206009ba34e) | [![AppVeyor Status](https://ci.appveyor.com/api/projects/status/fdebboflkpehf447/branch/master?svg=true)](https://ci.appveyor.com/project/robby-phd/act/branch/master) |

## Installation

### OSATE Version
* OSATE installation and usage instructions are available on ACT's [update site](https://github.com/sireum/act-plugin-update-site).

### Command Line Version
* Use the following to build ACT's CLI

   ```bash
   git clone --recursive https://github.com/sireum/act.git
   cd act
   bin/build.cmd build
   ```
* Run ``bin/act`` (or ``bin/act.bat`` under Windows) to view the available command line options

## Example CLI Usage
The following uses the JSON serialized AIR [file](https://github.com/loonwerks/CASE/blob/7b05fa2916b276d13374624c800ffe6af523dabd/CASE_simple_example_Build/.slang/MC_MissionComputer_Impl_Instance.json) that was generated from the [MissionComputer.Impl](https://github.com/loonwerks/CASE/blob/7b05fa2916b276d13374624c800ffe6af523dabd/CASE_simple_example_Build/MC.aadl#L96) system implementation.

  ```bash
  bin/act -o ./act-output/ CASE/CASE_Simple_Example_V2_Build/.slang/MC_MissionComputer_Impl_Instance.json
  ```
