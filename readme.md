# AADL-to-CAmkES Translator (ACT)

| Branch | macOS | Linux | Windows |  
| :----: | :---: | :---: | :---: | 
| master | [![Travis CI Status](https://travis-ci.org/sireum/act-plugin.svg?branch=master)](https://travis-ci.org/sireum/act-plugin) | [![Shippable Status](https://api.shippable.com/projects/5b95b754fb783206009ba34e/badge?branch=master)](https://app.shippable.com/projects/5b95b754fb783206009ba34e) | [![AppVeyor Status](https://ci.appveyor.com/api/projects/status/c92gofvirrfbaxu0/branch/master?svg=true)](https://ci.appveyor.com/project/robby-phd/act-plugin/branch/master) |

## Installation

### OSATE Version
* OSATE installation and usage instructions are available on ACT's [update site](https://github.com/sireum/act-plugin-update-site).

### Command Line Version
* Use the following to build ACT's CLI

   ```bash
   git clone https://github.com/sireum/act-plugin.git
   cd act-plugin
   bin/test.sh
   ```
* Run ``bin/act`` (or ``bin/act.bat`` under Windows) to view the available command line options

## Example CLI Usage
The following uses the JSON serialized AIR [file](https://github.com/sireum/CASE/blob/9b19584f0d9fe315ecc055385cf4d3fe7a4d81ac/CASE_simple_example_act/.slang/MC_MissionComputer_Impl_Instance.json) that was generated from the [MissionComputer.Impl](https://github.com/sireum/CASE/blob/9b19584f0d9fe315ecc055385cf4d3fe7a4d81ac/CASE_simple_example_act/MC.aadl#L96) system implementation.

  ```bash
  bin/act -o ./act-output/ CASE/CASE_simple_example_act/.slang/MC_MissionComputer_Impl_Instance.json
  ```
