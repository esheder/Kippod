# Kippod
This project is the "Kod Israeli Pashut Vepatuah leDiffusia", loosely translated as "Simple Open Source Israeli Diffusion Code".
This project will be a diffusion solver for a nuclear reactor core.

## Disclaimer
Obviously, due to fears over this code being used for ill, this project will not include any data that requires non-open-source software to obtain. Please upload no cross sections, core information or any other data that is not already in the public domain.
This also includes inputs for other software that cannot be run without said software. Even though those aren't directly protected, we want to make sure that our builds don't contain information in their attached files.

## License
This project is currently under no license and currently uses no libraries that require licenses. It will use some LGPL libraries in the future, probably, and this file will be updated accordingly when and if it does.

## Notes
### Project name
The project name is the first thing that came to my mind. On my PC it is still named even otherwise. Tradition dictates I name it after my daughter, but that won't happen any time soon. Therefore any names are welcome. This project will be rebranded as needed.
### Current goal
Currently the project is in its infancy. Modules for I/O, basic data structures and the like are the main focus currently.
A detailed summary of the current objectives will be written into this file, as well as an overview of current plans and schedule.
### Feature requests
We are currently not accepting any feature requests directly. Once the project allows for the calculation of a steady state core at BoC, a plan for future features will be thought of. At that time, requests for features will be considered and evaluated. In the meantine, please use a single text file in the main folder to list feature requests if needed.


## Overview of the code

### Directories
#### utils
This is the main utility directory. This is going to include everything that is just a useful tool that we use here, such as linked lists, mathematical functions and the like. 
#### IO
This directory includes all the input parsers and output buffers. This will later also include restart files and the like.
A thought that came to mind is to simplify the project by writing a python wrapper and communicating most of the parsing through a python-fortran module.
Notice: The project is moving to C++ because FORTRAN is hell. This should make this even more viable, as python and C++ communicate well.
#### solvers
If any home-made solvers are written (probably using a library as part of the process), this is where they will be located. The objective is to eventually use BiCGStab, but we will probably start with GMRES
#### structs
This folder contains all the major nuclear-engineering-inspired classes. These include rods, reflectors, cores, control blades and the like. The idea is to use these structures to get at the underlying data for the matrix generation for the solver. This should allow down the line for the users to be able to easily recall any rod's irradiation histotry and relate it to an actual physical object.
### Data structures
### Cross section formats
### Input file formats
### Makefiles
The code currently uses a makefile in every directory, and a major makefile in the general src directory. As soon as possible we will begin the practice that master must always compile and succeed in all unit tests. Please do all developments in branches and merge into master when stable and only after unit tests are created.

## V&V
### Unit Test Coverage
Every module will be required to have at least one test for every class, as a rule of thumb. Currently we're at 0% coverage, but as I learn how to use unit tests properly we will up the coverage to 100%. This doesn't mean that all the correct tests are required before merger, but it does require at least one test per class. The objective is the be able to save on debugging time later on, and so that when other people modify your code they will know if they change something major.
### Regression Testing
Currently unavailable. Once first k-effective is obtained, a major goal would be to apply automatic regression testing for this on selected problems. The coverage of these problems should cover 100% of any feature created, meaning we should be able to compare the results of any feature with a previous value for it, except for its first appearance. Any change to the regression problems will require project leader approval to allow merger into master. Accepted changes in values should be part of the comparison table.
### Benchmarking
Due to the fact that other than DONJON all nuclear diffusion codes are not open-source, benchmarking will not be part of this project.
Users are invited to benchmark the software against any codes they legally have access to and use that information to improve on the code.
As explained above, no inputs of other codes are allowed in this project, unless those are open-source as well, and the models are based entirely on public-domain data. This is extremely rare and I expect to not see any inputs of other software whatsoever up here.
### Validation
Comparison of calculations using this code as part of a grander lattice/diffusion code package to an actual reactor are an important step in validation of this software at its later stages. Even so, reactor data is rarely public-domain-safe, so we advise the users to do that by themselves without publicly releasing the data. You are welcome to use what you learn from such validations to improve on this software.
#### Verification to analytical models
Verification to analytical models, except for extreme cases, is completely unrestricted. Therefore comparisons to infinite, homogenous reactors and the like are allowed as part of this project, and should be part of the automatic-test list.