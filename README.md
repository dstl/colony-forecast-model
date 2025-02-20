# Colony Forecasting Model

This code provides an implementation of the model described in ["Paper title..."](link_to_paper). It consists of a set of R scripts that contain functions 
to simulate the population dynamics of an experimental primate colony, taking into account births, deaths, demographics, breeding controls and other potential 
outlets from the population (e.g. relocations). 

Please note that colony data used to generate the results in the paper cannot be made public, therefore the 
code in this repo will not reproduce the results of the paper. All data contained in these scripts is illustrative. 

The scripts have not been compiled into an R package, to make it simple for new users to interrogate and modify the code for their own use cases. 

The instructions provided below will enable you to get the model up and running on your local machine. 


## Published article

Technical description of the model and details of data requirements are provided in the following article from Nature Lab Animal: 

["Paper title..."](link_to_paper)


## Requirements

- R version >= 4.x


## Installation

- Download and install R.
- Clone the model R scripts from [this repo](https://github.com/dstl/colony-forecast-model), saving them in the working directory of your choice.


## Running the model

The model consists of two R scripts:

- [colony_forecast](colony_forecast.R): This script contains the main model function, `colony_forecast`, which implements Algorithm 1 from the paper. 
- [helper_functions](helper_functions.R): This script contains a series of functions that perform individual calculations, which are called by `colony_forecast`.

The model is based around the specific colony demographic structure described in the paper. If you intend to apply the model 
to a different colony, then some modification of the model may be required for your context. The article provides advice for making such modifications. 
Data to parametrise the model will be derived from your own colony record-keeping system (see [the Nature Lab Animal article](link_to_paper) for data requirements).

A third script is also provided, [demo_colony_forecast](demo_colony_forecast.R). This script demonstrates how to set up and run the model. Please note that you will need to insert the path to the working directory where the model scripts are saved, at the variable `working_dir_path`.
Having been pointed to the working directory, this script sources the two model scripts described above, and runs the model using dummy input parameters. 
These are the data items that will need to be derived from the record-keeping system whenever the model is applied to a real colony. 
Several illustrative scenarios are considered, and graphs are plotted to enable the user to compare the model results for each scenario. 


## Unit testing

Unit testing is an essential part of software design. Two scripts containing unit tests for the functions that comprise this model are included in the sub-directory [tests](tests):

- [test_helper_functions](tests/test_helper_functions.R): Unit tests for the lower-level functions called by the main model function (`colony_forecast`).
- [test_colony_forecast](tests/test_colony_forecast.R): Unit tests for the main model function.

Please note the following two things are required, to be able to run the tests:

- An additional R package, `testthat`, must be installed. This package is available from [https://cran.r-project.org/web/packages/testthat/index.html](https://cran.r-project.org/web/packages/testthat/index.html).
- You must insert the path of your working directory (i.e. where the model scripts are saved) where indicated, assigning it to the variable `working_dir_path`. This is required in both scripts.

Once `testthat` is installed and the variable `working_dir_path` is assigned, the two test scripts can be run. Please note that [test_colony_forecast](tests/test_colony_forecast.R) loads dummy model parameters (similar 
to those used in [demo_colony_forecast](demo_colony_forecast.R)) from an .RData file, [testing_dummydata](testing_dummydata.RData).

If these instructions are followed, the model will pass all of its unit tests. However, if any of the model functions are modified, for instance when customising the model for a new colony, it is expected that relevant unit tests will fail, due to the model changes. 
Therefore, it is recommended, as best practice, to make a habit of modifying the unit tests accordingly and iteratively re-testing, to ensure that the model continues to work as intended.


## Authors

  - **Joe Gillard** - jgillard@dstl.gov.uk - Model developer and owner
