# pestModel
## Introduction
This repository includes a temperature-based model that calculates life cycles of Navelorange Worms up to the sixth generation, including biofix. in tree nuts such as pistachio, almonds, and walnuts. This module utilizes the time series of maximum and minimum temperature sets at a specific point of interest in space under different climate scenarios. This version of the model uses 10 GCM outputs under two emission scenarios: RCP 4.5 and RCP 8.5. 

## Fundamental Input

The input data to the pestModel is stored in the "DATA" folder in a point specific folder [County][id][GCMnames]. These folders include the information downloaded from GCM models, but the pest model considers only two files: tasmax.csv and tasmin.csv as an input to calculate growing degree days (GDD) and the life cycle of thresholds of lifecycles. 

### Required  files 

The model follder one needz to create one folder for data set, named `DATE` and a list of locations or points of interest with the name `ListLocations.csv.` This file includes `Points ID`, `County`, `Latitude`, `Longitude`, `County.1.` The main file is the `runModel.R`  that needs to be open under the R-GUI environment to run the model.

## Scripts

Before running the model, the essential scripts are `function4GDD.R` and `modelNOW.R.` The former scrriptfile includes all the routineds neceaasry to run the model and some of them could be used for post processing as well. The later one is the main rouutine for pest model.






