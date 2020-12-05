# pestModel
## Introduction
This repository includes a temperature-based model that calculates life cycles of Navelorange Worms up to the sixth generation, including biofix. in tree nuts such as pistachio, almonds, and walnuts. This module utilizes the time series of maximum and minimum temperature sets at a specific point of interest in space under different climate scenarios. This version of the model uses 10 GCM outputs under two emission scenarios: RCP 4.5 and RCP 8.5. 

## Fundamental Input

The input data to the pestModel is stored in the "DATA" folder in a point specific folder [County][id][GCMnames]. These folders include the information downloaded from GCM models, but the pest model considers only two files: tasmax.csv and tasmin.csv as an input to calculate growing degree days (GDD) and the life cycle of thresholds of lifecycles. 

### Required  files 

The model follder one needz to create one folder for data set, named `DATE` and a list of locations or points of interest with the name `ListLocations.csv.` This file includes `Points ID`, `County`, `Latitude`, `Longitude`, `County.1.` The main file is the `runModel.R`  that needs to be open under the R-GUI environment to run the model. This file may require to modify the statistics of analysis, for instance, `Stat`=`MEDIAN.`

## Scripts

Before running the model, the essential scripts are `function4GDD.R` and `modelNOW.R.` The former scrriptfile includes all the routineds neceaasry to run the model and some of them could be used for post processing as well. The later one is the main routine for the pest model.


## Output
This program creates the output folder `Output` under which it generates the sequence of files as per locations listed on `ListLocations.csv.` First, it generates the time series of GDD and accumulated GDD in file `[RCPNAME]Year.csv` under the folder named  `[CROPNAME]/[SITENAME]` together with time series plot under the same folder. These `csv` files can be used for post processing. It also generates a set of `csv` files for each genation and each crop under `[CROPNAME]/Statistics/[#SITENAME][RECPBAME[Date,csv.`







