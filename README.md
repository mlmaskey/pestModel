# pestModel
## Introduction
This repository includes a temperature-based model that calculates life cycles of Navelorange Worms up to the sixth generation, including biofix. in tree nuts such as pistachio, almonds, and walnuts. This module utilizes the time series of maximum and minimum temperature sets at a specific point of interest in space under different climate scenarios. This version of the model uses 10 GCM outputs under two emission scenarios: RCP 4.5 and RCP 8.5. 
The input data to the pestModel is stored in the "DATA" folder in a point specific folder [County][id][GCMnames]. These folders include the information downloaded from GCM models, but the pest model considers only two files: tasmax.csv and tasmin.csv as an input to calculate growing degree days (GDD) and the life cycle of thresholds of lifecycles. 

After cloning or downloading the package, the following steps need to proceed.
Store temperature dataset under Data folder.
Create a list of locations or points of interest with the name ListLocations.csv
Open R script "runModel.R" under the R-GUI environment and run the model. 
Before running the model, the essential scripts are function4GDD.R and modelNOW.R




