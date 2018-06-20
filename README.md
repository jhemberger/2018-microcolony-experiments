# Microcolony Landscape Resource Simulation Experiments

This experiment was designed to simulate variable resource landscapes for foraging Bombus impatiens colonies.  This markdown contains a summary of the experiment as well as a description of the scripts and files within this repository.  

## Experiment Motivation

Free foraging bumble bee colonies are forced to respond to variable resource abundance over space and time as their colonies are centrally located and placement of colonies is selected by the queen early in the season.  As such, workers must make do with what resources are within their foraging range.  This experiment seeks to simulate variable quality resource landscapes both in terms of the abundance of food (pollen and nectar) available, as well as the temporal continuity of food offered.  See the experimental design, methods, and/or results for more information.

## Repository Files

#### R Scripts
* `D2018_MicroCol.R` R script for data cleaning/preparation and exploratory analysis

#### Data files
* `D2018_MircoCol_Round1.csv` ORIGINAL DATA csv file for first round of this experiment
* `D2018_MircoCol_Round1_Clean.csv` Cleaned data set using `D2018_MicroCol.R` script
* `D2018_MircoCol_Breakdown.csv` ORIGINAL DATA csv file containing final measurements of experiment - integrated into `D2018_MircoCol_Round1_Clean.csv` using `D2018_MicroCol.R`

#### Markdown Files
* `EXP_DESIGN.md` Background, hypotheses, design and treatment explanation
* `EXP_METHODS.md` Feeding methodology, protocols, etc
* `EXP_RESULTS.md` Summarize results from analyses

#### Additional Scripts
None at this time  