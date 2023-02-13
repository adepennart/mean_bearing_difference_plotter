# mean_bearing_difference_plotter
## About
A script for creating circle plots displaying mean bearing difference before and after a treatment.
ie. change in animal direction before and after external cue change


## Installation
This program can be directly installed from github (green Code button, top right).

The code will be located in a downloaded directory named mean_bearing_difference_plotter

### Dependencies
The script runs with Rstudio=2022.12.0+353. Rstudio can be downloading following this link.
https://posit.co/download/rstudio-desktop/
Code not yet tested in R.

## Usage
The script can be run in Rstudio.
Before running the code, the second line must be changed to the directory where the input files are.

Once this is changed the script can be run by pressing run in the top right of the code pane in Rstudio.

### Input

3 input files are used for this code, each with a example file provided.
-data_test.csv
-date.csv
-time.csv

DATA_TEST, is the actual datasheet, the first column is the sample number, the second column is the before and after specifications per beetle and the remaining columns are for the data.

DATE, is the day of the year run, most useful when considering outdoor conditions and correcting for azimtuh changes day to day.

TIME, is the time of day, most useful when considering outdoor conditions are correcting for azimuth changes through a day.
