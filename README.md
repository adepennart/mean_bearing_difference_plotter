# mean_bearing_difference_plotter
## About
A script for creating circle plots displaying mean bearing difference before and after a treatment.
ie. change in animal direction before and after external cue change


## Installation
This program can be directly installed from github (green CODE button, top right).

The code will be located in a downloaded directory named mean_bearing_difference_plotter.

### Dependencies
The script runs with Rstudio=2022.12.0+353. Rstudio can be downloading following this link.
https://posit.co/download/rstudio-desktop/
Code not yet tested in R.

## Usage
The script can be run in Rstudio.
Before running the code, the second line must be changed to the directory where the input files are.

Once this is changed the script can be run by pressing SOURCE in the top right of the code pane in Rstudio.

### Input

3 input files are used for this code, each with a example file provided.

-data_test.csv

-date.csv

-time.csv

DATA_TEST, is the actual datasheet, the first column is the sample number (1,2,...,n), the second column specifies the 'before condition (1)' and the 'after condition (2)' and the remaining columns are for the exit angles(0,355).

DATE, is the day of the year, needed to correct the azimuth changes in a day accurately. The first column is the sample number (1,2,...,n), the second column specifies the 'before condition (1)' and the 'after condition (2)' and the remaining columns are for the dates(1-30). 

TIME, is the time of day, needed when correcting for azimuth changes through a day. The first column is the sample number (1,2,...,n), the second column specifies the 'before condition (1)' and the 'after condition (2)' and the remaining columns are for the times(ie. 13:44). If running indoor experiments, or where azimuth is not changing, write the same time for the 'before' and 'after' condition.

### output

2 types of output files will be created in the INPUT folder.

-azi_dataframe.csv

-plots, (ie. condition_1.csv, condition_2.csv, condition_3.csv)

AZI_DATAFRAME, has 7 columns. The first column has the sample names, the second column has the mean vector length, the third has the difference in mean bearing, the fourth has the standard deviation value, the fifth has the angular deviation value, the sixth has the rayleigh test and the seventh has the sample size.

PLOTS, are the circular plots showing difference in mean bearing with the appropriate angular deviation and mean vector. All sample points will be seen as dots on the circle.
