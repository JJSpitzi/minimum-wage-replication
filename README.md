# Replication Package

This repository contains the code and data required to reproduce the tables and figures in the paper.

## Running the Replication

1. Obtain the IPUMS data file `usa_00003.dat` as described below.
2. Place `usa_00003.dat` in the main repository folder.
3. Open the repository folder in R or RStudio.
4. Run:

```r
source("00_Master.R")
```

The master file runs all R scripts in the correct order and produces all results reported in the paper and appendix.

## Raw Data

### IPUMS USA

The individual-level data are from IPUMS USA, Version 16.0:

https://usa.ipums.org/usa/

The analysis uses ACS samples from 2001–2016 and the variables `YEAR`, `STATEFIP`, `GQ`, `PERWT`, `SEX`, `AGE` and `EMPSTAT`.

To obtain the data:

1. Register or log in to IPUMS USA.
2. Click **Create an Extract**.
3. Select the annual ACS samples from 2001 through 2016.
4. Select `STATEFIP`, `SEX`, `AGE` and `EMPSTAT`. `YEAR`, `GQ` and `PERWT` are included by IPUMS as preselected variables.
5. Go to **View Cart** and click **CHECK OUT: Create Data Extract**.
6. Select **Fixed-width text (.dat)** as the data format and submit the extract.
7. When the extract is ready, download the **data** file.
8. The downloaded file is compressed as `.dat.gz`. Extract it to obtain the `.dat` file.
9. Rename the extracted file to `usa_00003.dat` and place it in the main repository folder.

The corresponding `usa_00003.xml` metadata file is already included in this repository.

### State Minimum Wage Data

The state minimum wage data are from Vaghul and Zipperer (2016), *Historical State and Sub-State Minimum Wage Data*:

https://equitablegrowth.org/working-papers/historical-state-and-sub-state-minimum-wage-data/

The file `VZ_state_annual.xlsx` is included in this repository.

## Code

The R files are numbered in the order in which they are executed. `00_Master.R` runs all files automatically.

## Software

Required non-base R packages are automatically installed by the replication code if they are not already installed.
