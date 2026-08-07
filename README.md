Replication Package



This repository contains the code and data instructions required to reproduce the tables and figures in the paper.



\## Running the Replication



1\. Place the following files in the main repository folder:



&#x20;  \* `usa\\\_00003.xml`

&#x20;  \* `usa\\\_00003.dat`

&#x20;  \* `VZ\\\_state\\\_annual.xlsx`

2\. Open the repository folder in R or RStudio.

3\. Run:



```r

source("00\\\_Master.R")

```



The master file runs all R scripts in the correct order and produces the results reported in the paper and appendix.



\## Raw Data



\### IPUMS USA



The individual-level data are from IPUMS USA:



https://usa.ipums.org/usa/



The analysis uses ACS data from 2001 to 2016 and the variables `YEAR`, `STATEFIP`, `GQ`, `PERWT`, `SEX`, `AGE` and `EMPSTAT`.



The IPUMS microdata are not included because redistribution is restricted. Download the data and corresponding XML file from IPUMS and name them:



\* `usa\\\_00003.dat`

\* `usa\\\_00003.xml`



\### State Minimum Wage Data



The state minimum wage data are from Vaghul and Zipperer (2016), \*Historical State and Sub-State Minimum Wage Data\*:



https://equitablegrowth.org/working-papers/historical-state-and-sub-state-minimum-wage-data/



The file used in the replication is `VZ\\\_state\\\_annual.xlsx`.



\## Code



The R files are numbered in the order in which they are executed. `00\\\_Master.R` runs all files automatically.



\## Software



The required non-base R packages are automatically installed by the replication code if they are not already installed.

