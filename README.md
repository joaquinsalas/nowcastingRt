# nowcastingRt

These programs implement a method to generate pausible sequences of confirmed positive COVID-19 infected people. 
The method describes statistically the day to day variations of updates and predicts what would it be the 
final count at the end of *D* days.  The programs are fine tuned to run with data for Mexico City but could easily customized.


# Before you run the programs
update the directories in the .Rmd and .m files. Currently, they point to directories in my personal computer.

To run the programs you will need the open data set at https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-direccion-general-de-epidemiologia:

# In R
## update files
*EstimateDelaysUpdateDF.Rmd*, updates the observation of confirmed positives by date of onset.
It generates <2ndStage_updated_delays.csv>

# In Matlab

create an object using an operation such as delay = delayDF and run delay.runme

Otherwise, execute the following instruction separately

* model. The instruction calls these routines:
*delay.compoundRateOfChange2* takes <2ndStage_updated_delays.csv> and compute the parameters for the Gamma distributions
in the file <GammaParam.csv> and the variable <theta>

* Current infectious number: *delay.nowcastingCompound* uses the Gamma distributions to update the current estimate for each day producing in the process uncertainty areas around the mean.

* Infectious sequences
*delay.nowcastingSamples* takes the parameters for the Gamma distributions and generates 1,000 sequences of random number of infectious. The sequences are saved in <infectious_samples.csv>

# In R
Go back to R and execute the following program. Execute the program *estimateRtMexicoNowcastingDF.Rmd*. This program will run the *EpiEstim* program to compute $R_t$.

