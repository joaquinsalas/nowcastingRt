# NowcastingRt

These programs implement a method to generate pausible sequences of confirmed positive COVID-19 infected people. 
The method describes statistically the day to day variations of updates and predicts what would it be the 
final count at the end of *D* days, for *D* large.  The programs are fine tuned to run with data for Mexico City, Jalisco, and Queretaro but could easily customized for other Mexican states. In other countries, there may be the need to find the appropriate fields in the datasets.


# Before you run the programs
update the directories in the .Rmd and .m files. Currently, they point to directories in my personal computer.

To run the programs you will need the open datasets at https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-direccion-general-de-epidemiologia. They are provided by the Mexican Health Ministery.

# In R
## update files
*updateDelays.Rmd*, updates the observation of confirmed positives by date of onset.
It generates in a first iteration <updated_delays*.csv> and <2ndStage_updated_delays*.csv> in a second one, one for each state of Mexico under analysis. When there is not <2ndStage_updated_delays*.csv> run it twice.

# In Matlab

Create an object using an operation such as delay = delay* and run delay.runme

Otherwise, execute the following instruction separately

* Create modeld. The instruction calls these routines:
*delay.compoundRateOfChange* takes <2ndStage_updated_delays*.csv> and compute the parameters for the Gamma distributions
in the file <GammaParam*.csv> and the variable <theta>

* Current infectious number: *delay.nowcastingCompound* uses the Gamma distributions to update the current estimate for each day producing in the process uncertainty areas around the mean.

* Infectious sequences
*delay.nowcastingSamples* takes the parameters for the Gamma distributions and generates 1,000 sequences of random number of infectious. The sequences are saved in <infectious_samples.csv>

# In R
Go back to R and execute the following program. Execute the program *estimateRMexicoNowcasting*.Rmd*. These programs will run Cori et al. (2013) library  *EpiEstim*  to compute $R_t$ using the sequences previously constructed.

