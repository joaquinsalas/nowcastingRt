# Nowcasting *R<sub>t</sub>*

These programs implement a method to estimate *R<sub>t</sub>*, the basic reproduction number. In our approach, we generate pausible sequences of confirmed positive COVID-19 infected people and then employ *EpiEstim*, a public available software library, to compute *R<sub>t</sub>*. 
Our method to generate sequences describes statistically the day to day variations of updates and predicts what would it be the 
final count at the end of *D* days, for *D* large.  The programs are fine tuned to run with data for states in Mexico, one ones with enough data to meet the learning stage requirements. In other countries, there may be the need to find the appropriate fields in the available datasets.


# Before you run the programs
Update the directories in the .Rmd and .m files. Currently, they point to directories in my personal computer.

To run the programs you will need the open datasets at https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-direccion-general-de-epidemiologia. They are provided by the Mexican Health Ministery.

# In R
## update files
*updateDelays.Rmd*, updates the observation of confirmed positives by date of onset.
It generates in a first iteration <updated_delays*.csv> and <2ndStage_updated_delays*.csv> in a second one, one for each state of Mexico under analysis. 

# In Matlab

Create an object using an operation such as delay = delayMX and run delay.runme

The program will generate the following outputs:

* Models. The parameters for the Gamma distributions
in the file <GammaParam*.csv> for each state in Mexico for which the learning requirements were met.


* Infectious sequences. 
Using the parameters for the Gamma distributions and generates 1,000 sequences of random number of infectious. The sequences are saved in <infectious_samples*.csv>

# In R
Go back to R and execute the program *estimateRMXNowcasting.Rmd*. These programs will use the plausible infectious sequences and run Cori et al. (2013) library  *EpiEstim*  to compute *R<sub>t</sub>*. The resulting estimate for *R<sub>t</sub>* will correspond to the mean and one standard deviation.

