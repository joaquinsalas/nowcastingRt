# Nowcasting *R<sub>t</sub>*

These programs implement a method to estimate *R<sub>t</sub>*, the effective reproduction number. In our approach, we generate pausible sequences of positive COVID-19 cases and then employ *EpiEstim*, a public available software library, to compute *R<sub>t</sub>*. Our method to generate sequences describes statistically the day to day variations of updates and predicts the 
final count at the end of *D* days, for a large *D*.  The programs are fine tuned to run with data of states in Mexico, those with enough data to meet the learning stage requirements. In other countries, there may be the need to find the appropriate fields in the available datasets.


# Before you run the programs
Update the directories in the .Rmd and .m files. Currently, they point to directories in my personal computer.

To run the programs you will need the open datasets at https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-direccion-general-de-epidemiologia. They are provided by the Mexican Health Ministery.

# In R
## create html files
Run the programs
* header.Rmd
* title.Rmd
* conclusion.Rmd

They will generate html files useful to visualize the results. You will only need to run these programs once.

Run the program *date.Rmd* to update the date and time for the report.

## update files
Update the directories within the files to point to your local source of data!

I pre-process the data provided by the Mexican goverment. It may take several days of preprocessing the whole dataset for the states and metropolitan areas. For instance, when I started pre-processing the dataset to generate nowcasting for metropolitan areas, I had to extract information from Abril 12 to September 15, 2020. It took my computer 50 hours. The files currently on githup contain the pre-processed datasets until October 14, 2020. If you find these programs useful, I may share more up to date files with you. Please send me a note.

* Run *updateDelays.Rmd*, which updates the observation of confirmed positives by date of onset.
It generates <updated_delays*.csv>, one for each state under analysis in Mexico . 

* Run *updateDelaysMetro.Rmd*, which updates the observation of confirmed positives by date of onset.
It generates <updated_delays*.csv>, one for each metropolitan area under analysis in Mexico. 


# In Matlab

Create an object using an operation such as delay = delaysMX2 and run delay.runme

The program will generate the following outputs:

* Models. The data describing the distributions for the compound rate of change 
in the file <NoParam*.csv> for each state in Mexico for which the learning requirements were met.


* Infectious sequences. 
Using the parameters for the Gamma distributions and generates 1,000 sequences of random number of infectious. The sequences are saved in <infectious_samples*.csv>

In the case of metropolis, perform the same operation using delay = delaysMetro2, followed by delay.runme

# In R
Go back to R and execute the programs *states.Rmd* and *metro.Rmd*. These programs will use the plausible infectious sequences and run Cori et al. (2013) library  *EpiEstim*  to compute *R<sub>t</sub>*. The resulting estimate for *R<sub>t</sub>* will correspond to the mean and one standard deviation.

# Programs output
The programs will generate html files that you may run on a browser. Start with index.html. Also, the programs generate a couple of zip files, estados.zip and metropolis.zip, which contain the csv files appearing in the figures.

# batch files
A possible short-cut to the previous process may be to employ the batch files estimateRtStates.bat, followed by estimateRtStates2.bat for states and estimateRtMetro.bat, followed by estimateRtMetro2.bat for metropolitan areas.

Watch out for the sound emitted by emitSound.m!

# Collaborators
* Dagoberto Pulido
* Samuel Salas
* Joaquín Salas
