# *Parturition Movement Rate*

MoveApps

Github repository: *https://github.com/ocouriot/parturition-movement-rate* 

## Description
This MoveApp estimates calving (i.e., parturition) status, timing and location, using GPS data of collared animals. The App has been conceived based on the R function from the `TuktuTools` R package. The method has been developed to infer calving events of female barren-ground caribou (*Rangifer tarandus spp.*), by analyzing their movement patterns, and more specifically, their movement rate. 

This tool is a refinement of the the "individual-based method" (IBM) developed by DeMars et al (2013) to infer calving status. This method assumes that a female's movement rate drops rapidly when she calves, and that displacements remain low for several days or weeks if the calf survives, since neonates are not able to move as quickly as adults. Calving events are identifiable by a sudden and marked change – or break point – in a female's mean step length (i.e. the distance between two successive relocations). Conversely, if the female loses her calf during the neonatal period, a second break point would occur in the female movement pattern, since the female would recover her pre-calving movement rate abruptly. The method also assumes that non-calving females maintian a constant mean movement rate throughout the entire calving period. 

The DeMars method assumes a regularly sampled time series of observations, since the analysis is performed on step lengths. We adapt the method to account for variability in sample schedules and missing data by performing analyses on movement *rates* (i.e. the distance traveled divided by the interval between observations) instead of on step length. More details on this method can be found in Couriot et al. (2023), and users are encouraged to read the paper for a deeper understanding of the methodology. 

## Documentation

The App first censors and trims data to focus on the calving period, to avoid detecting behaviors other than calving. For barren-ground caribou, The calving period has been described to occur between May 19 and July 07, for example (Cameron et al. 2018). 
The App also guarantees a minimum number of location fixes a day, and removes individuals with gaps in the data greater than a threshold. 
The App then estimates movement rate between subsequent relocations for each individual in the dataset, then determines the calving status of a female (no calf, with a calf or lost her calf), the calving date and location (if any), as well as the calf death date and location (if any). 


### Input data

MoveStack in Movebank format

### Output data

App returns calving_results, no additional output is produced to be used in subsequent apps.

### Artefacts
*If the App creates artefacts (e.g. csv, pdf, jpeg, shapefiles, etc), please list them here and describe each.*


### Settings 

`start`,`end` period of interest start and end dates, formatted as character strings in the form "mm-dd" (e.g. "05-19" and  "07-07")

`nfixes` individuals with fewer than *nfixes* observations per day over the period of of interest are removed

`dayloss` individuals with this many consecutive days with missing locations are removed. 

`restrictive` if TRUE, only individuals having monitoring for the entire period are considered

`int` integer value indicating the minimum number of days between the beginning of the time series and the first BP (calf birth), the last BP (calf death) and the end of the time series, and between the two BPs. The main reason for this constraint is that a minimum number of data points are required in each section of the time-series to be able to estimate model parameters. We recommend 9 relocations (e.g. 3 days at 3 locations / day).

`kcons` constraints on the recovery parameter *k*, as a vector of the minimum and maximum time it takes the female to recover normal movement rate (in days).  

`models` either *"full"* to fit all three models (i.e., no calf, calf and calf death models), or *"calfonly"* to fit only the no calf and calf models


### Most common errors
*Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.*

### Null or error handling
*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if settings/parameters are improperly set and any other important information that you find the user should be aware of.*

### References 

Cameron, M. D., Joly, K., Breed, G. A., Parrett, L. S., & Kielland, K. (2018). Movement-based methods to infer parturition events in migratory ungulates. Canadian Journal of Zoology, 96(11), 1187–1195. https://doi.org/10.1139/cjz-2017-0314

Couriot, O. H., Cameron, M. D., Joly, K., Adamczewski, J., Campbell, M. W., Davison, T., Gunn, A., Kelly, A. P., Leblond, M., Williams, J., Fagan, W. F., Brose, A., & Gurarie, E. (2023). Continental synchrony and local responses: Climatic effects on spatiotemporal patterns of calving in a social ungulate. Ecosphere, 14(1), e4399. https://doi.org/10.1002/ecs2.4399

DeMars, C. A., Auger-Méthé, M., Schlägel, U. E., & Boutin, S. (2013). Inferring parturition and neonate survival from movement patterns of female ungulates: A case study using woodland caribou. Ecology and Evolution, 3(12), 4149–4160. https://doi.org/10.1002/ece3.785

