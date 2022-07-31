# EnvStress-AgOwnership

A Bayesian spatial regression exploring the effects of environmental stress on levels of agricultural landownership in affected regions. If you would only like to reproduce the analysis, use the provided R data object. If you wish to reproduce the dataset, follow the instructions below.

## Running the Analysis


More info to come.



## Reproducing the Dataset

Reproducing the dataset requires manually downloading each of the prequisite datasets before running the code. Required data are as follows.

1. Shapefile or spatial-dataframe containing geometry for each country and administrative unit involved in the analysis. Each unit must have geometry, a defined country name, a defined survey year, a defined region ID, and a recorded agricultural landownership rate. A shapefile containing pre-defined fields can be retrieved from the [DHS survey spatial data repository](https://spatialdata.dhsprogram.com/home/).

2. Global Standardized Precipitation Evapotranspiration Index data files, containing gridded time-series SPEI values going back to 1920. Can be retrieved from the [global SPEI database](https://spei.csic.es/database.html).

3. Global growing season start and end date rasters. Can be retrieved from the [ASAP phenology reference data page](https://mars.jrc.ec.europa.eu/asap/download.php)

4. Global percent-agricultural raster. Can also be retrieved from [ASAP](https://mars.jrc.ec.europa.eu/asap/download.php)

