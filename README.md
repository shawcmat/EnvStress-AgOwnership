# EnvStress-AgOwnership

[The effects of environmental stress on global agricultural landownership](https://link.springer.com/article/10.1007/s11111-023-00429-0)

A Bayesian spatial regression exploring the effects of environmental stress on levels of agricultural landownership in affected regions. Published in Population and Environment. If you would only like to reproduce the analysis, use the provided R data object. If you wish to reproduce the dataset, follow the instructions below.

## Reproducing the Dataset

Reproducing the dataset requires manually downloading each of the prequisite datasets before running the code. Required data are as follows.

1. Shapefile or spatial-dataframe containing geometry for each country and administrative unit involved in the analysis. Each unit must have geometry, a defined country name, a defined survey year, a defined region ID, and a recorded agricultural landownership rate. A shapefile containing pre-defined fields can be retrieved from the [DHS survey spatial data repository](https://spatialdata.dhsprogram.com/home/).

2. Global Standardized Precipitation Evapotranspiration Index data files, containing gridded time-series SPEI values going back to 1920. Can be retrieved from the [global SPEI database](https://spei.csic.es/database.html).

3. Global growing season start and end date rasters. Can be retrieved from the [ASAP phenology reference data page](https://mars.jrc.ec.europa.eu/asap/download.php)

4. Global percent-agricultural raster. Can also be retrieved from [ASAP](https://mars.jrc.ec.europa.eu/asap/download.php)

Code for preparing the data is in `BuildDataset.R`. This script will process all primary data sources into the functional dataset used in the analysis. However, this script has not been debugged or rewritten in any way for replication. Data sources and file structures will have to be recreated. Please reach out to the author if you would like to use this script for replication purposes and find you are having issues.

## Reproducing the Analysis

All model code is contained in the `Analysis.Rmd` file. This file contains all code necessary to reproduce the analysis, including data preparation, model fitting, and posterior plot generation.