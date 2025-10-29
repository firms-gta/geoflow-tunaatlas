---
output:
  html_document: default
  pdf_document: default
---

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11563961.svg)](https://doi.org/10.5281/zenodo.11563961)



# Geoflow-tuna repository

This repository contains all material needed to update the Tuna atlas workflow by using geoflow package

1) Clone repository:
```
git clone -b sample https://github.com/firms-gta/geoflow-tunaatlas
```

2) Configure environment file:

To fully recreate the workflow and populate a database with the processed data, it is necessary to have a configuration file in the format of `config/template.env`. This file should include the connection details to a database where the data will be stored. Ensure that you have the proper database access and credentials before attempting to execute the workflow.
  
3) Execute GeoFlow
```
library(geoflow) # Load geoflow inside R
executeWorkflow("/path/to/geoflow-tunaatlas/launching_jsons_creating_GTA.R")
```

## Workflow Overview

The creation of the datasets involves several stages:

- **Pre-processing of tRFMOs data formats**: Initial treatment to standardize the diverse data formats across tRFMOs.
- **Data processing on tRFMOs data**: Detailed processing to clean, validate, and prepare the data for aggregation.
- **Aggregation of data**: Combining and georeferencing the data by tRFMOs to create comprehensive datasets that include spatial information.
- **Reporting**: Generation of detailed reports at each stage to document the process and outcomes.

### Datasets

Upon completion, the workflow produces five main datasets, one for each tRFMO. These datasets include:

- **Raw dataset**: The unprocessed collection of data as retrieved from the sources after pre-harmonisation.
- **Level 0 dataset**: A global dataset addressing overlaps in zones and standardizing spatial references.
- **Level 1 dataset**: Harmonized data with conversions from numbers to tonnes to unify measurement units.
- **Level 2 dataset**: Enhanced georeferenced data based on nominal data, tryng to increase the spatial detail and utility of the dataset.

All datasets created are accessible under DOI, ensuring easy access and citation for research and analysis purposes.
