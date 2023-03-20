# geoflow-tuna

This repository contains all material needed to update the Tuna atlas workflow by using geoflow package

Please download the "template.env" file and rename it as ".env" with proper values.

1) Clone repository:
```
git clone -b sample https://github.com/firms-gta/geoflow-tunaatlas
```

2) Configure environment file:

```
cd geoflow-tunaatlas
cp env.sample .env && nano .env # Edit file to match your credentials and preferences
```
  
3) Execute GeoFlow
```
library(geoflow) # Load geoflow inside R
executeWorkflow("/path/to/geoflow-tunaatlas/launching_jsons_creating_GTA.R")
```
