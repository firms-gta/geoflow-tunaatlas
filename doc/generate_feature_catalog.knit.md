---
title: "A data warehouse for fisheries aggregated datasets: the Tuna Atlas use case"
date: "2 avril 2020"
abstract: |
  Gathering fisheries data within a single database is a challenge when dealing with high resolution and highly heterogeneous data describing different parameters of fisheries (species, fishing gears, flags..). Most of the open (public domain) data in fisheries management domain are delivered by RFMOs as multi-dimensional (aggregated) data complying with different data structures. Indeed countries only accept data sharing at a low resolution, data have to remain anonymous and are therefore aggregated on spatial grids. We present here a set of tools to gather aggregated fisheries data delivered by different RFMOs within a single data warehouse which is meant to produce global datasets. FAIR data services are delivered on top of this data warehouse which is part of a Spatial Data Infrastructure. We describe the conceptual model and its implementation to fit the needs of building a global atlas of tuna fisheries. 
output:
  html_document:
    toc: true
    toc_depth: 2
    number_sections: true
    df_print: paged
---



# Multi-dimensional Fisheries (aggregated) data


The kind of data we are dealing with are **multi-dimensional data**, as well named **aggregated data** or multi-dimensional cubes (see Figure 1). Such data are made of:

* **measures** of different **variables** stored (also named parameters or facts, see section below) in a given **unit of measure**. Each variable takes its values in a space made of whole or part of these dimensions: variable(dim1, dim3, dim5..).
* **dimensions** coordinates which gives the **context of reported data** and can be shared by different variables:
    - Spatial dimension
    - Temporal dimension
    - Taxonomic / species dimension
    - Fishing gear dimension
    - Country / Flag dimension
    - ..


[<img   src="https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/multi_dimensional_dat_cube.svg?sanitize=true" style="text-align:center" width="800">](https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/multi_dimensional_dat_cube.svg?sanitize=true)


**Figure 1**: illustration of multidimensional data cube (adapted from [wikipedia](https://upload.wikimedia.org/wikipedia/commons/a/ae/Rubik%27s_cube_scrambled.svg) )

Multiple options can be considered to manage these kinds of data, eg : 

* XML format with SDMX implemented by organizations like Eurostat
* Binary data formats like NetCDF / HDF widely used to manage model outputs, remote sensing and in situ sensors data 
* SQL multi-dimensional data warehouse which enable to gather aggregated data within a single database

The CDL language used to formally describe the data structure in the headers of NetCDF files use the following syntactic convention to define what dimensions are used by a variables : **variable**(*dimension_1, dimension_2, ..dimension_n**). Eg of variables for tuna fisheries: 

* **catch**(*schooltype,species,time,area,gear,flag,catchtype,unit,source*)
* **effort**(*schooltype,time,area,gear,flag,unit,source*)
* **catch_at_size**(*schooltype,species,time,area,gear,flag,catchtype,sex,unit,sizeclass,source*)
* **conversion_factor**(*schooltype,species,time,area,sex*)

Since data to be managed comes from multiple providers, keeping tracks of metadata is a key issue for both data sources and related products. **Figure 2** summarizes the key classes that we consider for our model (expressed as a UML class diagram).

<img src="https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/UML_conceptual_model.svg?sanitize=true" width="800">

**Figure 2**: High level UML class diagram for multidimensional data management


We decided to implement this model in a **SQL data warehouse** which is used to load aggregated data provided by tuna RFMOs from which global and harmonized datasets are processed, stored and published. This data warehouse is also part of a wider spatial data infrastructure complying with FAIR data management principles for both data discovery and data access (see details in section XX). However, some past work has already been done in the past to export such data cubes in NetCDF or SDMX data format to fit the needs of different communities of users.


# Model of the SQL data warehouse

The SQL model of the data warehouse is inherited from conceptual model presented in previous section. In the context os SQL data warehouse variables are named **facts**.

## Conceptual Model

The model used is a classical **data warehouse** model:

* the part of the model dedicated to the management of each variable, named **fact** in the context of a data warehouse, is a mix of a star and snowflake schemas. This part of the model is usually called a **data mart**. See **Figure 2**.
* since facts are sharing numerous dimensions, the whole model is a **constellation model** which makes it a real data warehouse for multi-dimensional data. See **Figure 3**.
* Datasets loaded or produced in the data warehouse are described by metadata with following tables:
    - metadata_dcmi for global / discovery metadata complying with Dublin Core standard
    - data_dictionnary to describe the data structure complying with OGC feature catalog standard

[<img src="https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/fact_star_model.svg?sanitize=true" width="400">](https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/fact_star_model.svg?sanitize=true)

**Figure 2**: focus on the catch fact (variable) and related dimension, a typical **star schema** of a **data mart**.


<!-- <img  src="http://mdst-macroes.ird.fr/documentation/databases/Sardara/modelesSardara/SardaraV2/mld_sardara.svg" width="500"> -->

[<img src="https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/UML_diagram_class_catch_and_effort.svg?sanitize=true" width="400">](https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/UML_diagram_class_catch_and_effort.svg?sanitize=true)

**Figure 3**:  catch and effort facts (variables) have most of dimensions in common and generates a typical **constellation schema**.


## Management of variables

The implementation of the conceptual model described in section XX generates **a table for each variable / fact** (even if one table might contain data for all varianles). This **fact table** is made of following columns:

* a column for each dimension related to the fact which stores the foreign key of the dimension table
* the measure of the fact is made of two columns:
    - a column which stores the measured **value** of the variable (the fact itself)
    - a column which specifies the **unit of measure**,
* a column which indicates which dataset this value is part of (the foreign key of the **metadata** table).

The fact table is this the most important table but it only contains numbers (see sample in table below) sinc it is made of the value of the fact and the foreign keys of related dimensions. 

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Sample of catch fact table</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> id_catch </th>
   <th style="text-align:right;"> id_metadata </th>
   <th style="text-align:right;"> id_schooltype </th>
   <th style="text-align:right;"> id_species </th>
   <th style="text-align:right;"> id_time </th>
   <th style="text-align:right;"> id_area </th>
   <th style="text-align:right;"> id_gear </th>
   <th style="text-align:right;"> id_flag </th>
   <th style="text-align:right;"> id_catchtype </th>
   <th style="text-align:right;"> id_unit </th>
   <th style="text-align:right;"> id_source </th>
   <th style="text-align:right;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 13451114 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 71320 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 499 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 35.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451115 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12641 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 71320 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 499 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 407.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451116 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12638 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 71320 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 499 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 12.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451117 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12639 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 12.10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451118 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 9.33 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451119 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5.62 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451120 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2.34 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451121 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 21.60 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451122 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 8.60 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13451123 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 12640 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 33443 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
</tbody>
</table>

To make it human readable queries have to be made to join the content of dimensions tables where codes and labels are stored.

## Management of dimensions


The implementation of the conceptual model described in section XX generates at least two tables for each dimension:

* a **reference table**  (whose naming convention is “dimension”, eg “species”) which stores all possible codes for this dimension, first values being the standard codification chose by default.
* a **mapping table** (with the following naming convention : “dimension_mapping”, eg “species_mapping”) which enables to map codes from different codelists:
* as many tables as **specific codeslists** used by raw datasets loaded in the data warehouse. 

This enables to store raw dataset as provided by data producers and to transform by using standard codifications. Indeed, whenever possible each dimension of the fact table should take its value in a standard codification (eg CWP recommended codelists). However, since mapping between codifications may result in a loss of original details, we decided to store the low level / raw data by keeping their native codifications. Only global levels will be harmonized from level 0 by using standard codifications and thus will rely on a real star schema.

This means that each dimension has at least two tables. Additional tables containing native codifications can also be loade and reused, if needed, by querying the “dimension_mapping” table which redirect codes to their full definition within dedicated tables. See **Figure 4**.

[<img src="https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/UML_conceptual_model_dimension.svg?sanitize=true" width="400">](https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/doc/database_model_diagrams/UML_conceptual_model_dimension.svg?sanitize=true)


**Figure 4**:  management of multiple codifications for each dimension by using a mapping table to relate codes stored in the dimension table and defined in other tables.

Here is the list of standard dimensions implemented by default in the data warehouse [ref]:

* **Species**: the [ASFIS List of Species for Fishery Statistics Purposes](http://www.fao.org/fishery/collection/asfis/en) 
* **Gear**: the [International Standard Statistical Classification of Fishing Gear (ISSCFG)](http://www.fao.org/cwp-on-fishery-statistics/handbook) 
* **Flag**: the [UN Standard country or area codes for statistical use](https://unstats.un.org/unsd/methodology/m49/) .
* **Area**: CWP grids
Others ??







## Management of metadata

Metadata management is a key issue in an ETL workflow to keep track of all datasets loaded in the data warehouse. Two kinds of metadata are needed and stored in two different tables (see Figure 2):

* **Discovery metadata** (stored in **metadata_dcmi table** in the physical model) that can be summarized as main / global metadata elements which describes high level characteristics (title, description, keywords, temporal and spatial extent…) of the main datasets of the data warehouse. Since this table stores the SQL query which enables to extract each dataset, this table is properly speaking, a **catalog of SQL queries**. A recursive link on “metadata_dcmi” table enables to manage relationships between dataset and leads to the “metadata_mapping” table (see related assoction class in UML diagram of Figure 1).
* **Usage metadata** (data_dictionnary table) that are required for a proper reuse of the dataset. The metadata elements, in this case, are mainly a **data dictionary** defining attributes and their use.



## Physical Model

The implementation of previous conceptual model for the facts leads to the physical model presented in Figure 5.

[<img src="http://mdst-macroes.ird.fr/documentation/databases/Sardara/modelesSardara/SardaraV2/mld_sardara.svg" width="500">](http://mdst-macroes.ird.fr/documentation/databases/Sardara/modelesSardara/SardaraV2/mld_sardara.svg)

**Figure 5**: Logical model of the tuna atlas database (see full size image [here](http://mdst-macroes.ird.fr/documentation/databases/Sardara/modelesSardara/SardaraV2/mld_sardara.svg)). 

Depending on the number of datasets loaded and their related codifications, the number of tables in each dimension schema can increase (see explantations in section XX).

The SQL code of the physical model is made available on [github repository](https://github.com/eblondel/geoflow-tunaatlas/tree/master/tunaatlas_sql).


# Implementation : the Tuna Atlas use case


This approach has been implemented to load tuna RFMOs datasets within a data warehouse in ordre to build a global tuna atlas. The database is part of a spatial data infrastructure hosted by a virtual research environment powered by a european research infrastructure (d4science) in the context of multiple projects.

## Tuna Atlas datasets

The data warehouse desceribed in previous section has been developed, tested and improved to fit the needs of tuna fisheries datasets management. An ETL workflow has been set up to load the 5 tuna RFMOs datasets within a single database. The model has been used to store catch, fishing effort and size class data.

The datasets to be loaded in the data  warehouse are listed in tables complying with the same data structure to feed the R worfklow, eg: 

* [List of dimensions / codelists](https://docs.google.com/spreadsheets/d/1AGqXnPJvkqUzyhAxLLQ2xH36zfiLLpW6MOPbPlYlyis/edit?usp=sharing)
* [List of mapping between dimensions](https://docs.google.com/spreadsheets/d/1rMQn3JVvSyeTFkDTe4YN6MiT92IaYqJ9rPJioFKCeN8/edit?usp=sharing)
* [List or tuna RFMOs datasets](https://docs.google.com/spreadsheets/d/1D42xjUKT2ZFkAdbTAoBHXEwaRiUSlmOCvL6Orga8PAo/edit?usp=sharing)
* [Global datasets to be created from RFMOs datasets](https://docs.google.com/spreadsheets/d/1TzaQM-lI6SB1ZaF0hrQtaurYcCa7OBsDPLoRks25K3M/edit?usp=sharing)

This data structure complies with Dublin Core Metadata Initiative and adopt syntactic rules for valuation in order to enrich the core metadata elements. By doing so, we can automate the creation of FAIR services to foster data discovery, access and reuse.

## Infrastructures, interoperability and FAIR services
The model of the data warehouse, which includes metadata tables, is meant to deliver FAIR data services to make its content widely used by relying on standards to foster interoperability:

* Discovery service: data are findable by using well known standards:
    - Dublin Core Metadata Initiative (DCMI)
    - OGC standards: CSW access protocol and 19115, 19110, 19139 formats.
* Access service: data can be accessed in different ways:
    - SQL access 
    - OGC standard access protocols :  WMS, WFS
* Citation service with DOIs and related datacite schema

These services fit the needs of different communities which access can find and access data by using different protocols (http, GIS, SQL..).

## Software components

### Servers

The underlying spatial data infrastructure (SDI) of the Tuna Atlas relies on following components:

* RDBMS complying with OGC SFS standard: postgres & postgis implementing : Postgres & Postgis  in this case.
* Metadata management system complying with 19139 and CSW OGC standards: geonetwork in this case. 
* Spatial Data Server complying with OGC standard data access proctocols (WMS & WFS): geoserver in this case.

The orchestration of this SDI is fully operated in R with geoflow package.

### Clients

Interoperabiliy enables multiple users to access the data with a lot of clients (eg QGIS for GIS community) and multiple programming languages....

# Bibliography & Links


<!-- https://docs.oracle.com/cd/B13789_01/olap.101/b10333/multimodel.htm -->
Web:

* Tuna Atlas workflow [github repository](https://github.com/eblondel/geoflow-tunaatlas/tree/master/tunaatlas_sql)

Reports / data papers: 

* [Open data and common toolbox for tuna fisheries](http://data.d4science.org/alVxZ3dENGtYWmpGTWJNNjlxVDltdFd5T3haYXZZbFBHbWJQNStIS0N6Yz0)
* IOTC [Taconet Paul, Chassot Emmanuel, Guitton J., Palma C., Foirellato F., Anello E., Barde Julien. (2017). Global database and common toolbox for tuna fisheries. Madrid : ICCAT, 73 (9 (SCRS/2016/202)), 3327-3337. (Recueil de Documents Scientifiques - ICCAT ; 9 (SCRS/2016/202)). ISSN 1021-5212 ](http://www.documentation.ird.fr/hor/fdi:010071520)
* ICCAT[Taconet Paul, Chassot Emmanuel, Blondel E., Barde Julien. (2017). Global datasets for tuna fisheries. Victoria Mahé : CTOI, (IOTC-2017-WPDCS13-32_Rev1), 10 p. multigr. (Documents de Réunion - CTOI ; IOTC-2017-WPDCS13-32_Rev1). Working Party on Data Collection and Statistics : WPDCS13, 13., Mahé (SEY), 2017/11/26-28. ](http://www.documentation.ird.fr/hor/fdi:010071471)

Biblio:

* [Agregation and data sharing](https://academic.oup.com/icesjms/article/76/6/1415/5480138?searchresult=1)


# Annex

## Feature catalog


It is also very easy to make tables with knitr's `kable` function:

<!-- Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot. -->

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<caption>A knitr kable.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> TableType </th>
   <th style="text-align:left;"> SchemaName </th>
   <th style="text-align:left;"> FeatureType </th>
   <th style="text-align:left;"> MemberCode </th>
   <th style="text-align:left;"> MemberType </th>
   <th style="text-align:left;"> Definition </th>
   <th style="text-align:left;"> attribute_valuetype </th>
   <th style="text-align:right;"> max_length </th>
   <th style="text-align:left;"> is_nullable </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> materialized view </td>
   <td style="text-align:left;"> fact_tables </td>
   <td style="text-align:left;width: 2cm; font-weight: bold;"> atlantic_catch_1deg_1m_ps_iccat_level0__byschool </td>
   <td style="text-align:left;"> catchtype </td>
   <td style="text-align:left;"> attribute </td>
   <td style="text-align:left;width: 5cm; background-color: yellow !important;"> Fate of the catch, i.e. landed, discarded, unknown. Given the nature of the data, only landing data are currently available in SARDARA, with a very few exceptions of discarded fishes for ICCAT. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table. </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> materialized view </td>
   <td style="text-align:left;"> fact_tables </td>
   <td style="text-align:left;width: 2cm; font-weight: bold;"> atlantic_catch_1deg_1m_ps_iccat_level0__byschool </td>
   <td style="text-align:left;"> catchtype_label </td>
   <td style="text-align:left;"> attribute </td>
   <td style="text-align:left;width: 5cm; background-color: yellow !important;"> catchtype_label. </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> materialized view </td>
   <td style="text-align:left;"> fact_tables </td>
   <td style="text-align:left;width: 2cm; font-weight: bold;"> atlantic_catch_1deg_1m_ps_iccat_level0__byschool </td>
   <td style="text-align:left;"> flag </td>
   <td style="text-align:left;"> attribute </td>
   <td style="text-align:left;width: 5cm; background-color: yellow !important;"> Flagging country of the fishing vessels. Flagging country of fishing vessel OR The flag State is the state under whose laws the fishing vessel is registered or licensed. It has responsibility under international law for controlling the fishing activities of a vessel, no matter where the vessel operates. Data are generally reported by country but some data can be reported at a sub-level, e.g. catch from Reunion Island longliners are reported under the REU flag and not FRA. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table. </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> materialized view </td>
   <td style="text-align:left;"> fact_tables </td>
   <td style="text-align:left;width: 2cm; font-weight: bold;"> atlantic_catch_1deg_1m_ps_iccat_level0__byschool </td>
   <td style="text-align:left;"> flag_label </td>
   <td style="text-align:left;"> attribute </td>
   <td style="text-align:left;width: 5cm; background-color: yellow !important;"> flag_label. </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>


<!-- rmarkdown::render("~/Bureau/CODES/geoflow-tunaatlas/doc/generate_feature_catalog.Rmd", clean = FALSE) -->
