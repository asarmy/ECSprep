
# ECSprep

<!-- badges: start -->
[![R-CMD-check](https://github.com/asarmy/ECSprep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asarmy/ECSprep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ECSprep is to create CSV input files for the Lavrentiadis & Abrahamson 
Event Cooredinate System (ECS) algorithm from a user's SHP files that contain 
rupture or fault trace line work and/or fault displacement measurement sites.

There are two common applications for the ECS algorithm:

1. Forward-application of fault displacement hazard analysis models, where the
user has mapped traces of active faults.

2. Presentation and analysis of post-earthquake fault rupture and displacement
data, where the user has mapped traces of surface ruptures and sites where
fault displacement was measured.

## Installation

You can install the development version of ECSprep like so:

``` r
install.packages("remotes")
remotes::install_github("asarmy/ECSprep")
```

## User Overview

### Here is a basic workflow for developing the CSV for the fault traces/ruptures:

- User prepares a shapefile (SHP) containing mapped traces for the faults (or surface ruptures). 

  - It is recommended that the user creates a `Rank` attribute and assigns 
  "Principal" or "Distributed" classifications to the faults/ruptures. The
  algorithm generally will perform better if these are specified, especially
  when the fault system or surface rupture patterns are complex.
  
  - It does not matter which Coordinate Reference System (CRS) is used, but the
  CRS must be assigned. In other words, a PRJ file must be present with the
  SHP file suite.

- User runs `ECSprep::create_ecs_input` with the fault trace SHP as the input.
The script will produce a CSV that creates ordered vertices, with latitude
and longitude coordinates, for each fault trace/rupture. 
  
  - If the CRS is not assigned, an error occurs.

  - If a `Rank` column is not present, a warning is issued and it is 
  automatically added with default values of "None."

  - Other attributes that are present in the input SHP are retained in the CSV.
  
  - The CSV output defaults to the same directory as the input SHP, but the
  user can optionally specify an alternative path.
  
### Here is a basic workflow for developing the CSV for the fault displacement masurement sites:

TODO


## "Gotchas"

TODO

## Examples

This is a basic example which shows how to use the package:

``` r
library(ECSprep)
create_ecs_input(file.path(path/to/my/rupture_traces.shp), "line")
create_ecs_input(file.path(path/to/my/measurement_sites.shp), "point")
```

For more examples, see the Vignettes:

- [Creating ECS input files for Fault Displacement Hazard Analysis](https://github.com/asarmy/ECSprep/blob/main/vignettes/example-1.Rmd)
is an example showing how to create the ruptures input file from a standard 
Esri shapefile.


