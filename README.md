
# ECSprep

<!-- badges: start -->
[![R-CMD-check](https://github.com/asarmy/ECSprep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asarmy/ECSprep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ECSprep is to create CSV input files for the Lavrentiadis & 
Abrahamson Event Coordinate System (ECS) algorithm from a user's SHP files that 
contain rupture or fault trace line work, fault displacement measurement sites, 
or hazard analysis sites.

There are two common applications for the ECS algorithm:

1. Forward-application of fault displacement hazard analysis models, where the
user has mapped traces of active faults and site locations for hazard analysis.

2. Presentation and analysis of post-earthquake fault rupture and displacement
data, where the user has mapped traces of surface ruptures and locations where
fault displacement was measured.

## Installation

You can install the development version of ECSprep like so:

``` r
install.packages("remotes")
remotes::install_github("asarmy/ECSprep")
```

## Examples

This is a basic example which shows how to use the package:

``` r
library(ECSprep)
create_ecs_input_ruptures(file.path(path/to/my/rupture_traces.shp))
create_ecs_input_measurements(file.path(path/to/my/displ_meas_sites.shp))
create_ecs_input_haz_sites(file.path(path/to/my/hazard_study_sites.shp))
```

For more examples, see the Vignettes:

- [Creating ECS input files for Fault Displacement Hazard Analysis](https://github.com/asarmy/ECSprep/blob/master/vignettes/example-1.Rmd)
is an example geared toward forward-application of fault displacement hazard 
analysis models based on mapped traces of active faults and hazard evaluation
sites.

- [Creating ECS input files for presentation/analysis of data from a surface-rupturing earthquak](https://github.com/asarmy/ECSprep/blob/master/vignettes/example-2.Rmd)
is an example geared toward presentation and analysis of post-earthquake fault 
rupture and displacement data based on mapped traces of surface ruptures and 
locations where fault displacement was measured.

## "Gotchas"

- Only Esri shapefiles are accepted as inputs. If other file types are attempted,
then the user will be notified and the script will halt execution.

- The shapefiles must have an assigned Coordinate Reference System (CRS); in
other words, a PRJ file must be included.

- *Principal* and *Distributed* rankings for faults and measurements are not 
required but are recommended. The ECS algorithm performs better in complex
ruptures when these are specified. They should be containted in a column
called `Rank`.

- A column called `displ` is required for processing fault displacement
measurements. If it is not present, then the user will be notified and the 
script will halt execution.









