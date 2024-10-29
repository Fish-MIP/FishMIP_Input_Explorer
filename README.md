[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13902156.svg)](https://doi.org/10.5281/zenodo.13902156)


# FishMIP Input Explorer for Regional Ecosystem Models

## Background
The main goal of the Fisheries and Marine Ecosystem Model Intercomparison Project ([FishMIP](https://fishmip.org/)) is to better understand and project the long-term impacts of climate change on fisheries and marine ecosystems. It is expected the findings of this group will inform the design of policies to conserve or sustainably manage marine resources.  
  
To achieve this objective, FishMIP has designed model evaluation protocols, including [Protocol 3a](https://github.com/Fish-MIP/FishMIP2.0_TrackA_ISIMIP3a), which aims to understand and reduce uncertainty associated with ecosystem models through model evaluation under historical climate and fishing effort forcings. Protocol 3a was originally developed for global ecosystem models. However, due to the increasing number of regional models, Protocol 3a has been adapted to meet the unique needs of regional modelers. The publication describing the regional adaption is being led by [Kelly Ortega-Cisneros](https://orcid.org/0000-0003-2511-5448) and its title is **An Integrated Global-to-Regional Scale Workflow for Simulating Climate Change Impacts on Marine Ecosystems**. This work is currently under review, but it is available as a [pre-print](http://dx.doi.org/10.22541/essoar.171587234.44707846/v1).  


## Shiny app
This repository contains all code developed to produce the shiny app supporting this publication. The shiny app is designed to allow regional modelers to explore all environmental outputs available in the GFDL-MOM6-COBALT2 model used to forced ecosystem models under Protocol 3a. 

The [FishMIP Input Explorer Shiny app](https://rstudio.global-ecosystem-model.cloud.edu.au/shiny/FishMIP_Input_Explorer/) is under continuous development, but it is ready for public use. Users can select the FishMIP regional model and environmental variable of interest to see a map and time series of the climatological mean calculated from GFDL-MOM6-COBALT2 model outputs (1961-2010) and World Ocean Atlas 2023 (WOA 2023) data (1981-2010). Users can also see the difference between these two products over the common period (1981-2010). Finally, the **Download** button allows users to download the original GFDL-MOM6-COBALT2 model outputs and WOA 2023 data cropped within the boundaries of the regional model of interest. While the **Download** button in the comparison tab gives users access to monthly climatologies calculated from GFDL-MOM6-COBALT2 and WOA data  for the same period (1981-2010) and using the same grid, which can be used to perform bias correction if needed.  
  
## Do you have suggestions or comments?
If you have any suggestions on how to improve this app or if you spotted something that is not quite right, you can [create an issue](https://github.com/Fish-MIP/FishMIP_Input_Explorer/issues) in this repository. Provide as much detail as possible so we can address your request easily.  
  
## Do you want to contribute?
If you would like to contribute to the development of this app, you can submit a Pull Request with your suggested changes. 