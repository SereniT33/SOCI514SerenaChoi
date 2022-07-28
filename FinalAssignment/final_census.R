install.packages("cancensus")
install.packages("sf")
install.packages("geojsonsf")


library(cancensus)
library(sf)
library(geojsonsf)
library(tidyverse)
library(dplyr)
library(sf)
library(geojsonsf)

set_api_key("CensusMapper_330652d5ae9c2ccaafc7883a06fb3967", install=TRUE)

#finding CMA level number for Vancouver.
list_census_datasets()
list_census_regions('CA16') %>%
  filter(level =="CMA", name %in% "Vancouver")
# Vancouver is CMA 59933.

#finding vector # for education.
edu <- find_census_vectors("diploma", dataset="CA16", type="total", query_type="keyword")
#vectors to use include v_CA16_5096 (total), v_CA16_5099 (LH), v_CA16_5102 (H), v_CA16_5105 (PS)

#retrieving census tract level data for Vancouver
edu_ct <- get_census(dataset='CA16', regions=list(CMA='59933'),
                          vectors=c("v_CA16_5096","v_CA16_5099","v_CA16_5102", "v_CA16_5105"),
                          level = 'CT', geo_format="sf", use_cache = FALSE)


st_write(edu_ct, "edu_ct.shp")

#retrieving dissemination area level data for Vancouver
edu_da <- get_census(dataset='CA16', regions=list(CMA='59933'),
                     vectors=c("v_CA16_5096","v_CA16_5099","v_CA16_5102", "v_CA16_5105"),
                     level = 'DA', geo_format="sf", use_cache = FALSE)


st_write(edu_da, "edu_da.shp")

