install.packages("cancensus")
install.packages("sf")
install.packages("geojsonsf")
install.packages("xmlconvert")


library(cancensus)
library(sf)
library(geojsonsf)
library(tidyverse)
library(dplyr)
library(sf)
library(geojsonsf)
library(xmlconvert)

set_api_key("CensusMapper_330652d5ae9c2ccaafc7883a06fb3967", install=TRUE)

#finding CMA level number for Vancouver.
list_census_datasets()
list_census_regions('CA16') %>%
  filter(level =="CMA", name %in% "Vancouver")
# Vancouver is CMA 59933.

#However, it may be better to use the city level data.
# finding CSD level number for Vancouver == 5915022
list_census_datasets()
list_census_regions('CA16') %>%
  filter(level =="CSD", name %in% "Vancouver")


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


#finding vector # for population, land area, and population density.
find_census_vectors("Population", dataset="CA16", type="total", query_type="keyword")
#Total population 2016 == v_CA16_401
#Population density per sq. km == v_CA16_406
#Land area in sq. km == v_CA16_407

#finding vector # for income.
find_census_vectors("Median total income", dataset="CA16", type="total", query_type="keyword")
#Median total income in 2015 among recipients ($) == v_CA16_2207

#finding vector # for visible minority.
find_census_vectors("visible minority", dataset="CA16", type="total", query_type="keyword")
#Total -  25% sample data == v_CA16_3954
#Total visible minority population == v_CA16_3957

#finding vector # for aboriginal identity.
find_census_vectors("Aboriginal identity", dataset="CA16", type="total", query_type="keyword")
#Total - 25% sample data == v_CA16_3852
# Aboriginal identity == v_CA16_3855

#finding vector # for age.
find_census_vectors('Average age', dataset="CA16", type="all", query_type="exact")
#Average age == v_CA16_379


#retrieving dissemination level data
census_data_da <- get_census(dataset='CA16', regions=list(CSD='5915022'),
                     vectors=c("v_CA16_5096","v_CA16_5099","v_CA16_5105",
                               "v_CA16_401","v_CA16_406", "v_CA16_407",
                               "v_CA16_2207",
                               "v_CA16_3954", "v_CA16_3957",
                               "v_CA16_3852", "v_CA16_3855",
                               "v_CA16_379"),
                     level = 'DA', use_cache = FALSE)

geo_census_data_da <- get_census(dataset='CA16', regions=list(CSD='5915022'),
                             vectors=c("v_CA16_5096","v_CA16_5099","v_CA16_5105",
                                       "v_CA16_401","v_CA16_406", "v_CA16_407",
                                       "v_CA16_2207",
                                       "v_CA16_3954", "v_CA16_3957",
                                       "v_CA16_3852", "v_CA16_3855",
                                       "v_CA16_379"),
                             level = 'DA',geo_format="sf", use_cache = FALSE)
geo_census_data_da <- geo_census_data_da %>%
  rename(total_edu = "v_CA16_5096: Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data") %>%
  rename(LH = "v_CA16_5099: No certificate, diploma or degree") %>%
  rename(PS = "v_CA16_5105: Postsecondary certificate, diploma or degree") %>%
  rename(POP = "v_CA16_401: Population, 2016") %>%
  rename(DENSITY = "v_CA16_406: Population density per square kilometre") %>%
  rename(AREA = "v_CA16_407: Land area in square kilometres") %>%
  rename(INCOME = "v_CA16_2207: Median total income in 2015 among recipients ($)") %>%
  rename(total_race = "v_CA16_3954: Total - Visible minority for the population in private households - 25% sample data") %>%
  rename(VM = "v_CA16_3957: Total visible minority population") %>%
  rename(ABO = "v_CA16_3855: Aboriginal identity") %>%
  rename(AGE = "v_CA16_379: Average age")

st_write(geo_census_data_da, "geo_census_data_da.shp")

#cleaning the dataset
census_data_da <- census_data_da %>%
  rename(total_edu = "v_CA16_5096: Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data") %>%
  rename(LH = "v_CA16_5099: No certificate, diploma or degree") %>%
  rename(PS = "v_CA16_5105: Postsecondary certificate, diploma or degree") %>%
  rename(POP = "v_CA16_401: Population, 2016") %>%
  rename(DENSITY = "v_CA16_406: Population density per square kilometre") %>%
  rename(AREA = "v_CA16_407: Land area in square kilometres") %>%
  rename(INCOME = "v_CA16_2207: Median total income in 2015 among recipients ($)") %>%
  rename(total_race = "v_CA16_3954: Total - Visible minority for the population in private households - 25% sample data") %>%
  rename(VM = "v_CA16_3957: Total visible minority population") %>%
  rename(ABO = "v_CA16_3855: Aboriginal identity") %>%
  rename(AGE = "v_CA16_379: Average age")

census_DA <- subset(census_data_da,
                    select = c(total_edu,LH, PS, POP, DENSITY, AREA, INCOME, total_race, VM, ABO, AGE))

park_per_DA <- xml_to_df("~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/Park_per_DA.csv.xml", records.tag = "datarecord", fields="attributes")
