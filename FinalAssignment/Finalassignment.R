##################################################
# Final Assignment
#
# Green equity: does social vulnerability affect the distribution of green space? Case from Vancouver
#
##################################################
library(plyr)
library(tidyverse)
library(janitor)
park <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/park.csv")
local <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/local.csv", check.names=FALSE)
edu <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/Education_da.csv", check.names=FALSE)


## cleaning data frame
park <- subset(park, select = -OBJECTID)
#local <- subset(local, select = -ID)
local <- local %>%
  filter(Variable!='') %>%
  filter(ID %in% c(1, 27, 53, 1904:1919, 3080:3081)) %>%
  unite(variable, c("ID", "Variable"))
local$variable <- recode(local$variable,"1_Total - Age groups and average age of the population - 100% data" = "total_pop")
  local$variable <-recode(local$variable,"27_Total - Age groups and average age of males - 100% data" ="male")
  local$variable <-recode(local$variable,"53_Total - Age groups and average age of females - 100% data" = "female")
  local$variable <-recode(local$variable,"1904_Total - Total income groups in 2015 for the population aged 15 years and over in private households - 25% sample data" = "total_income_group")
  local$variable <-recode(local$variable,"1905_Without total income"="without_income")
  local$variable <-recode(local$variable,"1906_With total income"="with_income")
  local$variable <-recode(local$variable,"1907_    Under $10,000 (including loss)"="under_10k")
  local$variable <-recode(local$variable,"1908_    $10,000 to $19,999"="10_19k")
  local$variable <-recode(local$variable,"1909_    $20,000 to $29,999"="20_29k")
  local$variable <-recode(local$variable,"1910_    $30,000 to $39,999"="30_39k")
  local$variable <-recode(local$variable,"1911_    $40,000 to $49,999"="40_49k")
  local$variable <-recode(local$variable,"1912_    $50,000 to $59,999"="50_59k")
  local$variable <-recode(local$variable,"1913_    $60,000 to $69,999"="60_69k")
  local$variable <-recode(local$variable,"1914_    $70,000 to $79,999"="70_79k")
  local$variable <-recode(local$variable,"1915_    $80,000 to $89,999"="80_89k")
  local$variable <-recode(local$variable,"1916_    $90,000 to $99,999"="90_99k")
  local$variable <-recode(local$variable,"1917_    $100,000 and over"="over_100k")
  local$variable <-recode(local$variable,"1918_      $100,000 to $149,999"="100_140k")
  local$variable <-recode(local$variable,"1919_      $150,000 and over"="over_150k")
  local$variable <-recode(local$variable,"3080_Total - Visible minority for the population in private households - 25% sample data" ="total_race")
  local$variable <-recode(local$variable,"3081_Total visible minority population"="race")

local <- local %>%
  pivot_longer(!variable)%>%
  pivot_wider(names_from = variable, values_from = value)

colnames(local)

## joining
park_local <- join(park,local, by="name")

## cleaning dataframe (2) education
colnames(edu)
education <- edu %>%
  dplyr::rename(total_edu = vT_H2,
                LH = v_Ncd,
                H =vS_sd,
                PS =v_Pcd)
education <- subset(education, select = -c(OBJECTID, Join_Count, TARGET_FID, GeUID,
                                           Type, CD_UI, CSD_U, CT_UI, CMA_U,
                                          Shape_Area, Shape_Length,
                                           RgnNm,ShpAr))
edu_sum <- education %>%
  group_by(name) %>%
  summarise(
    Ppltn = sum(Ppltn),
    Hshld = sum(Hshld),
    Dwlln = sum(Dwlln),
    total_edu = sum(total_edu),
    LH = sum(LH),
    H = sum(H),
    PS = sum(PS),
    Area = sum(Ar_s_))

#join datasets (2)
local_profiles <- join(park_local,edu_sum, by="name")

## creating and recoding columns, park_service and normalizing other variables
local_profiles$total_pop <- gsub(",","", local_profiles$total_pop)
local_profiles <- local_profiles %>%
  mutate(park_service = 1000 * SUM_area_ha /as.numeric(total_pop)) %>%
  mutate(MALE = as.numeric(male) / as.numeric(total_pop) * 100) %>%
  mutate(FEMALE = as.numeric(female) / as.numeric(total_pop) * 100) %>%
  mutate(INCOME_WOI = as.numeric(without_income) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_WI = as.numeric(with_income) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L10K = as.numeric(under_10k) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L20K = as.numeric(`10_19k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L30K = as.numeric(`20_29k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L40K = as.numeric(`30_39k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L50K = as.numeric(`40_49k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L60K = as.numeric(`50_59k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L70K = as.numeric(`60_69k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L80K = as.numeric(`70_79k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L90K = as.numeric(`80_89k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L100K = as.numeric(`90_99k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_H100K = as.numeric(over_100k) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_L150K = as.numeric(`100_140k`) / as.numeric(total_income_group) * 100) %>%
  mutate(INCOME_H150K = as.numeric(over_150k) / as.numeric(total_income_group) * 100) %>%
  mutate(RACE = as.numeric(race) / as.numeric(total_race) * 100) %>%
  mutate(EDU_LH = as.numeric(LH) / as.numeric(total_edu) * 100) %>%
  mutate(EDU_H = as.numeric(H) / as.numeric(total_edu) * 100) %>%
  mutate(EDU_PS = as.numeric(PS) / as.numeric(total_edu) * 100) %>%
  mutate(DENSITY = as.numeric(total_pop) / as.numeric(Area))

## Research questions


## Building models
#treating education & income as categorical variables with
#EDUC_H and INCOME_L70K as referent. Included FEMALE.
model_all_variables <-
  lm(park_service ~ EDU_LH + EDU_PS +
       INCOME_L10K + INCOME_L20K + INCOME_L30K + INCOME_L40K +
       INCOME_L50K + INCOME_L60K + INCOME_L80K + INCOME_L90K + INCOME_L100K +
       INCOME_L150K + INCOME_H150K + RACE + FEMALE + DENSITY,
     data = local_profiles)
summary(model_all_variables)

model_all_variables_interaction <-
  lm(park_service ~ EDU_LH + EDU_PS +
       INCOME_L10K + INCOME_L20K + INCOME_L30K + INCOME_L40K +
       INCOME_L50K + INCOME_L60K + INCOME_L80K + INCOME_L90K + INCOME_L100K +
       INCOME_L150K + INCOME_H150K + RACE + FEMALE + DENSITY +
       (EDU_LH : INCOME_L10K) + (EDU_LH : INCOME_L20K) + (EDU_LH : INCOME_L30K) +
       # (EDU_LH : INCOME_L40K) + (EDU_LH : INCOME_L50K) + (EDU_LH : INCOME_L60K) +
       # (EDU_LH : INCOME_L70K) +(EDU_LH : INCOME_L80K) + (EDU_LH : INCOME_L90K) +
      # (EDU_LH : INCOME_L100K) + (EDU_LH : INCOME_L150K) + (EDU_LH : INCOME_H150K) +
        # (EDU_PS : INCOME_L10K) + (EDU_PS : INCOME_L20K) + (EDU_PS : INCOME_L30K) +
        # (EDU_PS : INCOME_L40K) + (EDU_PS : INCOME_L50K) + (EDU_PS : INCOME_L60K) +
        # (EDU_PS : INCOME_L70K) +(EDU_PS : INCOME_L80K) + (EDU_PS : INCOME_L90K) +
       # (EDU_PS : INCOME_L100K) + (EDU_PS : INCOME_L150K) + (EDU_PS : INCOME_H150K) ,
        # (RACE : INCOME_L10K) + (RACE : INCOME_L20K) + (RACE : INCOME_L30K) ,
        #(RACE : INCOME_L40K) + (RACE : INCOME_L50K) + (RACE : INCOME_L60K) ,
        #(RACE : INCOME_L70K) +(RACE : INCOME_L80K) + (RACE : INCOME_L90K) ,
       # (RACE : INCOME_L100K) + (RACE : INCOME_L150K) + (RACE : INCOME_H150K) ,
       # (FEMALE : INCOME_L10K) + (FEMALE : INCOME_L20K) + (FEMALE : INCOME_L30K) ,
        # (FEMALE : INCOME_L40K) + (FEMALE : INCOME_L50K) + (FEMALE : INCOME_L60K) ,
        # (FEMALE : INCOME_L70K) +(FEMALE : INCOME_L80K) + (FEMALE : INCOME_L90K) ,
        # (FEMALE : INCOME_L100K) + (FEMALE : INCOME_L150K) + (FEMALE : INCOME_H150K) ,
       (RACE : FEMALE) ,
       # (EDU_LH : FEMALE) ,
        # (EDU_PS : FEMALE) ,
       # (EDU_LH : RACE) ,
      # (EDU_PS : RACE) ,
       # (DENSITY : INCOME_L10K) + (DENSITY : INCOME_L20K) + (DENSITY : INCOME_L30K) ,
        # (DENSITY : INCOME_L40K) + (DENSITY : INCOME_L50K) + (DENSITY : INCOME_L60K) ,
       # (DENSITY : INCOME_L70K) +(DENSITY : INCOME_L80K) + (DENSITY : INCOME_L90K) ,
        #(DENSITY : INCOME_L100K) + (DENSITY : INCOME_L150K) + (DENSITY : INCOME_H150K),
     data = local_profiles)
summary(model_all_variables_interaction)

### Write this up - you learned than normalizing your data reduced the vif
car::vif(lm(park_service ~ as.numeric(LH) + as.numeric(PS) +
              as.numeric(under_10k) + as.numeric(`10_19k`) + as.numeric(`20_29k`) +
              as.numeric(`30_39k`) + as.numeric(`40_49k`) + as.numeric(`50_59k`) +
              as.numeric(`70_79k`) + as.numeric(`80_89k`) + as.numeric(`90_99k`) +
              as.numeric(`100_140k`) + as.numeric(over_150k) +
              as.numeric(female) + as.numeric(DENSITY),
            data = local_profiles))

car::vif(lm(park_service ~ as.numeric(LH) + as.numeric(PS) +
              as.numeric(under_10k) + as.numeric(`10_19k`) + as.numeric(`20_29k`) +
              as.numeric(`30_39k`) + as.numeric(`40_49k`) + as.numeric(`50_59k`) +
              as.numeric(`70_79k`) + as.numeric(`80_89k`) + as.numeric(`90_99k`) +
              as.numeric(`100_140k`) + as.numeric(over_150k) +
              as.numeric(female) + as.numeric(DENSITY),
            data = local_profiles))

#what if I use non-normalized variables?
model_all_variables_normalized <-
  lm(park_service ~ as.numeric(LH) + as.numeric(PS) +
       as.numeric(under_10k) + as.numeric(`10_19k`) + as.numeric(`20_29k`) +
       as.numeric(`30_39k`) + as.numeric(`40_49k`) + as.numeric(`50_59k`) +
       as.numeric(`70_79k`) + as.numeric(`80_89k`) + as.numeric(`90_99k`) +
       as.numeric(`100_140k`) + as.numeric(over_150k) +
       as.numeric(female) + as.numeric(DENSITY),
     data = local_profiles)
summary(model_all_variables_normalized)


# ###

subset_local_profiles <- subset(local_profiles, select = c(park_service, EDU_LH, EDU_PS, INCOME_L10K,
                                                           INCOME_L20K, INCOME_L30K, INCOME_L40K, INCOME_L50K,
                                                           INCOME_L60K, INCOME_L80K, INCOME_L90K, INCOME_L100K,
                                                           INCOME_L150K, INCOME_H150K, RACE, FEMALE, DENSITY))
plot(cor(subset_local_profiles))

