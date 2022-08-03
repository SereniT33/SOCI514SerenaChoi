##################################################
# Final Assignment
#
# Green equity: does social vulnerability affect the distribution of green space? Case from Vancouver
#
##################################################
library(plyr)
library(tidyverse)
library(janitor)
library(car)
park <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/park.csv")
local <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/local.csv", check.names=FALSE)
edu <- read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/Education_da.csv", check.names=FALSE)
DA_park <-read.csv(file="~/Desktop/MCRP Terms/Y2022Winter/Soci514/SOCI514SerenaChoi/FinalAssignment/DA_park.csv", check.names=FALSE)

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



# from using neighborhood level data, I realized that the collinearity happens due to too few observations.
#Hence, I looked at data divided by a smaller geographical unit, dissemination area.
#Here's the new dataset, "DA_park", which shows census data and park distribution per dissemination area.

DA_park_census <- subset(DA_park,
                         select = c("LH", "PS", "POP", "DENSI", "AREA", "INCOM",
                                    "ttl_r", "VM", "ABO", "AGE", "SUM_Area_SQUAREKILOMETERS"))

summary(DA_park_census)



#BIVARIATE MODELS
##1.
bivariate_LH <- lm(SUM_Area_SQUAREKILOMETERS ~ LH,
                     data = DA_park_census)
summary(bivariate_LH)
plot_bivariate_LH <- ggplot(DA_park_census,
                            aes(x = LH, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_LH)

plot(DA_park_census$LH, DA_park_census$SUM_Area_SQUAREKILOMETERS,
     pch = 16, cex = 1.3, col = "blue",
     main = "PARK PLOTTED AGAINST LH", xlab = "LH (people)", ylab = "Park (sq.km)")
abline(lm(SUM_Area_SQUAREKILOMETERS ~ LH,
          data = DA_park_census))
###LH direction: negative; not significant; Adj. R is negative so terrible fit.

##2.
bivariate_PS <- lm(SUM_Area_SQUAREKILOMETERS ~ PS,
                   data = DA_park_census)
summary(bivariate_PS)
plot_bivariate_PS <- ggplot(DA_park_census,
                            aes(x = PS, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_PS)

plot(DA_park_census$PS, DA_park_census$SUM_Area_SQUAREKILOMETERS,
     pch = 16, cex = 1.3, col = "blue",
     main = "PARK PLOTTED AGAINST PS", xlab = "PS (people)", ylab = "Park (sq.km)")
abline(lm(SUM_Area_SQUAREKILOMETERS ~ PS,
          data = DA_park_census))
### PS direction: positive; marginally significant at 0.1 level (=-.111);
### Adj. R is 0.002; still a bad fit.

##3.
bivariate_INCOM <- lm(SUM_Area_SQUAREKILOMETERS ~ INCOM,
                   data = DA_park_census)
summary(bivariate_INCOM)
plot_bivariate_INCOM <- ggplot(DA_park_census,
                            aes(x = INCOM, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_INCOM)

plot(DA_park_census$INCOM, DA_park_census$SUM_Area_SQUAREKILOMETERS,
     pch = 16, cex = 1.3, col = "blue",
     main = "PARK PLOTTED AGAINST INCOM", xlab = "INCOM ($)", ylab = "Park (sq.km)")
abline(lm(SUM_Area_SQUAREKILOMETERS ~ INCOM,
          data = DA_park_census))
### INCOM direction: positive; not significant; negative Adj. R = terrible

##4.
bivariate_VM <- lm(SUM_Area_SQUAREKILOMETERS ~ VM,
                      data = DA_park_census)
summary(bivariate_VM)
plot_bivariate_VM <- ggplot(DA_park_census,
                               aes(x = VM, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_VM)
plot(DA_park_census$VM, DA_park_census$SUM_Area_SQUAREKILOMETERS,
     pch = 16, cex = 1.3, col = "blue",
     main = "PARK PLOTTED AGAINST VM", xlab = "VM (persons)", ylab = "Park (sq.km)")
abline(lm(SUM_Area_SQUAREKILOMETERS ~ VM,
          data = DA_park_census))
### VM direction: positive; not significant; negative Adj. R = terrible

##5.
bivariate_ABO <- lm(SUM_Area_SQUAREKILOMETERS ~ ABO,
                   data = DA_park_census)
summary(bivariate_ABO)
plot_bivariate_ABO <- ggplot(DA_park_census,
                            aes(x = ABO, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_ABO)
### ABO direction: positive (close to 0); not significant; negative Adj. R = terrible

##6.
bivariate_AGE <- lm(SUM_Area_SQUAREKILOMETERS ~ AGE,
                    data = DA_park_census)
summary(bivariate_AGE)
plot_bivariate_AGE <- ggplot(DA_park_census,
                             aes(x = AGE, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_AGE)
### AGE direction: positive (close to 0); SIGNIFICANT; Adj. R = 0.004 ~ bad

##6.
bivariate_DENSI <- lm(SUM_Area_SQUAREKILOMETERS ~ DENSI,
                    data = DA_park_census)
summary(bivariate_DENSI)
plot_bivariate_DENSI<- ggplot(DA_park_census,
                             aes(x = DENSI, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_bivariate_DENSI)
### DENSI direction: negative; SIGNIFICANT; Adj. R = 0.004 ~ bad



#buidling simple linear model
Model_1 <- lm(SUM_Area_SQUAREKILOMETERS ~ LH + PS + AGE + DENSI,
                     data = DA_park_census)
summary(Model_1)
#

#Interpretation (1) the coefficients: - for LH and DENSI, + for PS and AGE
#(2) AGE is statistically significant at 0.05 level; PS & density are significant at 0.01 level.
#(3) Adjusted R-squared is very low; model is not a good fit.


Model_2 <- lm(SUM_Area_SQUAREKILOMETERS ~ LH + PS + INCOM + AGE + DENSI,
                            data = DA_park_census)
summary(Model_2)
#Interpretation
#(1) coefficients: - for LH and DENSI; + for PS, INCOM, AGE
#(2) PS and AGE statistically significant at 0.05 level; density at 0.01 level. Income is not significant.
#(3) Adjusted R squared is worse!!

plot_INCOM_park <- ggplot(DA_park_census, aes(x = INCOM, y = SUM_Area_SQUAREKILOMETERS)) +
  geom_point()
print(plot_INCOM_park)


Model_3 <- lm(SUM_Area_SQUAREKILOMETERS ~ LH + PS + INCOM + VM + AGE + DENSI,
     data = DA_park_census)
summary(Model_3)
#Interpretation
#(1) coefficients are positive for LH, PS, AGE; negative for INCOM, DENSI
#LH & income coefficient changed the direction, it does not make sense.
#(2) PS and AGE are statistically significant at 0.05 level; density at 0.01 level. Others are not.
#(3) Adjusted R squared (0.0125) is a little better than model 2, but worse than model1.


Model_4 <- lm(SUM_Area_SQUAREKILOMETERS ~ LH + PS + INCOM  + VM + ABO + AGE + DENSI,
                    data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_4)
#Interpretation
#(1)coefficients are negative for LH, income, VM and DENSI; positive for PS, ABo and AGE
#direction for LH changed back to - => makes sense
#direction for income stays - => why?
#direction for ABO is +
#(2)significance stays the same.
#(3)Adjusted R squared is worse.

plot_VM_INCOM <- ggplot(DA_park_census, aes(x = VM, y = INCOM)) +
  geom_point()
print(plot_VM_INCOM)

#incremental F test
anova(Model_1, Model_2)
# -> fail to reject the null hypothesis;
# adding INCOM does not significantly improve the fit of the model.

anova(Model_2, Model_3)
# fail to reject; adding VM does not improve

anova(Model_3, Model_4)
# fail to reject; adding VM does not improve



#INTERACTION EFFECTS
Model_5 <- lm(SUM_Area_SQUAREKILOMETERS ~
                LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                LH:PS,
              data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_5)
#Interpretation
#(1)coefficients: positive for LH, PS, ABO, AGE;
#negative for INCOM, VM, DENSI, LH:PS
#LH changed to +; why?
#(2)LH:PS significant at 0.05; AGE at 0.01; PS and DENSI at 0.001
#(3)Adj. R is much better (0.01729)

anova(Model_4, Model_5)
#reject the null hypothesis; adding LH:PS improves the model fit!

Model_6 <- lm(SUM_Area_SQUAREKILOMETERS ~
                LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                LH:PS + LH:INCOM,
              data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_6)
#Interpretation
#(1)coefficients: positive for PS, ABO, AGE, LH:INCOM;
#negative for LH, INCOM, VM, DENSI, LH:PS
#LH is back to - (yahoo!)
#(2)PS, AGE and LH:PS significant at 0.01; DENSI at 0.001
#(3)Adj. R is little worse (0.01701)

anova(Model_5, Model_6)
#cannot reject the null hypothesis;
#adding LH:INCOM does not improves the model fit



Model_7 <- lm(SUM_Area_SQUAREKILOMETERS ~
                LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                LH:PS +
                PS:INCOM,
              data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_7)
#Interpretation
#(1) coefficients + for LH, PS, INCOM, ABO, AGE;
# - for VM, DENSI, LH:PS, PS:INCOM
#(2) LH:PS and PS:INCOM statistically significant at 0.1 level;
#PS and AGE at 0.05 level; DENSI at 0.01 level
#(3) Adj. R = 0.019; better!

anova(Model_5, Model_7)
#marginally significant at 0.1 level
#may reject the null and therefore, adding PS:INCOM




#Model_8 <- lm(SUM_Area_SQUAREKILOMETERS ~
                #LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                #LH:PS +  PS:INCOM,
              #data = DA_park_census, y = TRUE, x=TRUE)
#summary(Model_8)
#Interpretation
#(1) coefficients + for LH, PS, INCOM, ABO, AGE;
# - for VM, DENSI, LH:PS, PS:INCOM
#(2) LH:PS and PS:INCOM statistically significant at 0.1 level;
#PS and AGE at 0.05 level; DENSI at 0.01 level
#(3) Adj. R = 0.019; better!
#anova(Model_5, Model_8)
#marginally significant at 0.1 level;therefore, adding PS:INCOM works
#==> model 7 and model 8 is the same

Model_9 <- lm(SUM_Area_SQUAREKILOMETERS ~
                LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                LH:PS +  PS:INCOM + VM : INCOM,
              data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_9)
#Interpretation


anova(Model_8, Model_9)
#cannot reject the null; adding VM:INCOM x


Model_10 <- lm(SUM_Area_SQUAREKILOMETERS ~
                LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                LH:PS +  PS:INCOM + ABO : INCOM,
              data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_10)

anova(Model_8, Model_10)
#cannot reject the null; adding ABO : INCOM x


Model_11 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + AGE : INCOM,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_11)

anova(Model_8, Model_11)
#cannot reject the null; adding AGE : INCOM x


Model_12 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + DENSI : INCOM,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_12)

anova(Model_8, Model_12)
#cannot reject the null; adding DENSI : INCOM x


Model_13 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + LH : VM,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_13)

anova(Model_8, Model_13)
#cannot reject the null; adding LH : VM x


Model_14 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : VM,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_14)

anova(Model_8, Model_14)
#cannot reject the null; adding PS : VM x


Model_15 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + LH : ABO,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_15)

anova(Model_8, Model_15)
#cannot reject the null; adding LH : ABO x


Model_16 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : ABO,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_16)

anova(Model_8, Model_16)
#cannot reject the null; adding PS : ABO x



Model_17 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + LH : AGE,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_17)

anova(Model_8, Model_17)
#cannot reject the null; adding LH : AGE x


Model_18 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_18)

anova(Model_8, Model_18)
#we can reject the null as it is statistically significant at 0.001;
#adding PS : AGE works!


Model_19 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + VM : ABO,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_19)

anova(Model_18, Model_19)
#cannot reject the null; VM:ABO -> x



Model_20 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + VM : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_20)

anova(Model_18, Model_20)
#cannot reject the null; VM:DENSI -> x


Model_21 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + ABO : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_21)

anova(Model_18, Model_21)
#cannot reject the null; ABO : DENSI -> x


Model_22 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + LH : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_22)

anova(Model_18, Model_22)
#cannot reject the null; LH : DENSI -> x


Model_23 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + PS : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_23)

anova(Model_18, Model_23)
#cannot reject the null; PS : DENSI -> x


Model_24 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + AGE : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_24)
#Interpretation
#(1)coefficients + for LH, AGE, DENSI, PS:AGE
# - for PS, INCOM, VM, ABO, LH:PS, PS:INCOM, and AGE:DENSI
#weird directions: LH, PS, INCOM, DENSI
#(2) VM significant at 0.1 level; PS at 0.05;
#DENSI, PS:AGE, AGE:DENSI at 0.001 level
#(3)Adj. R = 0.04579

anova(Model_18, Model_24)
#can reject the null; significant at 0.001
#Adding AGE : DENSI


Model_25 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + AGE : DENSI +
                 VM : AGE,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_25)
#Interpretation
#(1)coefficients + for LH, VM, AGE, DENSI, PS:AGE
# - for PS, INCOM, ABO, LH:PS, PS:INCOM, AGE:DENSI, VM:AGE
#weird directions: LH, PS, INCOM,VM, DENSI
#(2) VM, AGE at 0.05; VM:AGE at 0.01
#PS, DENSI, PS:AGE, AGE:DENSI at 0.1
#(3)Adj. R = 0.05233

anova(Model_24, Model_25)
#can reject the null; significant at 0.05
#add VM:AGE


Model_26 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + INCOM  + VM + ABO + AGE + DENSI +
                 LH:PS +  PS:INCOM + PS : AGE + AGE : DENSI +
                 VM : AGE + ABO : AGE,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_26)

anova(Model_25, Model_26)
#cannot reject the null; ABO : AGE -> x

# Then is the model 25 is optimal?


#collinearity?
vif(Model_25)
# high collinearity for PS, VM, DENSI, and all interaction variables
# -> could be a problem for LS estimates
#In that case, what if we drop some IVs that have not been statistically
#significant and maybe causing the problems?
#Suppose that we are dropping INCOM and ABO, and thereby ABO:AGE
Model_27 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + VM + AGE + DENSI +
                 LH:PS  + PS : AGE + AGE : DENSI +
                 VM : AGE,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_27)

anova(Model_27, Model_25)
#cannot refuse the null. That is, keeping independent variables,
#INCOM, ABO, ABO:AGE, does not improve the model fit.

#model 28 further drops VM and VM:AGE
Model_28 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + PS + AGE + DENSI +
                 LH:PS  + PS : AGE + AGE : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_28)

anova(Model_28, Model_27)
#statistically significant; should keep VM and VM:AGE

#what about dropping PS related variables while keeping VM?
Model_29 <- lm(SUM_Area_SQUAREKILOMETERS ~
                 LH + VM + AGE + DENSI +
                 VM:AGE + AGE : DENSI,
               data = DA_park_census, y = TRUE, x=TRUE)
summary(Model_29)

anova(Model_29, Model_27)
#statistically significant; should keep PS and related variables.

##Checking model assumptions
###1. model muist be correctly specified: direction of causality; linear; no omitted variables
###2. errors are indipendently normally distributed.
stand.res <- stdres(Model_4)  # standardized residuals
qqnorm((stand.res),
       ylab="Standardized Residuals",
       xlab="Theoretical Quantiles",
       main="QQ plot of Standardized Residuals")
qqline((stand.res), col="magenta")


###3. the mean of the errors is 0.
mean(stand.res)
plot(stand.res)
abline(h=mean(stand.res), col="red")
abline(h=0, col="blue")
####it is very close to zero. This assumption is not violated and
####the intercept is not biased.

###4. the variance of the errors is constant across cases.
plot(fitted(Model_4), stand.res[1:length(fitted(Model_4))],
     xlab="Fitted Values of y",
     ylab="Standardized Residuals",
     main="Std. Residuals vs. Fitted Values for the model")
abline(h=0, col="red")
libary(lmtest)
bptest(Model_4)
#### As p-value is large, we cannot reject the null hypothesis.
####Therefore, we can conclude that the variance of the errors
####is constant across cases, and the assumption is not violated.

###5. the error is unrelated to all of the independent variables.
plot_LH_str <- plot(DA_park_census$LH, stand.res[1:length(DA_park_census$LH)],
     xlab="LH",
     ylab="Standardized Residuals",
     main="Std. Residuals vs. LH")
abline(h=0, col="red")

plot_PS_str <- plot(DA_park_census$PS, stand.res[1:length(DA_park_census$PS)],
                    xlab="PS",
                    ylab="Standardized Residuals",
                    main="Std. Residuals vs. PS")
abline(h=0, col="red")

plot_INCOM_str <- plot(DA_park_census$INCOM, stand.res[1:length(DA_park_census$INCOM)],
                    xlab="INCOM",
                    ylab="Standardized Residuals",
                    main="Std. Residuals vs. INCOM")
abline(h=0, col="red")


plot_VM_str <- plot(DA_park_census$VM, stand.res[1:length(DA_park_census$VM)],
                       xlab="VM",
                       ylab="Standardized Residuals",
                       main="Std. Residuals vs. VM")
abline(h=0, col="red")


plot_ABO_str <- plot(DA_park_census$ABO, stand.res[1:length(DA_park_census$ABO)],
                       xlab="ABO",
                       ylab="Standardized Residuals",
                       main="Std. Residuals vs. ABO")
abline(h=0, col="red")


plot_DENSI_str <- plot(DA_park_census$DENSI, stand.res[1:length(DA_park_census$DENSI)],
                     xlab="DENSI",
                     ylab="Standardized Residuals",
                     main="Std. Residuals vs. DENSI")
abline(h=0, col="red")


plot_AGE_str <- plot(DA_park_census$AGE, stand.res[1:length(DA_park_census$AGE)],
                       xlab="AGE",
                       ylab="Standardized Residuals",
                       main="Std. Residuals vs. AGE")
abline(h=0, col="red")
####Did not find any clear associations.
