

library(tidyverse)
library(psych)
library(factoextra)


#data downloaded from https://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
load("./data/WV6_Data_R_v20201117.rdata") 

#converting to a tibble
WV6_Data_R_v20201117 %>% 
  tibble() -> wvs6

#renaming variables 
wvs6 %>% 
  rename(
    country = B_COUNTRY_ALPHA,
    age = V242,
    pol = V95,
    rel = V152,
    edu = V248,
    lifesat = V23
  ) %>% 

#keeping only Germany and the Netherlands  
  filter(country == "DEU" | country == "NLD") %>% 

#subtracting battery mean for all to correct for aquiescence
  mutate(meanBFI = rowMeans(select(., matches("V160\\w")))) %>% 
  mutate(across(V160A:V160J, \(x) x - meanBFI)) %>% 
#then standardizing within countries
  mutate(across(V160A:V160J, \(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)), .by = country) %>% 
#reversing personality items to all have high trait value be high value
  mutate(across(c(V160A, V160G, V160C, V160D, V160E), \(x) x*-1)) %>% 
#reversing science attitude items to all have high trait value be positive
  mutate(across(c(V194, V195, V196), \(x) 11-x)) %>% 
#labelling sex variable
  mutate(sex = case_when(V240==1 ~ "Male", V240==2 ~ "Female")) %>% 

#making additive indexes for the big 5
  mutate(
    extraversion = rowMeans(select(.,V160A, V160F)),
    agreeableness = rowMeans(select(.,V160B, V160G)),
    conscientiousness = rowMeans(select(.,V160C, V160H)),
    neuroticism = rowMeans(select(.,V160D, V160I)),
    openness = rowMeans(select(.,V160E, V160J)),
#and the additive science attitude measure
    sciatts = rowMeans(select(., V192, V193, V194, V195, V196, V197))
  ) %>% 

#making standardized versions of all continuous variables used in analyses.
  mutate(
    V192s = (V192-mean(V192, na.rm = T))/sd(V192, na.rm = T),
    V193s = (V193-mean(V193, na.rm = T))/sd(V193, na.rm = T),
    V194s = (V194-mean(V194, na.rm = T))/sd(V194, na.rm = T),
    V195s = (V195-mean(V195, na.rm = T))/sd(V195, na.rm = T),
    V196s = (V196-mean(V196, na.rm = T))/sd(V196, na.rm = T),
    V197s = (V197-mean(V197, na.rm = T))/sd(V197, na.rm = T),
    educont = (edu-mean(edu, na.rm = T))/sd(edu, na.rm = T),
    across(
      c(sciatts,
        extraversion,
        agreeableness,
        conscientiousness,
        neuroticism,
        openness,
        pol,
        rel),
      \(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)
    ),
  #then relabeling education
    edu = case_when(
      edu == 1 ~ "1) No formal education",
      edu == 2 ~ "2) Incomplete primary school",
      edu == 3 ~ "3) Complete primary school",
      edu == 4 ~ "4) Incomplete secondary school: technical/ vocational type",
      edu == 5 ~ "5) Complete secondary school: technical/ vocational type",
      edu == 6 ~ "6) Incomplete secondary school: university-preparatory type",
      edu == 7 ~ "7) Complete secondary school: university-preparatory type",
      edu == 8 ~ "8) Some university-level education, without degree",
      edu == 9 ~ "9) University - level education, with degree"
    )
  ) -> wvs6_cut




# factor score data -------------------------------------------------------

#getting factor scores for a factor solution for science attitudes
bind_cols(
  wvs6_cut %>% drop_na(V192:V197) %>% select(-sciatts),
  factor.scores(
    wvs6_cut %>% select(V192:V197)%>% drop_na(),
    fa(wvs6_cut %>% select(V192:V197) %>% drop_na(), 1)
  )$scores %>% 
    as_tibble() %>% 
    transmute(sciatts = MR1/sd(MR1)*-1)
) -> fadata


# pca data ----------------------------------------------------------------

#and a principal component solution
bind_cols(
  wvs6_cut %>% drop_na(V192:V197) %>% select(-sciatts), 
  get_pca_ind(
    princomp(
      wvs6_cut %>%  
        select(V192:V197) %>%
        drop_na(), 
      cor = TRUE 
    )
  )$coord[,1]
) %>% 
  rename(sciatts = length(names(.))) %>% 
  mutate(sciatts = sciatts/sd(sciatts)*-1) -> pcadata


