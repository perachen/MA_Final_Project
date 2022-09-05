{
  library(betareg)
  library(ggdist)
  library(broom)
  library(MASS)
  library(tidyverse)
  library(gtsummary)
  library(ggtext)
  
  select <- dplyr::select
}

dat <- read_csv("data/Anemone_data.csv")


prep_dat <- dat %>% 
  select(-PlantStage, # All "Flower"
         -contains("PercLf"), # non relevant - It's enough to have PercHerbPlant
         -contains("PercHerbpetal")
  ) %>% 
  mutate(NumberoftheSeeds = round(NumberoftheSeeds),
         Flowersexual = case_when(Flowersexual %in% c("Mael", "Mal e",
                                                      "Male", "Malel",
                                                      "Malr") ~ "Male",
                                  Flowersexual %in% c("Femael", "Femal e",
                                                      "Female") ~ "Female")) %>% 
  rename(number_of_seeds = NumberoftheSeeds,
         weight_of_seeds = `Weightofallseeds(g)`,
         color = Red_NonRed) %>% 
  mutate(rate_Herb = PercHerbPlant/100,
         rate_petals = Percentageofdamagepetals/100) %>% 
  select(-c(PercHerbPlant,
            Percentageofdamagepetals)) %>% 
  select(Site,
         ht_cm,
         color,
         Flowercolor,
         Flowersexual,
         weight_of_seeds,
         number_of_seeds,
         rate_Herb,
         rate_petals)



# preparing the data for the first two questions ====
first_two_q <- prep_dat %>%
  select(Site,
         ht_cm,
         color,
         rate_Herb,
         rate_petals,
         Flowersexual
  ) %>% 
  filter(Site == "Beqoa") %>% 
  select(-Site) 

# DATA FOR BETA REGRESSION
omitted_data <- first_two_q %>%
  na.omit() %>% 
  mutate(rate_Herb = ifelse(rate_Herb == 0, 0.001, rate_Herb)) %>% 
  mutate(rate_petals = case_when(rate_petals == 0 ~ 0.001,
                                 rate_petals == 1 ~ 0.999,
                                 T ~ rate_petals)) 

# preparing the data for the last two questions ====
last_two_q <- prep_dat %>%
  select(ht_cm,
         Site,
         rate_Herb,
         rate_petals,
         color,
         Flowersexual,
         number_of_seeds,
         weight_of_seeds) %>% 
  filter(!is.na(number_of_seeds),
         !is.na(Flowersexual))




# DATA for Ordinal regression ----
alternative_ordinal_dat_herb_beqoa <- prep_dat %>% 
  select(color, rate_Herb, ht_cm, Flowersexual, Site) %>%
  na.omit() %>%
  filter(Site == "Beqoa") %>% 
  mutate(Y = case_when(rate_Herb >= 0 & rate_Herb <= 0.03 ~ "[0,0.3%)",
                       rate_Herb > 0.03 & rate_Herb < 0.25 ~ "(0.3%,2.5%)",
                       rate_Herb >= 0.25 ~ "(2.5%, 100%]" 
  )) %>% 
  mutate(Y = factor(Y, levels = c("[0,0.3%)",
                                  "(0.3%,2.5%)",
                                  "(2.5%, 100%]")))



alternative_ordinal_flori_beqoa_dat <- prep_dat %>% 
  select(color, rate_petals, ht_cm, Flowersexual, Site) %>%
  na.omit() %>%
  filter(Site == "Beqoa") %>% 
  mutate(Y = case_when(rate_petals == 0 ~ "0",
                       rate_petals > 0 & rate_petals < 0.1 ~ "(0,1%)",
                       rate_petals >= 0.1 ~ "[1%, 100%]" 
  )) %>% 
  mutate(Y = factor(Y, levels = c("0", "(0,1%)", "[1%, 100%]")))



# information about transformed values -------------------------------------

freq_table <- first_two_q %>% 
  select(rate_Herb, rate_petals) %>% 
  filter(!(is.na(rate_Herb) | is.na(rate_petals))) %>% 
  mutate(rate_Herb_zeros = sum(rate_Herb == 0),  
         rate_Herb_one = sum(rate_Herb == 1),
         rate_petals_zeros = sum(rate_petals == 0),
         rate_petals_one = sum(rate_petals == 1)
  ) %>% 
  select(-c(rate_Herb, rate_petals)) %>% 
  distinct() 


## Number of values transformed from 0 to 0.001

tibble::tibble(
  "Leaf" = glue::glue("{freq_table$rate_Herb_zeros}/60"),
  "Petal" = glue::glue("{freq_table$rate_petals_zeros}/60")
) #%>% 
  # saveRDS("tbl_of_transformed_values_EDA_part.RDS")


# ## Number of values transformed from 1 to 0.99
# 
# tibble::tibble(
#   "Leaf" = glue::glue("{freq_table$rate_Herb_zeros} / 60"),
#   "Petal" = glue::glue("{freq_table$rate_petals_zeros} / 60")
# ) #%>% 
#   # knitr::kable()
