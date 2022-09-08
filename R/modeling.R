source("R/preprocess.R")
library(performance)


# Whether the Florivors chose to eat the flower because of the color of the flower --------
beta_reg_florivors <- betareg(rate_petals ~ color + 
                                ht_cm + 
                                Flowersexual, 
                              data = omitted_data, 
                              link = "logit")

summary(beta_reg_florivors)
broom::tidy(beta_reg_florivors) %>% 
  saveRDS("Saving_models_as_RDS_folder/beta_reg_florivors.RDS")

# Only by color
beta_reg_florivors_color <- betareg(rate_petals ~ color,
                                    data = omitted_data, 
                                    link = "logit")

broom::tidy(beta_reg_florivors_color) %>%
  saveRDS("Saving_models_as_RDS_folder/beta_reg_florivors_color.RDS")



# Ordinal regression FLORIVORY
flor_ordinal_mod <- polr(Y ~ color + 
                           ht_cm + 
                           Flowersexual,
                         data = alternative_ordinal_flori_beqoa_dat)


ctable_flor <- coef(summary(flor_ordinal_mod))
p_flor <- pnorm(abs(ctable_flor[, "t value"]), lower.tail = FALSE) * 2
ctable_flor <- cbind(ctable_flor, "p value" = p_flor)


ctable_flor %>% 
  saveRDS("Saving_models_as_RDS_folder/categorical_regression_florivors.RDS")


# Linear model for FLORIVORY
lm_flor <- lm(rate_petals ~ color +
                ht_cm + 
                Flowersexual,
              data = alternative_ordinal_flori_beqoa_dat)


summary(lm_flor)
# performance::check_model(lm_flor)

tidy(lm_flor) %>% 
  saveRDS("Saving_models_as_RDS_folder/lm_florivor_alternative.RDS")





# Whether the Herbivores chose to eat the leaves because of the color of the flower? --------
beta_reg_herbivors <- betareg(rate_Herb ~ color + 
                                ht_cm + 
                                Flowersexual, 
                              data = omitted_data, 
                              link = "logit")

summary(beta_reg_herbivors)
broom::tidy(beta_reg_herbivors) %>% 
  saveRDS("Saving_models_as_RDS_folder/beta_reg_herbivors.RDS")

# Only by color
beta_reg_herbivors_color <- betareg(rate_Herb ~ color,
                                    data = omitted_data, 
                                    link = "logit")

broom::tidy(beta_reg_herbivors_color) %>% 
  saveRDS("Saving_models_as_RDS_folder/beta_reg_herbivors_color.RDS")




# Ordinal regression for HERBIVORY
herb_ordinal_mod <- polr(Y ~ color + ht_cm + 
                           Flowersexual,
                         data = alternative_ordinal_dat_herb_beqoa)


ctable <- coef(summary(herb_ordinal_mod))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

ctable %>% 
  saveRDS("Saving_models_as_RDS_folder/categorical_regression_herbivors.RDS")


# Linear model for HERBIVORY
lm_herb <- lm(rate_Herb ~ color + ht_cm + 
                Flowersexual, data = alternative_ordinal_dat_herb_beqoa)

summary(lm_herb)
# performance::check_model(lm_herb)
tidy(lm_herb) %>% 
  saveRDS("Saving_models_as_RDS_folder/lm_herbivory_alternative.RDS")


# Whether the herbivory reduces fitness to a different extent in red flowers vs. non-red flowers ----
lm_herb_fitness_seed_weight <- lm(weight_of_seeds ~ rate_Herb +
                                    color + 
                                    ht_cm + 
                                    Flowersexual +
                                    Site,
                                  data = last_two_q)

summary(lm_herb_fitness_seed_weight)

car::vif(lm_herb_fitness_seed_weight)


broom::tidy(lm_herb_fitness_seed_weight) %>% 
  saveRDS("Saving_models_as_RDS_folder/lm_herb_fitness_seed_weight.RDS")

# Whether the florivory reduces fitness to a different extent in red flowers vs. non-red flowers -----
lm_flori_fitness_number_of_seeds <- lm(number_of_seeds ~ rate_petals +
                                         color +
                                         ht_cm +
                                         Flowersexual +
                                         Site,
                                       data = last_two_q)

summary(lm_flori_fitness_number_of_seeds)

car::vif(lm_flori_fitness_number_of_seeds)


broom::tidy(lm_flori_fitness_number_of_seeds) %>% 
  saveRDS("Saving_models_as_RDS_folder/lm_flori_fitness_number_of_seeds.RDS")
