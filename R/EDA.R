source("R/preprocess.R")

### Exploration

# Flower color by sites #TODO - report
# flower_color_by_site_plot <-
prep_dat %>% 
  select(Flowercolor, Site) %>%
  group_by(Site) %>% 
  count(Flowercolor) %>% 
  mutate(nn = sum(n), prop = n/nn) %>% 
  mutate(Flowercolor = ifelse(is.na(Flowercolor), yes = "Missing", Flowercolor)) %>% 
  ggplot(aes(x = as.factor(Flowercolor),
             y = prop,
             fill = as.factor(Flowercolor),
             label = glue::glue("{round(prop*100)}% ({n})"))) +
  geom_col(color = "black") +
  geom_text(aes(vjust = -0.5),
            size = 3.5) +
  facet_wrap(~Site,
             drop = FALSE) +
  scale_fill_manual(values = c("pink" = "#FA99D3",
                               "Pink-white" = "#EDCDE0",
                               "Red" = "#F64850",
                               "White" = "#FFFFFF",
                               "purple" = "#D16DEA",
                               "Missing" = "gray")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.05)) +
  labs(title = "Flower Color Distribution by Site") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        title = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))      



# Herbivory by Site
herbi_plot <- prep_dat %>% 
  select(color, rate_Herb, Site) %>%
  na.omit() %>%
  filter(Site == "Beqoa") %>%
  mutate(color = case_when(color == "Non_Red" ~ "Non Red",
                           T ~ color)) %>% 
  ggplot(aes(x = rate_Herb,
             y = color,
             fill= color)) +
  stat_slab(aes(thickness = stat(pdf*n)), 
            scale = 0.7) +
  stat_dotsinterval(side = "bottom",
                    scale = 0.7,
                    slab_size = NA) + 
  scale_fill_manual(values = c("Red" = "#F64850",
                               "Non Red" = "#C4D6B0")) + 
  theme_bw(13) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     labels =  seq(0, 1, 0.1),
                     limits = c(0,1)) +
  xlab("Herbivory Rate") +
  ylab("") +
  labs(title = "Herbivory Rate for <span style = 'color:#F64850;'>Red</span> and <span style = 'color: #C4D6B0;'>Non Red</span> flowers") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_markdown()) 
  

ggsave(plot = herbi_plot, filename = "herbivory_by_color_plot.png")



# Florivory

flori_plot <- prep_dat %>% 
  select(color, rate_petals, Site) %>%
  na.omit() %>%
  filter(Site == "Beqoa") %>%
  mutate(color = case_when(color == "Non_Red" ~ "Non Red",
                           T ~ color)) %>% 
  ggplot(aes(x = rate_petals,
             y = color,
             fill = color))+
  stat_slab(aes(thickness = stat(pdf*n)), 
            scale = 0.7) +
  stat_dotsinterval(side = "bottom",
                    scale = 0.7,
                    slab_size = NA) + 
  scale_fill_manual(values = c("Red" = "#F64850",
                               "Non Red" = "#C4D6B0")) + 
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     labels =  seq(0, 1, 0.1),
                     limits = c(0,1)) +
  theme_bw(13) +
  xlab("Florivory Rate") +
  ylab("") +
  labs(title = "Florivory Rate for <span style = 'color:#F64850;'>Red</span> and <span style = 'color: #C4D6B0;'>Non Red</span> flowers") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_markdown())


ggsave(plot = flori_plot, filename = "florivory_by_color_plot.png")




# number of seeds by rate of florivory
florivory_by_number_of_seeds <- prep_dat %>% 
  select(number_of_seeds, rate_petals) %>%
  na.omit() %>%
  ggplot(aes(x = rate_petals, y = number_of_seeds)) +
  geom_point(size = 3) +
  ylab("Number of Seeds") +
  xlab("Florivory Rate") +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     labels =  seq(0, 1, 0.1),
                     limits = c(0, 1)) +
  theme_bw(13) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_markdown())


ggsave(plot = florivory_by_number_of_seeds,
       filename = "florivory_by_number_of_seeds.png")



# weight of seeds by rate of herbivory
herbivory_by_weight_of_seeds <- prep_dat %>% 
  select(weight_of_seeds, rate_Herb) %>%
  na.omit() %>%
  ggplot(aes(x = rate_Herb, y = weight_of_seeds)) +
  geom_point(size = 3) +
  ylab("Weight of Seeds") +
  xlab("Herbivory Rate") +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     labels =  seq(0, 1, 0.1),
                     limits = c(0,1)) +
  theme_bw(13) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_markdown())
  
ggsave(plot = herbivory_by_weight_of_seeds,
       filename = "herbivory_by_weight_of_seeds.png")

