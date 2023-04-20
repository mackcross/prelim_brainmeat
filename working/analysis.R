rm(list=ls())
library(readr)
library(dplyr)

all_df <- read_csv("cleaned_df.csv")
meat_df <- read_csv("meat_df.csv")

library(ggplot2)
ggplot(all_df, x=)


# Primate Meat Intake Summary Statistics 
# Haplorhine vs Strepsirhine 

# Number of Meat Eating Primates 
# Mean (divide number of meat eating/total of that group)
# Of meat eaters, Diversity of Prey (How many cateogires?)
# Mean of diverse prey per group 

infra_prey_diversity_df <- meat_df %>%
  group_by(infraorder, prey) %>%
  summarize(prey = unique(prey)) %>%
  ungroup()
# Haplorhines more diverse (instances of eating fish), but otherwise share mammals, squamata, ambipians, and aves 

parv_prey_diversity_df <- meat_df %>%
  group_by(parvorder, prey) %>%
  summarize(prey = unique(prey)) %>%
  ungroup()


fam_prey_diversity_df <- meat_df %>%
  group_by(family, prey) %>%
  summarize(prey = unique(prey)) %>%
  ungroup()

fam_prey_diversity_df <- meat_df %>%
  group_by(prey, infraorder) %>%
  summarize(prey = unique(prey)) %>%
  ungroup()
# strepshirrhines DO NOT EAT FISH 


# do an ANOVA test!!!



















