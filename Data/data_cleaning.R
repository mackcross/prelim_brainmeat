
rm(list=ls())

# Set working directory 

# DATA IMPORT
# Primate Tool Use

# NOTE: table DID NOT include chimps, gorillas, orangs and bonobos (need to manually add)
# NOTE: fix species name 
library(rvest)
url <- "https://en.wikipedia.org/wiki/Tool_use_by_animals"
tb <- html_table(read_html(url))
tool_df <- tb[[1]]
tool_df <- tool_df[, c("Species", "Type and Extent of Tool Use")] 
names(tool_df)[names(tool_df) == "Species"] <- "species"
names(tool_df)[names(tool_df) == "Type and Extent of Tool Use"] <- "tool_type"

# Meat Eating
library("readxl")
meat_df <- read_excel("Data/Watts_Meat_eating_data.xlsx", skip = 3)

# Other Variables 
brain_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls")
brain_df <- brain_df[, c("KEY", "Brain Mass")] 
names(brain_df)[names(brain_df) == "KEY"] <- "species"
names(brain_df)[names(brain_df) == "Brain Mass"] <- "brain_mass"
brain_df$species <- tolower(brain_df$species)
brain_df$species <- factor(brain_df$species)
brain_df$brain_mass <- as.numeric(brain_df$brain_mass)

fruit_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "Diet Data")
fruit_df <- fruit_df[, c("Taxon", "Diet Category", "% Fruit")] 
names(fruit_df)[names(fruit_df) == "Taxon"] <- "species"
names(fruit_df)[names(fruit_df) == "Diet Category"] <- "diet_category"
names(fruit_df)[names(fruit_df) == "% Fruit"] <- "per_fruit"
fruit_df$species <- tolower(fruit_df$species)
fruit_df$species <- factor(fruit_df$species)
fruit_df$diet_category <- factor(fruit_df$diet_category)
fruit_df$per_fruit <- as.numeric(fruit_df$per_fruit)

weight_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "Body Data")
weight_df <- weight_df[, c("Taxon", "Final Body Weight (g)")] 
names(weight_df)[names(weight_df) == "Taxon"] <- "species"
names(weight_df)[names(weight_df) == "Final Body Weight (g)"] <- "body_weight"

socialsystem_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "System Data")
socialsystem_df <- socialsystem_df[, c("Taxon", "Social System", "Mating System")] 
names(socialsystem_df)[names(socialsystem_df) == "Taxon"] <- "species"
names(socialsystem_df)[names(socialsystem_df) == "Social System"] <- "social_system"
names(socialsystem_df)[names(socialsystem_df) == "Mating System"] <- "mating_system"

groupsize_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "Group Size Data")
groupsize_df <- groupsize_df[, c("Taxon", "Group Size")] 
names(groupsize_df)[names(groupsize_df) == "Taxon"] <- "species"
names(groupsize_df)[names(groupsize_df) == "Group Size"] <- "group_size"
groupsize_df$species <- tolower(groupsize_df$species)
groupsize_df$species <- factor(groupsize_df$species)
groupsize_df$group_size <- as.numeric(groupsize_df$group_size)







