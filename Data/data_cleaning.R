rm(list=ls())
#set working directory 

library(rvest)
url <- "https://en.wikipedia.org/wiki/Tool_use_by_animals"
tb <- html_table(read_html(url))
tool_df <- tb[[1]]
tool_df <- tool_df[, c("Species", "Type and Extent of Tool Use")] 
#formatting such as making lowercase and changing data type
names(tool_df)[names(tool_df) == "Species"] <- "species"
names(tool_df)[names(tool_df) == "Type and Extent of Tool Use"] <- "tool_type"
#extract text within parentheses 
tool_df$species <- gsub(".*?\\((.*?)\\).*", "\\1", tool_df$species)
tool_df$species <- gsub(" ", "_", tool_df$species)
tool_df$species <- tolower(tool_df$species)
#one name wasn't extracted properly 


#the tool use Wikipedia link did not have tool use for apes within the table
#this has been manually appended
species <- c("pan_troglodytes_troglodytes", "pan_paniscus", "pongo_abelii", "pongo_pygmaeus", "gorilla_gorilla")
tool_type <- c("yes", "yes", "yes", "yes", "yes")
ape_tool_df <- data.frame(species, tool_type)
tool_df <- rbind(tool_df, ape_tool_df)  


library("readxl")
meat_df <- read_excel("Data/Watts_Meat_eating_data.xlsx", skip = 3)
meat_df <- meat_df[, c("infraorder", "superfamily", "family", "primate_species", "prey", "prey_order")] 
#formatting such as making lowercase and changing data type
meat_df$primate_species <- tolower(meat_df$primate_species)
meat_df$prey <- tolower(meat_df$prey)
meat_df$prey_order <- tolower(meat_df$prey_order)
meat_df$infraorder <- factor(meat_df$infraorder)
meat_df$superfamily <- factor(meat_df$superfamily)
meat_df$family <- factor(meat_df$family)
meat_df$prey <- factor(meat_df$prey)
meat_df$prey_order <- factor(meat_df$prey_order)
meat_df$prey_family <- factor(meat_df$family)
#removed spaces
meat_df$primate_species <- gsub(" ", "_", meat_df$primate_species)
#fixed spelling errors
levels(meat_df$prey)[levels(meat_df$prey) == "amhibia"] <- "amphibia"
#pisces isn't currently used--moving this to same level as teleostei
levels(meat_df$prey)[levels(meat_df$prey) == "pisces"] <- "teleostei"

brain_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls")
brain_df <- brain_df[, c("KEY", "Brain Mass")] 
#formatting such as making lowercase and changing data type
names(brain_df)[names(brain_df) == "KEY"] <- "species"
names(brain_df)[names(brain_df) == "Brain Mass"] <- "brain_mass"
brain_df$species <- tolower(brain_df$species)
# brain_df$species <- factor(brain_df$species)
brain_df$brain_mass <- as.numeric(brain_df$brain_mass)
#removing duplicate species and taking the mean 
library(dplyr)
brain_df <- group_by(brain_df, species)
brain_df <- summarize(brain_df, mean_brain = mean(brain_mass))

fruit_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "Diet Data")
fruit_df <- fruit_df[, c("Taxon", "Diet Category", "% Fruit")] 
#formatting such as making lowercase and changing data type
names(fruit_df)[names(fruit_df) == "Taxon"] <- "species"
names(fruit_df)[names(fruit_df) == "Diet Category"] <- "diet_category"
names(fruit_df)[names(fruit_df) == "% Fruit"] <- "per_fruit"
fruit_df$species <- tolower(fruit_df$species)
#fruit_df$species <- factor(fruit_df$species)
fruit_df$diet_category <- factor(fruit_df$diet_category)
fruit_df$per_fruit <- as.numeric(fruit_df$per_fruit)

weight_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "Body Data")
weight_df <- weight_df[, c("Taxon", "Final Body Weight (g)")] 
#formatting such as making lowercase and changing data type
names(weight_df)[names(weight_df) == "Taxon"] <- "species"
names(weight_df)[names(weight_df) == "Final Body Weight (g)"] <- "body_weight"
weight_df$species <- tolower(weight_df$species)

socialsystem_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "System Data")
socialsystem_df <- socialsystem_df[, c("Taxon", "Social System", "Mating System")] 
#formatting such as making lowercase and changing data type
names(socialsystem_df)[names(socialsystem_df) == "Taxon"] <- "species"
names(socialsystem_df)[names(socialsystem_df) == "Social System"] <- "social_system"
names(socialsystem_df)[names(socialsystem_df) == "Mating System"] <- "mating_system"
socialsystem_df$social_system <- factor(socialsystem_df$social_system)
socialsystem_df$mating_system <- factor(socialsystem_df$mating_system)
socialsystem_df$social_system <- tolower(socialsystem_df$social_system)
socialsystem_df$mating_system <- tolower(socialsystem_df$mating_system)
socialsystem_df$species <- tolower(socialsystem_df$species)

groupsize_df <- read_excel("Data/DeCasien_Primate brain size is predicted by diet but not sociality.xls", sheet = "Group Size Data")
groupsize_df <- groupsize_df[, c("Taxon", "Group Size")] 
#formatting such as making lowercase and changing data type
names(groupsize_df)[names(groupsize_df) == "Taxon"] <- "species"
names(groupsize_df)[names(groupsize_df) == "Group Size"] <- "group_size"
groupsize_df$species <- tolower(groupsize_df$species)
#groupsize_df$species <- factor(groupsize_df$species)
groupsize_df$group_size <- as.numeric(groupsize_df$group_size)
#removing duplicate species and taking the mean 
groupsize_df <- group_by(groupsize_df, species)
groupsize_df <- summarize(groupsize_df, mean_groupsize = mean(group_size))

#master dataframe
#contains everything except tool_df
df1 <- merge(brain_df, fruit_df, by = "species", all = TRUE)
df2 <- merge(df1, groupsize_df, by = "species", all = TRUE)
df3 <- merge(df2, socialsystem_df, by = "species", all = TRUE)
df4 <- merge(df3, tool_df, by = "species", all = TRUE)
df5 <- merge(df4, weight_df, by = "species", all = TRUE)

library(readr)
write_csv(df5, "cleaned_df.csv") 
write_csv(meat_df, "cleaned_meat.csv")

# To Do 
# double check that all factors aren't duplicates 
# x <- subset(tool_df, species = "brown_capuchin_(sapajus_apella")
