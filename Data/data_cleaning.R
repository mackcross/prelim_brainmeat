rm(list=ls())
#set working directory 

library(rvest)
library("readxl")
library(dplyr)
library(tidyr)


#############################################################
# Create additional taxonomic categories
plat_parvorder <- c("atelidae", "callitrichidae", "cebidae", "pitheciidae")
cat_parvorder <- c("cercopithecidae", "hylobatidae", "pongidae", "hominidae")
lem_parvorder <- c("lorisidae", "lemuridae")


#############################################################
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
tool_df$species <- ifelse(tool_df$species == "brown_capuchin_(sapajus_apella", "sapajus_apella", tool_df$species)
tool_df$species <- ifelse(tool_df$species == "sapajus_xanthosternus", "sapajus_xanthosternos", tool_df$species)

#the tool use Wikipedia link did not have tool use for apes within the table
#this has been manually appended
species <- c("pan_troglodytes_troglodytes", "pan_paniscus", "pongo_abelii", "pongo_pygmaeus", "gorilla_gorilla_gorilla", "homo_sapiens", "gorilla_beringei")
tool_type <- c("yes", "yes", "yes", "yes", "yes", "yes", "yes")
ape_tool_df <- data.frame(species, tool_type)
tool_df <- rbind(tool_df, ape_tool_df)  


#############################################################
meat_df <- read_excel("~/Documents/research/tools_diet_brain/data/downloaded_data/Watts_Meat_eating_data.xlsx", skip = 3)
meat_df <- meat_df[, c("infraorder", "superfamily", "family", "primate_species", "prey", "prey_order", "prey_family")] 
#formatting such as making lowercase and changing data type
meat_df$primate_species <- tolower(meat_df$primate_species)
meat_df$prey <- tolower(meat_df$prey)
meat_df$prey_order <- tolower(meat_df$prey_order)
meat_df$infraorder <- factor(meat_df$infraorder)
meat_df$superfamily <- factor(meat_df$superfamily)
meat_df$prey <- factor(meat_df$prey)
meat_df$prey_order <- factor(meat_df$prey_order)
meat_df$prey_family <- factor(meat_df$prey_family)
#fixed species name
meat_df$primate_species <- gsub(" ", "_", meat_df$primate_species)
names(meat_df)[names(meat_df) == "primate_species"] <- "species"
meat_df$species <- ifelse(meat_df$species == "pan_troglodytes", "pan_troglodytes_troglodytes", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "callithrix_humeralifer", "callithrix_humeralifera", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "allenopithecus_nigriviridis", "allenopithecus_nigroviridis", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "cebuella_pygmaea", "callithrix_pygmaea", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "cebus_olivaceous", "cebus_olivaceus", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "cercopithecus_l'hoesti", "cercopithecus_lhoesti", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "erthyrocebus_patas", "erythrocebus_patas", meat_df$species)
meat_df$species <- ifelse(meat_df$species == "eulemur_fulvus", "eulemur_fulvus_fulvus", meat_df$species)
#fixed spelling errors
levels(meat_df$prey)[levels(meat_df$prey) == "amhibia"] <- "amphibia"
# collapsed reptilia under squamata 
levels(meat_df$prey)[levels(meat_df$prey) == "reptiles"] <- "squamata"
levels(meat_df$prey)[levels(meat_df$prey) == "reptilia"] <- "squamata"
#pisces isn't currently used--moving this to same level as teleostei
levels(meat_df$prey)[levels(meat_df$prey) == "pisces"] <- "teleostei"


#############################################################
brain_df <- read_excel("~/Documents/research/tools_diet_brain/data/downloaded_data/DeCasien_Primate brain_size_diet.xls")
brain_df <- brain_df[, c("KEY", "Brain Mass")] 
#formatting such as making lowercase and changing data type
names(brain_df)[names(brain_df) == "KEY"] <- "species"
names(brain_df)[names(brain_df) == "Brain Mass"] <- "brain_mass"
brain_df$species <- tolower(brain_df$species)
# brain_df$species <- factor(brain_df$species)
brain_df$brain_mass <- as.numeric(brain_df$brain_mass)
#removing duplicate species and taking the mean 
brain_df <- group_by(brain_df, species)
brain_df <- summarize(brain_df, mean_brain = mean(brain_mass))


#############################################################
fruit_df <- read_excel("~/Documents/research/tools_diet_brain/data/downloaded_data/DeCasien_Primate brain_size_diet.xls", sheet = "Diet Data")
fruit_df <- fruit_df[, c("Taxon", "Diet Category", "% Fruit")] 
#formatting such as making lowercase and changing data type
names(fruit_df)[names(fruit_df) == "Taxon"] <- "species"
names(fruit_df)[names(fruit_df) == "Diet Category"] <- "diet_category"
names(fruit_df)[names(fruit_df) == "% Fruit"] <- "per_fruit"
fruit_df$species <- tolower(fruit_df$species)
#fruit_df$species <- factor(fruit_df$species)
fruit_df$diet_category <- factor(fruit_df$diet_category)
fruit_df$per_fruit <- as.numeric(fruit_df$per_fruit)


#############################################################
weight_df <- read_excel("~/Documents/research/tools_diet_brain/data/downloaded_data/DeCasien_Primate brain_size_diet.xls", sheet = "Body Data")
weight_df <- weight_df[, c("Taxon", "Final Body Weight (g)")] 
#formatting such as making lowercase and changing data type
names(weight_df)[names(weight_df) == "Taxon"] <- "species"
names(weight_df)[names(weight_df) == "Final Body Weight (g)"] <- "body_weight"
weight_df$species <- tolower(weight_df$species)


#############################################################
socialsystem_df <- read_excel("~/Documents/research/tools_diet_brain/data/downloaded_data/DeCasien_Primate brain_size_diet.xls", sheet = "System Data")
socialsystem_df <- socialsystem_df[, c("Taxon", "Social System", "Mating System")] 
#formatting such as making lowercase and changing data type
names(socialsystem_df)[names(socialsystem_df) == "Taxon"] <- "species"
names(socialsystem_df)[names(socialsystem_df) == "Social System"] <- "social_system"
names(socialsystem_df)[names(socialsystem_df) == "Mating System"] <- "mating_system"
socialsystem_df$social_system <- tolower(socialsystem_df$social_system)
socialsystem_df$mating_system <- tolower(socialsystem_df$mating_system)
socialsystem_df$species <- tolower(socialsystem_df$species)
socialsystem_df$social_system <- factor(socialsystem_df$social_system)
socialsystem_df$mating_system <- factor(socialsystem_df$mating_system)


#############################################################
groupsize_df <- read_excel("~/Documents/research/tools_diet_brain/data/downloaded_data/DeCasien_Primate brain_size_diet.xls", sheet = "Group Size Data")
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


#############################################################
#semi-master data frame
#contains everything except tool_df
df1 <- merge(brain_df, fruit_df, by = "species", all = TRUE)
df2 <- merge(df1, groupsize_df, by = "species", all = TRUE)
df3 <- merge(df2, socialsystem_df, by = "species", all = TRUE)
df4 <- merge(df3, tool_df, by = "species", all = TRUE)
df5 <- merge(df4, weight_df, by = "species", all = TRUE)


#group the data frame by the "id" column and summarize the "col1" and "col2" columns
df6 <- meat_df %>%
  group_by(species) %>%
  summarize(infraorder = infraorder, superfamily, family, 
            prey = list(unique(prey)), prey_order = list(unique(prey_order)), 
            prey_family = list(unique(prey_family))) %>%
  ungroup()
#collapse the list columns into strings
df6 <- df6 %>%
  mutate(prey = sapply(prey, paste, collapse = ", "),
         prey_order = sapply(prey_order, paste, collapse = ", "),
         prey_family = sapply(prey_family, paste, collapse = ", "))
df6 <- unique(df6)
df7 <- merge(df5, df6, by = "species", all = TRUE)

#############################################################
# add a tool user column 
if("tool_type" %in% colnames(df7)){
  df7$use_tools <- 0
  df7$use_tools[!is.na(df7$tool_type) & df7$tool_type != ""] <- 1
}

############################################################
# modify rows and add columns 
# remove the _sp. rows (they're redundant)
rows_to_remove <- grepl("_sp\\.", df7$species) 
df7 <- df7[!rows_to_remove, ]

# create a genus column 
genus <- sapply(strsplit(df7$species, "_"), "[", 1)
df7$genus <- genus

# trying to fill in some of the NA INFRAORDER 
df7[df7$species == "aotus_azarai", "infraorder"] <- "haplorrhini"
df7[df7$species == "ateles_belzebuth", "infraorder"] <- "haplorrhini"
df7[df7$species == "chiropotes_satanas", "infraorder"] <- "haplorrhini"
df7[df7$species == "colobus_angolensis", "infraorder"] <- "haplorrhini"
df7[df7$species == "erythrocebus_patas", "infraorder"] <- "haplorrhini"
df7[df7$species == "brachyteles_arachnoides", "infraorder"] <- "haplorrhini"
df7[df7$species == "gorilla_beringei", "infraorder"] <- "haplorrhini"
df7[df7$species == "homo_sapiens", "infraorder"] <- "haplorrhini"
df7[df7$species == "lophocebus_albigena", "infraorder"] <- "haplorrhini"
df7[df7$species == "nasalis_larvatus", "infraorder"] <- "haplorrhini"
df7[df7$species == "pithecia_monachus", "infraorder"] <- "haplorrhini"
df7[df7$species == "piliocolobus_badius", "infraorder"] <- "haplorrhini"
df7[df7$species == "presbytis_comata", "infraorder"] <- "haplorrhini"
df7[df7$species == "procolobus_verus", "infraorder"] <- "haplorrhini"
df7[df7$species == "pygathrix_nemaeus", "infraorder"] <- "haplorrhini"
df7[df7$species == "semnopithecus_entellus", "infraorder"] <- "haplorrhini"
df7[df7$species == "simias_concolor", "infraorder"] <- "haplorrhini"
df7[df7$species == "symphalangus_syndactylus", "infraorder"] <- "haplorrhini"
df7[df7$species == "theropithecus_gelada", "infraorder"] <- "haplorrhini"
df7[df7$species == "trachypithecus_cristatus", "infraorder"] <- "haplorrhini"

df7[df7$species == "arctocebus_calabarensis", "infraorder"] <- "strepsirrhini"
df7[df7$species == "avahi_laniger", "infraorder"] <- "strepsirrhini"
df7[df7$species == "daubentonia_madagascariensis", "infraorder"] <- "strepsirrhini"
df7[df7$species == "galagoides_demidoff", "infraorder"] <- "strepsirrhini"
df7[df7$species == "hapalemur_simus", "infraorder"] <- "strepsirrhini"
df7[df7$species == "indri_indri", "infraorder"] <- "strepsirrhini"
df7[df7$species == "lemur_catta", "infraorder"] <- "strepsirrhini"
df7[df7$species == "lepilemur_mustelinus", "infraorder"] <- "strepsirrhini"
df7[df7$species == "propithecus_coquereli", "infraorder"] <- "strepsirrhini"
df7[df7$species == "varecia_rubra", "infraorder"] <- "strepsirrhini"
df7[df7$species == "varecia_variegata_variegata", "infraorder"] <- "strepsirrhini"

# trying to fill in some of the NA Family  
df7[df7$species == "aotus_azarai", "family"] <- "aotidae"
df7[df7$species == "arctocebus_calabarensis", "family"] <- "lorisidae"
df7[df7$species == "ateles_belzebuth", "family"] <- "atelidae"
df7[df7$species == "avahi_laniger", "family"] <- "indriidae"
df7[df7$species == "brachyteles_arachnoides", "family"] <- "atelidae"
df7[df7$species == "chiropotes_satanas", "family"] <- "pitheciidae"
df7[df7$species == "colobus_angolensis", "family"] <- "cercopithecidae"
df7[df7$species == "daubentonia_madagascariensis", "family"] <- "daubentoniidae"
df7[df7$species == "galagoides_demidoff", "family"] <- "galagoides"
df7[df7$species == "gorilla_beringei", "family"] <- "homininae"
df7[df7$species == "hapalemur_aureus", "family"] <- "lemuridae"
df7[df7$species == "homo_sapiens", "family"] <- "homininae"
df7[df7$species == "indri_indri", "family"] <- "indriidae"
df7[df7$species == "lemur_catta", "family"] <- "lemuridae"
df7[df7$species == "lepilemur_mustelinus", "family"] <- "lepilemuridae"
df7[df7$species == "lophocebus_albigena", "family"] <- "cercopithecidae"
df7[df7$species == "nasalis_larvatus", "family"] <- "cercopithecidae"
df7[df7$species == "piliocolobus_badius", "family"] <- "cercopithecidae"
df7[df7$species == "pithecia_monachus", "family"] <- "pitheciidae"
df7[df7$species == "presbytis_comata", "family"] <- "cercopithecidae"
df7[df7$species == "procolobus_verus", "family"] <- "cercopithecidae"
df7[df7$species == "propithecus_coquereli", "family"] <- "indriidae"
df7[df7$species == "pygathrix_nemaeus", "family"] <- "cercopithecidae"
df7[df7$species == "semnopithecus_entellus", "family"] <- "cercopithecidae"
df7[df7$species == "simias_concolor", "family"] <- "cercopithecidae"
df7[df7$species == "symphalangus_syndactylus", "family"] <- "hylobatidae"
df7[df7$species == "theropithecus_gelada", "family"] <- "cercopithecidae"
df7[df7$species == "trachypithecus_cristatus", "family"] <- "cercopithecidae"
df7[df7$species == "varecia_rubra", "family"] <- "lemuridae"


df7[df7$species == "aotus_azarai", "superfamily"] <- "simmiformes"
df7[df7$species == "avahi_laniger", "superfamily"] <- "lemuriformes"
df7[df7$species == "daubentonia_madagascariensis", "superfamily"] <- "lemuriformes"
df7[df7$species == "galagoides_demidoff", "superfamily"] <- "lorisiformes"
df7[df7$species == "gorilla_beringei", "superfamily"] <- "simmiformes"
df7[df7$species == "lepilemur_mustelinus", "superfamily"] <- "lemuriformes"

for (i in unique(df7$genus)) {
  missing_rows <- which(df7$genus == i & is.na(df7$infraorder))
  non_missing_row <- which(df7$genus == i & !is.na(df7$infraorder))[1]
  if (length(non_missing_row) > 0) {
    df7$infraorder[missing_rows] <- df7$infraorder[non_missing_row]
  }
}

for (i in unique(df7$genus)) {
  missing_rows <- which(df7$genus == i & is.na(df7$superfamily))
  non_missing_row <- which(df7$genus == i & !is.na(df7$superfamily))[1]
  if (length(non_missing_row) > 0) {
    df7$superfamily[missing_rows] <- df7$superfamily[non_missing_row]
  }
}

for (i in unique(df7$genus)) {
  missing_rows <- which(df7$genus == i & is.na(df7$family))
  non_missing_row <- which(df7$genus == i & !is.na(df7$family))[1]
  if (length(non_missing_row) > 0) {
    df7$family[missing_rows] <- df7$family[non_missing_row]
  }
}

for (i in unique(df7$family)) {
  missing_rows <- which(df7$family == i & is.na(df7$superfamily))
  non_missing_row <- which(df7$family == i & !is.na(df7$superfamily))[1]
  if (length(non_missing_row) > 0) {
    df7$superfamily[missing_rows] <- df7$superfamily[non_missing_row]
  }
}

# removing the proto-primate
df7 <- df7[df7$species != "ignacius_graybullianus", ]

# creating meat eating column binary 
if("prey" %in% colnames(df7)){
  df7$meat_eater <- 0
  df7$meat_eater[!is.na(df7$prey) & df7$prey != ""] <- 1
}

# designating humans as meat eaters 
df7[df7$species == "homo_sapiens", "meat_eater"] <- "1"

# create a new row creating a brain_body ratio 
# df7$eq <- (df7$mean_brain / 0.12) * ((df7$body_weight)^0.66)

df7$family <- ifelse(df7$family == "homininae", "hominidae", df7$family)
df7$family <- ifelse(df7$family == "pongidae", "hominidae", df7$family)

#############################################################
#finishing creation of new columns 
meat_df$parvorder <- meat_df$family
#had to wait to change it to factor to fix the duplication issues
meat_df$family <- factor(meat_df$family)
for (i in 1:nrow(meat_df)){
  if (meat_df$parvorder[i] %in% plat_parvorder){
    meat_df$parvorder[i] <- "platyrrhine"
  }
  if (meat_df$parvorder[i] %in% cat_parvorder){
    meat_df$parvorder[i] <- "catarrhine"
  }
  if (meat_df$parvorder[i] %in% lem_parvorder){
    meat_df$parvorder[i] <- "lemuriforms"
  }
}



library(readr)
write_csv(df7, "cleaned_df.csv") 
write_csv(meat_df, "meat_df.csv") 
