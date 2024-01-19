# AGG-CoP November 2023 Data Analysis
# Adam Dimech
# adam.dimech@agriculture.vic.gov.au

# Clean slate
rm(list=ls())

# Libraries
library(readxl)
library(tidyverse)
library(writexl)

# Get data - Barley
Barley_Q1 <- read_excel("data/BARLEY_ What are the most important AGG-collected traits (up to five) for your research_(1-5000).xlsx")
Barley_Q2 <- read_excel("data/BARLEY_ Are any of the non-collected traits important for your research_(1-5000).xlsx")
Barley_Q3 <- read_excel("data/BARLEY_ Add any other key traits of interest.(1-5000).xlsx")
Wheat_Q1 <- read_excel("data/WHEAT_ What are the most important AGG-collected traits (up to five) for your research_(1-5000).xlsx")
Wheat_Q2 <- read_excel("data/WHEAT_ Are any of the non-collected traits important for your research_(1-5000).xlsx")
Wheat_Q3 <- read_excel("data/WHEAT_ Add any other key traits of interest.(1-5000).xlsx")
Chickpea_Q1 <- read_excel("data/CHICKPEA_ What are the most important AGG-collected traits (up to five) for your research_(1-5000).xlsx")
Chickpea_Q2 <- read_excel("data/CHICKPEA_ Are any of the non-collected traits important for your research_(1-5000).xlsx")
Chickpea_Q3 <- read_excel("data/CHICKPEA_ Add any other key traits of interest.(1-5000).xlsx")
Lentil_Q1 <- read_excel("data/LENTIL_ What are the most important AGG-collected traits (up to five) for your research_(1-5000).xlsx")
Lentil_Q2 <- read_excel("data/LENTIL_ Are any of the non-collected traits important for your research_(1-5000).xlsx")
Lentil_Q3 <- read_excel("data/LENTIL_ Add any other key traits of interest.(1-5000).xlsx")
MungBean_Q1 <- read_excel("data/MUNG BEAN_ What are the most important AGG-collected traits (up to five) for your research(1-5000).xlsx")
MungBean_Q2 <- read_excel("data/MUNG BEAN_ Are any of the non-collected traits important for your research_(1-5000).xlsx")
MungBean_Q3 <- read_excel("data/MUNG BEAN_ Add any other key traits of interest.(1-5000).xlsx")


# Barley
# Sort Data - Q1
Barley_Q1_Tidy <- data.frame(Barley_Q1[6]) # Select appropriate column (discard the rubbish)
colnames(Barley_Q1_Tidy) <- c('Trait') # Reset column name for simplicity
Barley_Q1_List <- Barley_Q1_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Barley_Q1_List <- Barley_Q1_List[!apply(Barley_Q1_List == "", 1, all),] # Remove empty rows
Barley_Q1_List <- Barley_Q1_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Barley_Q1_List$Source <- rep(c("Collected"),times=nrow(Barley_Q1_List)) # Add source to table

# Sort Data - Q2
Barley_Q2_Tidy <- data.frame(Barley_Q2[6]) # Select appropriate column (discard the rubbish)
colnames(Barley_Q2_Tidy) <- c('Trait') # Reset column name for simplicity
Barley_Q2_List <- Barley_Q2_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Barley_Q2_List <- Barley_Q2_List[!apply(Barley_Q2_List == "", 1, all),] # Remove empty rows
Barley_Q2_List <- Barley_Q2_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Barley_Q2_List$Source <- rep(c("Non-Collected"),times=nrow(Barley_Q2_List)) # Add source to table

# Sort Data - Q3
Barley_Q3_Tidy <- data.frame(Barley_Q3[6]) # Select appropriate column (discard the rubbish)
colnames(Barley_Q3_Tidy) <- c('Trait') # Reset column name for simplicity
Barley_Q3_Tidy$Trait <- tolower(Barley_Q3_Tidy$Trait)
Barley_Q3_List <- Barley_Q3_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Barley_Q3_List <- Barley_Q3_List[!apply(Barley_Q3_List == "", 1, all),] # Remove empty rows
Barley_Q3_List <- Barley_Q3_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Barley_Q3_List$Source <- rep(c("Others"),times=nrow(Barley_Q3_List)) # Add source to table

# Bind all Barley
barley <- rbind(Barley_Q1_List, Barley_Q2_List, Barley_Q3_List) # Combine data
barley$Species <- rep(c("Barley"),times=nrow(barley)) # Add species column
barley <- barley %>% arrange(desc(n)) # Sort by popularity


# Wheat
# Sort Data - Q1
Wheat_Q1_Tidy <- data.frame(Wheat_Q1[6]) # Select appropriate column (discard the rubbish)
colnames(Wheat_Q1_Tidy) <- c('Trait') # Reset column name for simplicity
Wheat_Q1_List <- Wheat_Q1_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Wheat_Q1_List <- Wheat_Q1_List[!apply(Wheat_Q1_List == "", 1, all),] # Remove empty rows
Wheat_Q1_List <- Wheat_Q1_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Wheat_Q1_List$Source <- rep(c("Collected"),times=nrow(Wheat_Q1_List)) # Add source to table

# Sort Data - Q2
Wheat_Q2_Tidy <- data.frame(Wheat_Q2[6]) # Select appropriate column (discard the rubbish)
colnames(Wheat_Q2_Tidy) <- c('Trait') # Reset column name for simplicity
Wheat_Q2_List <- Wheat_Q2_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Wheat_Q2_List <- Wheat_Q2_List[!apply(Wheat_Q2_List == "", 1, all),] # Remove empty rows
Wheat_Q2_List <- Wheat_Q2_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Wheat_Q2_List$Source <- rep(c("Non-Collected"),times=nrow(Wheat_Q2_List)) # Add source to table


# Sort Data - Q3
Wheat_Q3_Tidy <- data.frame(Wheat_Q3[6]) # Select appropriate column (discard the rubbish)
colnames(Wheat_Q3_Tidy) <- c('Trait') # Reset column name for simplicity
Wheat_Q3_Tidy$Trait <- tolower(Wheat_Q3_Tidy$Trait)
Wheat_Q3_List <- Wheat_Q3_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Wheat_Q3_List <- Wheat_Q3_List[!apply(Wheat_Q3_List == "", 1, all),] # Remove empty rows
Wheat_Q3_List <- Wheat_Q3_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Wheat_Q3_List$Source <- rep(c("Others"),times=nrow(Wheat_Q3_List)) # Add source to table

# Bind all Wheat
wheat <- rbind(Wheat_Q1_List, Wheat_Q2_List, Wheat_Q3_List) # Combine data
wheat$Species <- rep(c("Wheat"),times=nrow(wheat)) # Add species column
wheat <- wheat %>% arrange(desc(n)) # Sort by popularity


# Chickpea
# Sort Data - Q1
Chickpea_Q1_Tidy <- data.frame(Chickpea_Q1[6]) # Select appropriate column (discard the rubbish)
colnames(Chickpea_Q1_Tidy) <- c('Trait') # Reset column name for simplicity
Chickpea_Q1_List <- Chickpea_Q1_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Chickpea_Q1_List <- Chickpea_Q1_List[!apply(Chickpea_Q1_List == "", 1, all),] # Remove empty rows
Chickpea_Q1_List <- Chickpea_Q1_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Chickpea_Q1_List$Source <- rep(c("Collected"),times=nrow(Chickpea_Q1_List)) # Add source to table

# Sort Data - Q2
Chickpea_Q2_Tidy <- data.frame(Chickpea_Q2[6]) # Select appropriate column (discard the rubbish)
colnames(Chickpea_Q2_Tidy) <- c('Trait') # Reset column name for simplicity
Chickpea_Q2_List <- Chickpea_Q2_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Chickpea_Q2_List <- Chickpea_Q2_List[!apply(Chickpea_Q2_List == "", 1, all),] # Remove empty rows
Chickpea_Q2_List <- Chickpea_Q2_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Chickpea_Q2_List$Source <- rep(c("Non-Collected"),times=nrow(Chickpea_Q2_List)) # Add source to table

# Sort Data - Q3
Chickpea_Q3_Tidy <- data.frame(Chickpea_Q3[6]) # Select appropriate column (discard the rubbish)
colnames(Chickpea_Q3_Tidy) <- c('Trait') # Reset column name for simplicity
Chickpea_Q3_Tidy$Trait <- tolower(Chickpea_Q3_Tidy$Trait)
Chickpea_Q3_List <- Chickpea_Q3_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Chickpea_Q3_List <- Chickpea_Q3_List[!apply(Chickpea_Q3_List == "", 1, all),] # Remove empty rows
Chickpea_Q3_List <- Chickpea_Q3_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Chickpea_Q3_List$Source <- rep(c("Others"),times=nrow(Chickpea_Q3_List)) # Add source to table

# Bind all Chickpea
chickpea <- rbind(Chickpea_Q1_List, Chickpea_Q2_List, Chickpea_Q3_List) # Combine data
chickpea$Species <- rep(c("Chickpea"),times=nrow(chickpea)) # Add species column
chickpea <- chickpea %>% arrange(desc(n)) # Sort by popularity


# Lentil
# Sort Data - Q1
Lentil_Q1_Tidy <- data.frame(Lentil_Q1[6]) # Select appropriate column (discard the rubbish)
colnames(Lentil_Q1_Tidy) <- c('Trait') # Reset column name for simplicity
Lentil_Q1_List <- Lentil_Q1_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Lentil_Q1_List <- Lentil_Q1_List[!apply(Lentil_Q1_List == "", 1, all),] # Remove empty rows
Lentil_Q1_List <- Lentil_Q1_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Lentil_Q1_List$Source <- rep(c("Collected"),times=nrow(Lentil_Q1_List)) # Add source to table

# Sort Data - Q2
Lentil_Q2_Tidy <- data.frame(Lentil_Q2[6]) # Select appropriate column (discard the rubbish)
colnames(Lentil_Q2_Tidy) <- c('Trait') # Reset column name for simplicity
Lentil_Q2_List <- Lentil_Q2_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Lentil_Q2_List <- Lentil_Q2_List[!apply(Lentil_Q2_List == "", 1, all),] # Remove empty rows
Lentil_Q2_List <- Lentil_Q2_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Lentil_Q2_List$Source <- rep(c("Non-Collected"),times=nrow(Lentil_Q2_List)) # Add source to table

# Sort Data - Q3
Lentil_Q3_Tidy <- data.frame(Lentil_Q3[6]) # Select appropriate column (discard the rubbish)
colnames(Lentil_Q3_Tidy) <- c('Trait') # Reset column name for simplicity
Lentil_Q3_Tidy$Trait <- tolower(Lentil_Q3_Tidy$Trait)
Lentil_Q3_List <- Lentil_Q3_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
Lentil_Q3_List <- Lentil_Q3_List[!apply(Lentil_Q3_List == "", 1, all),] # Remove empty rows
Lentil_Q3_List <- Lentil_Q3_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
Lentil_Q3_List$Source <- rep(c("Others"),times=nrow(Lentil_Q3_List)) # Add source to table

# Bind all Lentil
lentil <- rbind(Lentil_Q1_List, Lentil_Q2_List, Lentil_Q3_List) # Combine data
lentil$Species <- rep(c("Lentil"),times=nrow(lentil)) # Add species column
lentil <- lentil %>% arrange(desc(n)) # Sort by popularity


# MungBean
# Sort Data - Q1
MungBean_Q1_Tidy <- data.frame(MungBean_Q1[6]) # Select appropriate column (discard the rubbish)
colnames(MungBean_Q1_Tidy) <- c('Trait') # Reset column name for simplicity
MungBean_Q1_List <- MungBean_Q1_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
MungBean_Q1_List <- MungBean_Q1_List[!apply(MungBean_Q1_List == "", 1, all),] # Remove empty rows
MungBean_Q1_List <- MungBean_Q1_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
MungBean_Q1_List$Source <- rep(c("Collected"),times=nrow(MungBean_Q1_List)) # Add source to table

# Sort Data - Q2
MungBean_Q2_Tidy <- data.frame(MungBean_Q2[6]) # Select appropriate column (discard the rubbish)
colnames(MungBean_Q2_Tidy) <- c('Trait') # Reset column name for simplicity
MungBean_Q2_List <- MungBean_Q2_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
MungBean_Q2_List <- MungBean_Q2_List[!apply(MungBean_Q2_List == "", 1, all),] # Remove empty rows
MungBean_Q2_List <- MungBean_Q2_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
MungBean_Q2_List$Source <- rep(c("Non-Collected"),times=nrow(MungBean_Q2_List)) # Add source to table

# Sort Data - Q3
MungBean_Q3_Tidy <- data.frame(MungBean_Q3[6]) # Select appropriate column (discard the rubbish)
colnames(MungBean_Q3_Tidy) <- c('Trait') # Reset column name for simplicity
MungBean_Q3_Tidy$Trait <- tolower(MungBean_Q3_Tidy$Trait)
MungBean_Q3_List <- MungBean_Q3_Tidy %>% separate_rows(Trait, sep=";") # Pivot long by separating rows
MungBean_Q3_List <- MungBean_Q3_List[!apply(MungBean_Q3_List == "", 1, all),] # Remove empty rows
MungBean_Q3_List <- MungBean_Q3_List %>% group_by(Trait) %>% summarise(n=n()) %>% arrange(desc(n)) # Tally and sort
MungBean_Q3_List$Source <- rep(c("Others"),times=nrow(MungBean_Q3_List)) # Add source to table

# Bind all MungBean
mungbean <- rbind(MungBean_Q1_List, MungBean_Q2_List, MungBean_Q3_List) # Combine data
mungbean$Species <- rep(c("Mung Bean"),times=nrow(mungbean)) # Add species column
mungbean <- mungbean %>% arrange(desc(n)) # Sort by popularity


# Sow it all together
trait_summary <- data.frame(rbind(barley, wheat, lentil, chickpea, mungbean)) # Combine data
trait_summary$Trait <- stringr::str_replace(trait_summary$Trait, "([[:alpha:]])", toupper) #Let's make it pretty

# Save file to Excel and CSV

write_xlsx(trait_summary,"output/AGG_CoP_Trait-Summary_November-2023.xlsx")
write.csv(trait_summary,"output/AGG_CoP_Trait-Summary_November-2023.csv")