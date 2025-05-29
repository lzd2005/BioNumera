# Load required libraries
library(tidyverse)
library(ggplot2)
library(betareg)
library(MuMIn)
library(vegan)

# Read the dataset
dataset <- read.csv("Journal of Applied Ecology - 2024 - Horstmann - Traffic-Dataset_Horstmann_et_al_2024.csv")

# Data Wrangling and Exploration
# Check the structure of the dataset to understand variables
str(dataset)

# Summary statistics for key variables
summary(dataset[, c("BF_Abun", "BF_Rich", "WB_Abun", "WB_Rich", "TI")])

# Data Visualization
# Plot of wild bee abundance vs. traffic intensity
ggplot(dataset, aes(x = TI, y = WB_Abun)) +
  geom_point() +
  scale_x_log10() +
  labs(x = "Traffic Intensity (vehicles per day)",
       y = "Wild Bee Abundance",
       title = "Wild Bee Abundance vs. Traffic Intensity")

# Plot of butterfly richness vs. plant richness
ggplot(dataset, aes(x = Vasc_Rich, y = BF_Rich)) +
  geom_point() +
  labs(x = "Vascular Plant Richness",
       y = "Butterfly Richness",
       title = "Butterfly Richness vs. Plant Richness")

# Statistical Analysis
# Model for wild bee abundance considering traffic intensity and flower density
bee_model <- glm(WB_Abun ~ TI + Flow_Dens + Vasc_Rich, data = dataset, family = poisson())
summary(bee_model)

# Model for butterfly richness considering traffic intensity and road verge classification
butterfly_model <- glm(BF_Rich ~ TI + RV_Quality, data = dataset, family = gaussian())
summary(butterfly_model)

# Backward model selection to identify the best model
bee_model_selection <- dredge(bee_model)
summary(bee_model_selection)

# Beta regression for butterfly evenness considering traffic intensity and plant richness
butterfly_evenness_model <- betareg(BF_Evenness ~ TI + Vasc_Rich, data = dataset)
summary(butterfly_evenness_model)

# NMDS analysis for plant community composition
plant_community <- dataset[, 35:242] # Columns 35 to 242 are plant species frequency
plant_distance <- vegdist(plant_community, method = "bray")
plant_nmds <- metaMDS(plant_distance)
plot(plant_nmds)

# Comparing plant community composition across different road verge categories
plant_community$Category <- dataset$RV_Quality
adonis(plant_distance ~ Category, data = plant_community)

# Save the models and plots for reporting
saveRDS(bee_model, "bee_model.rds")
saveRDS(butterfly_model, "butterfly_model.rds")
ggsave("wild_bee_abundance_vs_traffic.png")
ggsave("butterfly_richness_vs_plant_richness.png")