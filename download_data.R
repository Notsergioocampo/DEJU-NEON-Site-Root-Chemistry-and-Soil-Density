# A script to download some NEON data and save it to a csv file
# Sergio O.
# April 22, 2025
# socampo3@dons.usfca.edu

# load neonUtilities package
library("neonUtilities")

# download, stacking, and loading into memory
# the dataset we want to use
# Root biomass and chemistry, Megapit DP1.10066.001
megapit_data <- loadByProduct(dpID = "DP1.10066.001",
                                      site = c("DEJU"),
                                      check.size = FALSE)


# Old Code
# Choose the correct data frame (adjust this if you want something else)

# For example, you might be looking for 'megapitRootBiomass'
megapit_biomass <- megapit_data$megapitRootBiomass

# Save it as CSV
write.csv(megapit_biomass, "data/raw_data/megapit_biomass.csv")


# New Set
# Extract nitrogen data from carbon-nitrogen table
megapit_cn <- megapit_data[["mpr_carbonNitrogen"]]

# Save to CSV for future reference or sharing
write.csv(megapit_cn, "data/raw_data/megapit_carbon_nitrogen.csv", row.names = FALSE)

# Load it back in later (when you're starting from saved data)
megapit_cn <- read.csv("data/raw_data/megapit_carbon_nitrogen.csv")



# Plot Time! Nitrogen % Distrubution 

library(ggplot2)

ggplot(megapit_cn, aes(x = nitrogenPercent)) +
  geom_histogram(binwidth = 0.2, fill = "#4CAF13", color = "black") +
  labs(title = "Distribution of Nitrogen Content in Roots",
       x = "Nitrogen Percent",
       y = "Number of Samples") +
  theme_minimal()

# Exploring Carbon Vs Nitrogen for Correlation 
ggplot(megapit_cn, aes(x = carbonPercent, y = nitrogenPercent)) +
  geom_point(alpha = 0.6, color = "#FF9800") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Carbon vs Nitrogen in Megapit Root Samples",
       x = "Carbon Percent",
       y = "Nitrogen Percent") +
  theme_minimal()

# Calculating C:N ratio

megapit_cn$cn_ratio <- megapit_cn$carbonPercent / megapit_cn$nitrogenPercent
ggplot(megapit_cn, aes(x = cn_ratio)) +
  geom_histogram(binwidth = 2, fill = "#9C27B0", color = "black") +
  labs(title = "Carbon:Nitrogen Ratio Distribution",
       x = "C:N Ratio",
       y = "Count") +
  theme_minimal()


          
