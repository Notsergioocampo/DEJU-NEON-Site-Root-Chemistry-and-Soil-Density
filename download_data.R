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




# Load the soil chemistry dataset (DP1.00096.001)
soil_data <- loadByProduct(dpID = "DP1.00096.001",
                           site = c("DEJU"),          # Replace or expand with other sites if needed
                           check.size = FALSE)

# Extract the soil chemical properties table
# This will likely be named "slc_soilChemistry" but you can check names(soil_data) to confirm
soil_chem <- soil_data[["slc_soilChemistry"]]

# Save the dataset to CSV
write.csv(soil_chem, "data/raw_data/soil_chemistry.csv", row.names = FALSE)

# Reload from CSV if needed later
soil_chem <- read.csv("data/raw_data/soil_chemistry.csv")



          
