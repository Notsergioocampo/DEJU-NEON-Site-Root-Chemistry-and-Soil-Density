# A script to download some NEON data and save it to a csv file
# Sergio O.
# April 22, 2025
# socampo3@dons.usfca.edu
# Load neonUtilities package
library("neonUtilities")
# -------------------------------
# Root biomass and chemistry data
# -------------------------------
# Download, stack, and load into memory
megapit_data <- loadByProduct(dpID = "DP1.10066.001",
                              site = c("DEJU"),
                              check.size = FALSE)
# Extract biomass and carbon-nitrogen data frames
megapit_biomass <- megapit_data[["mpr_perrootsample"]][["incrementRootBiomass"]]
megapit_cn <- megapit_data[["mpr_carbonNitrogen"]]
# Save to CSV
write.csv(megapit_biomass, "data/raw_data/megapit_biomass.csv",
          row.names = FALSE)
write.csv(megapit_cn, "data/raw_data/megapit_carbon_nitrogen.csv",
          row.names = FALSE)
# Extract and save root sample metadata (used for sizeCategory)
root_sample <- megapit_data[["mpr_perrootsample"]]
if (nrow(root_sample) > 0) {
  write.csv(root_sample, "data/raw_data/megapit_root_samples.csv",
            row.names = FALSE)
} else {
  message("⚠️ No data in 'mpr_perrootsample'. Writing placeholder.")
  write.csv(data.frame(cnSampleID = character(),
                       sampleID = character(),
                       sizeCategory = character()),
            "data/raw_data/megapit_root_samples.csv",
            row.names = FALSE)
}
# -------------------------------
# Soil bulk density data
# -------------------------------
# Download and load bulk density data
soil_data <- loadByProduct(dpID = "DP1.00096.001",
                           site = c("DEJU"),
                           check.size = FALSE)
# Extract relevant table
soil_bulk <- soil_data[["mgp_perbulksample"]]
# Save to CSV
write.csv(soil_bulk, "data/raw_data/soil_bulk_density.csv",
          row.names = FALSE)
