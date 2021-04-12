library(tidyverse)

temp_sizes_combined <- read.csv("data/temp_sizes_combined.csv")


harvestable_mass <- temp_sizes_combined %>%
  mutate(total = yr0 + yr1 + yr2)
total_cut <- harvestable_mass$total[1:42]

## manually enter summary_tab$mass_totals[15] if not loaded (total mass in kilos) - 15608750 in autumn 2019

total_mass_tonnes <- mass_intervals_c_filtered[1,5]

est_mass <-  exp((log(harvestable_mass$size[1:42])* 3.315)-8.939)
size.biomass <- harvestable_mass$total[1:42] * est_mass


size.biomass <- total_cut*est_mass

size.biomass.sum <- sum(size.biomass)

bio.relfreq <- size.biomass/size.biomass.sum

bio.cumfreq <- cumsum(bio.relfreq)

cum.biomass <- bio.cumfreq*total_mass_tonnes
inv.bio <- 1-bio.cumfreq

inv.mass <- inv.bio*total_mass_tonnes

cu_mass_df <- data.frame(harvestable_mass$size[1:42], cum.biomass, inv.mass)

cu_h_mass <- cu_mass_df$inv.mass[17]

cu_hor_mass <- bio.cumfreq[17]

cu_harvest_mass <- ggplot(cu_mass_df, aes(harvestable_mass$size[1:42],inv.mass)) + geom_point(col = "#a6611a") + geom_line(col = "#a6611a") + geom_vline(xintercept = 20, col = "#dfc27d", linetype = "dashed", alpha = 0.9) + geom_hline(yintercept = cu_h_mass, col = "#dfc27d", linetype = "dashed", alpha = 0.9 ) + theme_minimal() + labs(x= "Length mm", y = "Biomass Tonnes")

# Open a pdf file
pdf("plots/harvest_mass.pdf") 

# 2. Create a plot
cu_harvest_mass

# Close the pdf file
dev.off() 

write.csv(cu_mass_df, "tabs/cu_mass_df.csv")
