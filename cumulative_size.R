library(tidyverse)

temp_sizes_combined <- read.csv("data/temp_sizes_combined.csv")

cumulative_mass <- temp_sizes_combined %>%
  mutate(total = yr0 + yr1 + yr2)


total_cut <- cumulative_mass$total[1:42]



## manually enter summary_tab$mass_totals[15] if not loaded (total mass in kilos) - 15608750 in autumn 2019

total_mass_tonnes <- mass_intervals_c_filtered[1,5]

est_mass <-  exp((log(cumulative_mass$size[1:42])* 3.315)-8.939)

size.biomass <- total_cut*est_mass

size.biomass.sum <- sum(size.biomass)

bio.relfreq <- size.biomass/size.biomass.sum

bio.cumfreq <- cumsum(bio.relfreq)

cum.biomass <- bio.cumfreq*total_mass_tonnes

inv.bio <- 1-bio.cumfreq

inv.mass <- inv.bio*total_mass_tonnes
cu_mass_df <- data.frame(cumulative_mass$size[1:42], cum.biomass, inv.mass)

cu_h_mass <- cu_mass_df$inv.mass[17]

cu_hor_mass <- bio.cumfreq[17]

cu_mass <- ggplot(cu_mass_df, aes(cumulative_mass$size[1:42],bio.cumfreq)) + geom_point(col = "#a6611a") + geom_line(col = "#a6611a") + geom_vline(xintercept = 20, col = "#dfc27d", linetype = "dashed", alpha = 0.9) + geom_hline(yintercept = cu_hor_mass, col = "#dfc27d", linetype = "dashed", alpha = 0.9 ) + theme_minimal() + labs(x= "Length mm", y = "Cumulative Relative Biomass")
cu_mass
# Open a pdf file
pdf("plots/cumulative_mass.pdf") 

# 2. Create a plot
cu_mass

# Close the pdf file
dev.off() 