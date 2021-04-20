# load libraries
library(tidyverse)

# import data
temp_sizes <- read.csv("data/temp_sizes.csv")


# spread data by yearclass
wide_temp_sizes <- temp_sizes %>%
  spread(year_class, quantity) %>%
  mutate(total = year0 + year1 + year2)

# don't filter each group
md_sizes <- wide_temp_sizes

# filter each group
md_sizes <- wide_temp_sizes %>%
  filter(Block == "Mostyn Deep")

cockle_md <- rep(c(md_sizes$length), times = md_sizes$total)
cockle_md_df <- data.frame(cockle_md)
cockle_ecdf <- data.frame(x=unique(cockle_md_df$cockle_md),  y=ecdf(cockle_md_df$cockle_md)(unique(cockle_md_df$cockle_md))*length(cockle_md_df$cockle_md))
cockle_ecdf$y <- scale(cockle_ecdf$y,center=min(cockle_ecdf$y),scale=diff(range(cockle_ecdf$y)))
hl <- cockle_ecdf$y[25]

cu_p <- ggplot(cockle_md_df, aes(cockle_md)) + stat_ecdf(geom ="point", col = "#a6611a") + geom_vline(xintercept = 32, col = "#dfc27d", linetype = "dashed", alpha = 0.9) + geom_hline(yintercept = hl, col = "#dfc27d", linetype = "dashed", alpha = 0.9 ) + geom_line(data = cockle_ecdf, aes(x, y, col =  "#a6611a"),show.legend = FALSE) + theme_minimal() + labs(x= "Length mm", y = "Cumulative Frequency")

cu_p

# Open a pdf file
pdf("plots/ecdf.pdf") 

# 2. Create a plot
cu_p

# Close the pdf file
dev.off() 

# calculate frequency of harvestable cockles (based on 32mm being retained by riddle)
freq_hl <- 1- hl

# write to csv file
write.csv(freq_hl, "tabs/freq_hl.csv")
