#load library
library(tidyverse)

# import data
cockle_size <- read.csv("data/cockle_mass_dee.csv")

# set pre-defined line for Burry
c.l = c(4:40)

e.m = exp((log(c.l)* 3.315)-8.939)

temp_df <- data.frame(c.l, e.m)



p <- cockle_size %>%
  ggplot(aes(x = length, y = mass)) +
  geom_point()

p + geom_line(data = temp_df, aes(x = c.l, y = e.m), color = "#91bfdb")


c.l = c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)

e.m = exp((log(c.l)* 3.315)-8.939)

temp_df <- data.frame(c.l, e.m)

size_plot <- p + geom_point() +
  geom_line(data = temp_df, aes(x = c.l, y = e.m), color = "#91bfdb")

# Open a pdf file
pdf("plots/size_plot.pdf") 

# 2. Create a plot
size_plot

# Close the pdf file
dev.off() 


sm_19_df %>% mutate(cubed = mass_19^(1/3)) %>% ggplot(aes(length_19,cubed)) + geom_point()

p + geom_point() +
  geom_line(data = temp_df, aes(x = c.l, y = e.m), color = "#91bfdb") + scale_y_continuous(trans = "log2") + scale_x_continuous(trans = "log2")
