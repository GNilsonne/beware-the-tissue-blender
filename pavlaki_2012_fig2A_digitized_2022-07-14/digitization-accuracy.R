library(tidyverse)

data <- read.csv("./pavlaki_2012_fig2A_digitized_2022-07-14.csv")

# Add a new column with rounded age values 
# and the absolute difference between raw age and rounded value

data <- data %>% 
  mutate(age_rounded=round(as.numeric(age))) %>% 
  mutate(age_diff_abs=age - age_rounded)

summarise(data, 
          age_diff_abs_mean=mean(age_diff_abs), 
          age_diff_abs_range=range(age_diff_abs),
          age_diff_abs_sd=sd(age_diff_abs))

ggplot(data, aes(x=age_diff_abs)) + geom_histogram()
