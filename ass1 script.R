#install.packages("tidyverse")
library(tidyverse)

fastfood <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")
head(fastfood)
head(fastfood, 10)
#install.packages("knitr")
library(knitr)
kable(fastfood[1:10, 1:5])