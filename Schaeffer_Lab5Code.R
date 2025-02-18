#################################################################
####                      STEP 1                            #####
##################################################################
### Loading Packages
library(stringr)
library(tidyverse)
library(jsonlite)

###Loading data files
artist.data = read.csv("data/essentia.data.csv")
allentown.data = read.csv("data/essentia.data.allentown.csv")
IQR(grouped.data$overall_loudness)

###Grouping data by artist and summarizing
grouped.data = artist.data|>
  group_by(artist) |>
  summarize(min = min(overall_loudness),
            Q1 = quantile(overall_loudness, 0.25),
            Q3 = quantile(overall_loudness, 0.75),
            LF = Q1 - 1.5*IQR(overall_loudness),
            UF = Q3 + 1.5*IQR(overall_loudness),
            max = max(overall_loudness)
            )




