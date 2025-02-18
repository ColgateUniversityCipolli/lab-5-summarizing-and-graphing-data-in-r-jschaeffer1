#################################################################
####                      STEP 1                            #####
##################################################################
### Loading Packages
library(stringr)
library(tidyverse)
library(jsonlite)

###Loading data files
artist.data = read.csv("data/essentia.data.csv")
allentown = read.csv("data/essentia.data.allentown.csv")
IQR(grouped.data$overall_loudness)

###Grouping data by artist and summarizing
grouped.data = artist.data|>
  group_by(artist) |> #Moving on to summarize general qualities
  summarize(min = min(overall_loudness),
            Q1 = quantile(overall_loudness, 0.25),
            Q3 = quantile(overall_loudness, 0.75),
            LF = Q1 - 1.5*IQR(overall_loudness),
            UF = Q3 + 1.5*IQR(overall_loudness),
            max = max(overall_loudness)
            ) |> 
  #Adding new column to check if Allentown is within bounds
  mutate(out.of.range = (allentown$overall_loudness > max | 
                           allentown$overall_loudness < min)) |>
  mutate(unusual = (allentown$overall_loudness > UF |
                      allentown$overall_loudness < LF)) |>
  #Adding a new column to describe properties of where Allentown lies in bounds
  mutate(description = case_when(out.of.range == TRUE ~ "Out of Range",
                                               unusual == TRUE ~ "Outlying",
                                               TRUE       ~ "Within Range"))





view(grouped.data)
view(allentown)




