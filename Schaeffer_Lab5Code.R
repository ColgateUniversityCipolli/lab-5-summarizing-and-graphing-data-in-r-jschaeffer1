#################################################################
####                      STEP 1                            #####
##################################################################
### Loading Packages
library(stringr)
library(tidyverse)
library(jsonlite)
library(xtable)

###Loading data files
artists = read.csv("data/essentia.data.csv")
allentown = read.csv("data/essentia.data.allentown.csv")

###Making a vector of all column names
columns <- artists %>% select(where(is.numeric)) %>% colnames()

### FOR LOOP TO SUMMARIZE EVERY PARAMETER ###
#Grouping together our dataframe


final_df = data.frame() #Making an empty df to store all data

for (feature in columns){
  
  ###Grouping data by artist and summarizing
  grouped = artists|>
    group_by(artist) |> #Moving on to summarize general qualities
    summarize(min = min(get(feature)),
              Q1 = quantile(get(feature), 0.25, na.rm = TRUE),
              Q3 = quantile(get(feature), 0.75, na.rm = TRUE),
              LF = Q1 - 1.5*IQR(get(feature), na.rm = TRUE),
              UF = Q3 + 1.5*IQR(get(feature), na.rm = TRUE),
              max = max(get(feature))
    ) |> 
    
    #Adding new column to check if Allentown is within bounds
    mutate(out.of.range = (get(feature, allentown) > max | 
                             get(feature, allentown) < min)) |>
    mutate(unusual = (get(feature, allentown) > UF |
                        get(feature, allentown) < LF)) |>
    
    #Adding a new column to describe properties of where Allentown lies in bounds
    mutate(description = case_when(out.of.range == TRUE ~ "Out of Range",
                                   unusual == TRUE ~ "Outlying",
                                   TRUE       ~ "Within Range")) |>
    mutate(feature = feature)
  
  final_df = rbind(final_df, grouped) #Combining the new feature into final dataframe
  
}
### Reorganizing dataframe - making feature second and description third
final_df <- final_df %>%
  select(artist, feature, description, everything())

############################################################################
##########              STEP TWO: Analyzing the Data              ##########
############################################################################
###Tallying up how many times Allentown is within bounds for each artist
bounded_count = final_df %>%
  count(artist, description)


class(bounded_count)


view(bounded_count)

view(artists)
view(final_df)



