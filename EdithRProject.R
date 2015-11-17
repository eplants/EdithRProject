## Edith Plants-Paris' Survey project in R ##

# Install and load dplyr and ggplot2
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Download data
download.file("http://files.figshare.com/2236372/combined.csv", "data/portal_data_joined.csv")

# Import Data
surveys <- read.csv('data/portal_data_joined.csv')
class(surveys)
str(surveys)

# Devise and implement stats and figures for this data
# Figures
  # TAXA
  # Relationship sex and weight by species?
  # Avg Species weight versus year?   
  # Avg Hindfoot length vs avg weight by species
  # Hindfoot length vs sex by species
  # Distribution male/ female weight
  
# relationship  point, line
# Distr.   histogram, bar
# Comparison among items, over time    bar, boxplot, line

# Relationship between avg species weight and year

WeightYear <- Surveys %>% 
  select(species, sex, weight) %>%
  group_by(species) %>%
  group_by(sex) %>%
  filter(!is.na(weight)) %>%
  filter(!is.na(sex))
  
ggplot(data=surveys, aes(x=species, y= weight)) + geom_boxplot()
