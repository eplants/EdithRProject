## Edith Plants-Paris' Survey project in R ##

# Install and load dplyr and ggplot2
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Download data
download.file("http://files.figshare.com/2236372/combined.csv", "data/portal_data_joined.csv")

# Import Data and do an overview
surveys <- read.csv('data/portal_data_joined.csv')
str(surveys)

# Look at the survey data and the number of values for each species
  group_by(species) %>%
  tally

# Create a new object looking at only the values for species merriami
# Trim number of columns
merriami <- surveys %>%
  filter(species == "merriami") %>%
  select(genus, species, year, sex, hindfoot_length, weight)

# Create an object to look at the distribution of weight 
weight <- merriami %>%
  select(species, weight) %>%
  filter(!is.na(weight)) 

# Create a histogram looking at the distribution of weight
ggplot(data = weight, aes(x=species)) + geom_histogram()

# Create an object to compare weights of males and females

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

# Export figures

# Relationship between avg species weight and year



WeightYear <- surveys %>% 
  select(species, sex, weight) %>%
  group_by(species) %>%
  group_by(sex) %>%
  filter(!is.na(weight)) %>%
  filter(!is.na(sex))
  
ggplot(data=surveys, aes(x=species, y= weight)) + geom_boxplot()
