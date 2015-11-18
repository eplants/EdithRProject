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
Tally <- surveys %>%
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
Sex <- merriami %>%
  group_by(sex) %>%
  tally

# Create 2 objects looking at male weight and female weight to exclude the unkown values
Sex_male <- merriami %>%
  filter(sex == "M") %>%
  select(species, sex, weight) %>%
  filter(!is.na(weight))

Sex_female <- merriami %>%
  filter(sex == "F") %>%
  select(species, sex, weight) %>%
  filter(!is.na(weight))

#sexed <- surveys %.%
  #filter(!sex == "")
# Combine the two data sets together.
SexCombined <- rbind(Sex_female, Sex_male)

# Boxplot comparing weights of males and females
ggplot(data = SexCombined, aes(x=species, y=weight , fill=sex)) + geom_boxplot()

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

#
WeightYear <- merriami %>%
  select(year, weight) %>%
  filter(!is.na(weight))

Avg <- aggregate(WeightYear$weight, list(year = WeightYear$year), mean)

ggplot(data=Avg, aes(x=year, y= x)) + geom_point()

JD1 <- surveys %>%
  select(species, hindfoot_length) %>%
  filter(!is.na(hindfoot_length))

Avg2 <- aggregate(JD1$hindfoot_length, list(species = JD1$species), mean)
ggplot(data=Avg2, aes(x=species, y= x)) + geom_bar

ggplot(data=Avg2, aes(x=species, y=x , fill=species)) + geom_bar(stat="identity")
