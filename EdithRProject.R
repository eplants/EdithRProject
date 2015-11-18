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

# Option 1 Create a histogram looking at the distribution of weight
ggplot(data = weight, aes(x=weight)) + geom_histogram(binwidth=1)

# Option 2, in case option 1 does not work
qplot(weight, data = weight, ylab= "Frequency", xlab="Weight", binwidth = 1, geom="histogram")

# Create an object to compare weights of males and females
Sex <- merriami %>%
  group_by(sex) %>%
  tally

# Create an object looking at male weight and female weight to exclude the unkown values
Sex_comparison <- merriami %>%
  filter(!sex == "") %>%
  select(species, sex, weight) %>%
  filter(!is.na(weight))

# Run t-test to see if there is a difference between the mean weight of sexes
t.test(weight ~ sex, data=Sex_comparison)

# Boxplot comparing weights of males and females
ggplot(data = Sex_comparison, aes(x=species, y=weight , fill=sex)) + geom_boxplot()

# Create an object to look at Weight over time
WeightYear <- merriami %>%
  select(year, weight) %>%
  filter(!is.na(weight))

# Find the average weight of the species each year
Avg <- aggregate(WeightYear$weight, list(year = WeightYear$year), mean)

# Create a scatterplot to look at the mean weight per year and create a Add linear regression line that includes 95% confidence region
pdf("figures/scatterplot.pdf")
ggplot(data=Avg, aes(x=year, y= x)) + geom_point() + geom_smooth(method=lm) + ylab("Mean Weight (g)") + xlab("Year")
dev.off()

