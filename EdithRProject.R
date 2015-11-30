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
Weight <- merriami %>%
  select(species, weight) %>%
  filter(!is.na(weight)) 

# Option 1 Create a histogram looking at the distribution of weight
# Also saves the figure as a pdf
pdf("figures/scatterplot.pdf")
ggplot(data = Weight, aes(x=weight)) + geom_histogram(aes(fill = ..count..),binwidth=1) + scale_fill_gradient("Count", low = "#ff0099", high = "#9900cc") + theme_bw() + ylab("Frequency") + xlab("Weight(g)") + scale_x_continuous(breaks=seq(0,100,5)) + scale_y_continuous(breaks=seq(0,800,100)) + ggtitle("Distribution of weight within D.merriami") + theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off()

# Option 2, in case option 1 does not work
pdf("figures/scatterplot.pdf")
qplot(weight, data = weight, ylab= "Frequency", xlab="Weight", binwidth = 1, geom="histogram")
dev.off()

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
# Also saves the figure as a pdf
pdf("figures/scatterplot.pdf")
ggplot(data = Sex_comparison, aes(x=species, y=weight , fill=sex)) + geom_boxplot() + scale_fill_manual(values=c("#ff0099", "#0099ff")) + theme_bw() + ylab("Weight (g)") + xlab("Species") + ggtitle("Comparison of weights between male and female D. merriami") + theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off()

# Create an object to look at Weight over time
WeightYear <- merriami %>%
  select(year, weight) %>%
  filter(!is.na(weight))

# Find the average weight of the species each year
Avg <- aggregate(WeightYear$weight, list(year = WeightYear$year), mean)

# Create a scatterplot to look at the mean weight per year and create a Add linear regression line that includes 95% confidence region
# Also saves the figure as a pdf
pdf("figures/scatterplot.pdf")
ggplot(data=Avg, aes(x=year, y= x)) + geom_point(shape=8,size=5, colour="#9900cc") + geom_smooth(method=lm, colour="#9900cc") + ylab("Mean Weight (g)") + xlab("Year") + theme_bw()
dev.off()

