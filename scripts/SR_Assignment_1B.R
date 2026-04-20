# Introduction to tidyverse
# Using Iris dataset for data wrangling and visualization
#

# Load libraries and data
library(tidyverse)
data(iris)

head(iris)
str(iris)

# Create tibble
df <- tibble::as_tibble(iris)
df

# Filter examples
filter(df, Species == "versicolor")

filter(df, Petal.Length > 2)

filter(df, Petal.Length > 6 & Sepal.Length > 7)

# Arrange examples
arrange(df, Sepal.Length, Petal.Width)

arrange(df, desc(Sepal.Length))

# Select columns
select(df, Species, Petal.Width, Petal.Length)

# Mutate - add new column
mutate(df, log.Sepal.length = log(Sepal.Length))

# Summarise examples
summarise(df, mean(Petal.Length))

group_by(df, Species) %>% count(n())

df %>% 
  group_by(Species) %>%
  summarise(mean(Petal.Length))

# VISUALIZATIONS with titles and axis labels

# 1) Scatter Plot
ggplot(data=df, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length (cm)") +
  ylab("Sepal Width (cm)") +
  ggtitle("Sepal Length vs Sepal Width by Species") +
  theme_minimal()

# 2) Box Plot
box <- ggplot(data=df, aes(x=Species, y=Sepal.Length))

box + 
  geom_boxplot(aes(fill=Species)) +
  ylab("Sepal Length (cm)") +
  xlab("Species") +
  ggtitle("Sepal Length Distribution by Species") +
  stat_summary(fun=mean, geom="point", shape=5, size=4, color="purple") +
  theme_minimal()

# 3) Histogram
histogram <- ggplot(data=df, aes(x=Sepal.Width))

histogram +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +
  xlab("Sepal Width (cm)") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Sepal Width by Species") +
  theme_minimal()

# 4) Bar plot
bar <- ggplot(data=df, aes(x=Species))

bar +
  geom_bar(aes(fill=Species)) + 
  xlab("Species") + 
  ylab("Count") +
  ggtitle("Count of Iris Flowers by Species") +
  theme_minimal()

# 5) Faceting
facet <- ggplot(data=df, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(aes(shape=Species), size=1.5) +
  xlab("Sepal Length (cm)") +
  ylab("Sepal Width (cm)") +
  ggtitle("Sepal Dimensions by Species: Faceted") +
  theme_minimal()

# Facet along columns
facet + facet_grid(. ~ Species)

renv::snapshot() 
