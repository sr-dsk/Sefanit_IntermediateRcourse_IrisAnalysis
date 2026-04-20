#Step 3: installing and initializing packages
install.packages('tidyverse')
library(tidyverse)

#Save the state of library
renv::snapshot()

#Load and explore IRIS data
data("iris")
head(iris)
summary(iris)
str(iris)
iris <- as_tibble(iris)
iris

#summarize mean of all numeric columns
iris %>% 
  summarize_if(is.numeric, mean)

#Packages for plotting
install.packages("ggplot2")
install.packages("GGally")
library(ggplot2)
library(GGally)

#Data quality check, including aggregation and sampling
ggpairs(iris, aes(color = Species))

clean.data <- iris %>% drop_na() %>% unique()
summary(clean.data)

iris %>% group_by(Species) %>% 
  summarize_all(mean)
iris %>% group_by(Species) %>% 
  summarize_all(median)

#Random Sampling
sample(c("A", "B", "C"), size = 10, replace = TRUE)
take <- sample(seq(nrow(iris)), size = 15)
take

iris[take, ]

set.seed(1000)

s <- iris %>% slice_sample(n = 15)
ggpairs(s, aes(color = Species))

#Stratified Sampling
install.packages("sampling")
library(sampling)
id2 <- strata(iris, stratanames = "Species", size = c(5,5,5), method = "srswor")
id2

s2 <- iris %>% slice(id2$ID_unit)
ggpairs(s2, aes(color = Species))

#Dimensionality reductions 
plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Sepal.Width,
                size = ~Petal.Width, color = ~Species, type="scatter3d")
pc <- iris %>% select(-Species) %>% as.matrix() %>% prcomp()
summary(pc)

plot(pc, type = "line")
str(pc)

iris_projected <- as_tibble(pc$x) %>% add_column(Species = iris$Species)
ggplot(iris_projected, aes(x = PC1, y = PC2, color = Species)) + 
  geom_point()

ggplot(iris_projected, 
       aes(x = PC1, y = 0, color = Species)) + 
  geom_point() +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_text('Iris Project PC1')
  )

#Factoextra plotting
install.packages("factoextra")
library(factoextra)
fviz_pca(pc)
fviz_pca_var(pc)
d <- iris %>% select(-Species) %>% dist()
fit <- cmdscale(d, k = 2)
colnames(fit) <- c("comp1", "comp2")
fit <- as_tibble(fit) %>% add_column(Species = iris$Species)

ggplot(fit, aes(x = comp1, y = comp2, color = Species)) + geom_point()

#Discrete features 
ggplot(iris, aes(x = Petal.Width)) + geom_histogram(binwidth = .1)
iris %>% pull(Sepal.Width) %>% cut(breaks = 3)

install.packages("arules", type = "binary")
library(arules)
iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3)
iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3)
iris %>% pull(Petal.Width) %>% discretize(method = "cluster", breaks = 3)

#vertical lines drawn at each interval deterimined by discretize function and divides petal.width variable 
#into 3 equal intervals
ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3, onlycuts = TRUE),
             color = "red") +
  labs(title = "Discretization: interval", subtitle = "Here, the red lines are boundaries")

#again the veritcal lines are drawn at the boundaries created by discretize function, this time taking the
#petal.width column to create 3 boundaries based on the frequency of the values
ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3, onlycuts = TRUE),
             color = "red") +
  labs(title = "Discretization: frequency", subtitle = "Here, the red lines are boundaries") 

#Standardize Data
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

iris.scaled <- iris %>% scale_numeric()
iris.scaled
summary(iris.scaled)

#Proximites - Similarities and distances 
#Minkowsky Distances
iris_sample <- iris.scaled %>% select(-Species) %>% slice(1:5)
iris_sample
dist(iris_sample, method = "euclidean")
dist(iris_sample, method = "manhattan")
dist(iris_sample, method = "maximum")

#distances for binary data
b_data <- rbind(
  c(0,0,0,1,1,1,1,0,0,1),
  c(0,0,1,1,1,0,0,1,0,0)
)
b_data

b_data_logical <- apply(b, MARGIN = 2, as.logical)
b_data_logical
dist(b_data, method = "manhattan")
dist(b_data, method = "euclidean")^2
dist(b_data, method = "binary")
people <- tibble(
  height = c(      160,    185,    170),
  weight = c(       52,     90,     75),
  sex    = c( "female", "male", "male")
)
people

install.packages("proxy")
install.packages("caret")
library(proxy)
library(caret)

data_test<- dummyVars(~., people) %>% predict(people)
data_test
weight_matrix <- matrix(c(1, 1, 1/2, 1/2), ncol = 4, nrow = nrow(data_test), byrow = TRUE)
data_test_scaled <- scale(data_test) * weight_matrix

d_test <- dist(data_test_scaled)
d_test

#More proximity measures for package proxy
pr_DB$get_entry_names()

#Relationships between features
#Correlation
cc <- iris %>% select(-Species) %>% cor()
cc

ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  geom_point() +
  geom_smooth(method = "lm")

with(iris, cor(Petal.Length, Petal.Width))

#Rank correlation
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), ordered = TRUE))

iris_ord
summary(iris_ord)

iris_ord %>% pull(Sepal.Length)
iris_ord %>% pull(Sepal.Width)
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "kendall")
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "spearman")
iris %>% select(-Species) %>% cor()

#Density Estimation
ggplot(iris, aes(x = Petal.Length, y = 0)) + geom_point()

#Histograms
ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram() +
  geom_rug(alpha = 1/2)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_bin2d(bins = 10) +
  geom_jitter(color = "orange")

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_hex(bins = 15) +
  geom_jitter(color = "orange")

#Kernel density estimate
ggplot(iris, aes(Petal.Length)) +
  geom_density(bw = .2) +
  geom_rug(alpha = 1/2) +
  labs(
    title = "Kernel density estimate",
    x = "Petal length",
    y = "Density"
  )

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_density_2d_filled() +
  geom_jitter()

# Exploring Iris Dataset: Complete Tutorial Script
# Load required libraries
library(tidyverse)
library(GGally)
library(ggcorrplot)
library(seriation)
library(tidyr)

# Basic statistics
summary(iris)

iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% sd()

mean(c(1, 2, NA, 3, 4, 5))
mean(c(1, 2, NA, 3, 4, 5), na.rm = TRUE)

iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% mean(trim = .1)

iris %>% summarize_if(is.numeric, mean)
iris %>% summarize_if(is.numeric, sd)
iris %>% summarize_if(is.numeric, list(min = min, median = median, max = max))
iris %>% summarize_if(is.numeric, mad)

# Grouping
iris %>% group_by(Species) %>% summarize(across(Sepal.Length, mean))
iris %>% group_by(Species) %>% summarize_all(mean)

res.aov <- aov(Sepal.Length ~ Species, data = iris)
summary(res.aov)
TukeyHSD(res.aov)

iris %>% group_by(Species) %>% summarize(n())

# Tabulate data
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), ordered = TRUE))
iris_ord
summary(iris_ord)

tbl <- iris_ord %>% select(Sepal.Length, Species) %>% table()
tbl

iris_ord %>%
  select(Species, Sepal.Length) %>%
  pivot_longer(cols = Sepal.Length) %>%
  group_by(Species, value) %>% count() %>% ungroup() %>%
  pivot_wider(names_from = Species, values_from = n)

tbl %>% chisq.test()
fisher.test(tbl)

# Percentiles (Quantiles)
iris %>% pull(Petal.Length) %>% quantile()
iris %>% summarize(IQR = quantile(Petal.Length, probs = 0.75) - quantile(Petal.Length, probs = 0.25))

# Visualization - Histogram
ggplot(iris, aes(Petal.Width)) + 
  geom_histogram(bins = 20) +
  labs(title = "Petal Width Distribution", x = "Petal Width (cm)", y = "Frequency")

# Boxplot
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot() +
  labs(title = "Sepal Length by Species", x = "Species", y = "Sepal Length (cm)")

iris %>% group_by(Species) %>% summarize_if(is.numeric, median)

library(tidyr)
iris_long <- iris %>% mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long, aes(name, value)) + 
  geom_boxplot() +
  labs(title = "Feature Distributions", x = "Measurement", y = "Original Value (cm)")

iris_long_scaled <- iris %>% scale_numeric() %>% mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long_scaled, aes(name, value)) + 
  geom_boxplot() +
  labs(title = "Scaled Feature Distributions", x = "Measurement", y = "Scaled Value")

# Scatterplot
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point() +
  labs(title = "Petal Length vs Petal Width", x = "Petal Length (cm)", y = "Petal Width (cm)")

# Scatterplot matrix
ggpairs(iris, aes(color = Species))

# Data matrix visualization
iris_matrix <- iris %>% select(-Species) %>% as.matrix()
iris_long <- as_tibble(iris_matrix) %>% mutate(id = row_number()) %>% pivot_longer(1:4)
head(iris_long)

ggplot(iris_long, aes(x = name, y = id, fill = value)) + 
  geom_tile() +
  labs(title = "Iris Data Matrix", x = "Feature", y = "Observation", fill = "Value")

library(seriation)
ggpimage(iris_matrix, prop = FALSE)
iris_scaled <- scale(iris_matrix)
ggpimage(iris_scaled, prop = FALSE)
ggpimage(iris_scaled, order = seriate(iris_scaled), prop = FALSE)

# Correlation Matrix
cm1 <- iris %>% select(-Species) %>% as.matrix %>% cor()
cm1
library(ggcorrplot)
ggcorrplot(cm1) +
  labs(title = "Correlation Matrix")

gghmap(cm1, prop = TRUE)

cm2 <- iris %>% select(-Species) %>% as.matrix() %>% t() %>% cor()
ggcorrplot(cm2) +
  labs(title = "Transposed Correlation Matrix")

# Parallel Coordinates Plot
ggparcoord(iris, columns = 1:4, groupColumn = 5) +
  labs(title = "Parallel Coordinates Plot")

o <- seriate(as.dist(1-cor(iris[,1:4])), method = "BBURCG")
get_order(o)
ggparcoord(iris, columns = get_order(o), groupColumn = 5) +
  labs(title = "Seriation Optimized Parallel Coordinates")
