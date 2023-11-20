#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyverse")

library(readxl)
library(ggplot2)
df <- read_excel("data/distance.dataset.xlsx", sheet=1, col_names = TRUE, col_types = c("numeric", "text", "text", "numeric"))
df$structure <- gsub("\\r\\n", "", df$structure)

#### Descriptive Statistics: #### 
descriptive_stats <- function(type) {
  if(missing(type)) {
    input <- df$distance
  }else{
    input <- df$distance[df$structure==type]
  }
  
  output <- c(
  paste("mean:", mean(input)),
  paste("median:", median(input)),
  paste("standard deviation", sd(input)),
  paste("IQR:", IQR(input)),
  paste("Min:", min(input)),
  paste("Max:", max(input))
  )
  
  print(output)
  print(quantile(input))
}

descriptive_stats()
descriptive_stats("overt connective")
descriptive_stats("juxtaposition")
descriptive_stats("elliptical")

#### Freq Histograms: #### 
freq_histogram <- function(type) {
  if(missing(type)) {
    input <- df$distance
    text <- paste("Distance (n) of all structures")
  } else {
    input <- df$distance[df$structure==type]
    text <- paste("Distance (n) of structure", type)
  }
  hist(input,
       col = "green",
       breaks = seq(0, length(input), by = 5),
       freq = TRUE,
       xlab = text,
       main=NULL)
}

par(mfrow=c(2,2))
freq_histogram()
freq_histogram("overt connective")
freq_histogram("juxtaposition")
freq_histogram("elliptical")

#### Dens Histograms: ####
density_histogram <- function(type) {
  if(missing(type)) {
    input <- df$distance
    text <- paste("Density of all structures")
  } else {
    input <- df$distance[df$structure==type]
    text <- paste("Density of structure", type)
  }
  hist(input,
       freq = FALSE,
       col = "green",
       breaks = seq(0, length(input), by = 5),
       #xlim = c(0, length(input)/5),
       xlab = text,
       main=NULL)
  lines(density(input))
}

par(mfrow=c(2,2))
density_histogram()
density_histogram("overt connective")
density_histogram("juxtaposition")
density_histogram("elliptical")

#### Correlations: #### 
correlation <- function(type1, type2) {
  column1 <- df$distance[df$structure == type1]
  column2 <- df$distance[df$structure == type2]
  
  if(length(column1) > length(column2)) {
    column2 <- append(rep(NA, length(column1)-length(column2)), column2, after = 0)
    #cor(column1,column2,use = "na.or.complete", method = "spearman")
  }else{
    column1 <- append(rep(NA, length(column2)-length(column1)), column1, after = 0)
  }
  return(cor.test(column1, column2, alternative = "greater", method = "pearson"))
}

correlation("overt connective", "juxtaposition") #Negative correlation: a relationship between two variables in which one variable increases as the other decreases, and vice versa. But not statistically significant.
correlation("overt connective", "elliptical") #Not statistically significant.
correlation("juxtaposition", "elliptical") #Negative correlation: a relationship between two variables in which one variable increases as the other decreases, and vice versa. But not statistically significant.

#### T-tests: #### 

two_tailed_t_test <- function(type1, type2) {
  input <- df[df$structure%in% c(type1, type2),]
  return(t.test(distance~structure, data = input))
}

two_tailed_t_test("overt connective", "juxtaposition") #Not significantly different. But not statistically signifcant.
two_tailed_t_test("overt connective", "elliptical") #significantly different
two_tailed_t_test("juxtaposition", "elliptical")  #significantly different
a
#### Visualization: ####
ggplot(df, aes(x = distance, y = structure)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Distance and Structure")

#### Regression model ####
test <- transform(df, postFac=cut(distance,
        breaks=c(-Inf, median(distance), Inf),
        labels=c("lo", "hi")))

#test$postFac[test$structure=='overt connective']
#test$distance <- test$distance + 1 
cdplot(postFac ~ distance, data=test, subset=structure == "juxtaposition",
       main="Geschätzte Kategorien-Wkt. Juxtaposition")
cdplot(postFac ~ distance, data=test, subset=structure == "overt connective",
       main="Geschätzte Kategorien-Wkt. Overt connective ")
cdplot(postFac ~ distance, data=test, subset=structure == "elliptical",
       main="Geschätzte Kategorien-Wkt. Elliptical")

(glmFit <- glm(postFac ~ distance + structure, family=binomial(link="logit"),
               data=test))

exp(confint(glmFit))

#### Naive Bayes classification ####
#### https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/

#install.packages('naivebayes')
library(naivebayes)
#install.packages('dplyr')
library(dplyr)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('psych')
library(psych)

test2 <- df
head(test2)
test2 <- test2[1:2]

xtabs(~structure+distance, data = test2)
str(test2)

test2$structure <- as.factor(test2$structure)
test2$distance <- as.factor(test2$distance)

pairs.panels(test2)

test2 %>%
  ggplot(aes(x=distance, y=structure, fill = structure)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

set.seed(1234)
ind <- sample(2, nrow(test2), replace = T, prob = c(0.8, 0.2))
train <- test2[ind == 1,]
test <- test2[ind == 2,]

model <- naive_bayes(structure ~ ., data = train, laplace = 1, usekernel = T) 
plot(model) 

p <- predict(model, train, type = 'prob')
head(cbind(p, train))

p1 <- predict(model, train)
(tab1 <- table(p1, train$structure))
1 - sum(diag(tab1)) / sum(tab1)

p2 <- predict(model, test)
(tab2 <- table(p2, test$structure))
1 - sum(diag(tab2)) / sum(tab2)


