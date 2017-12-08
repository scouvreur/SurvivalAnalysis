# R code to perform predictive modelling on the Titanic sample
# dataset from Kaggle

# Clear variable environment and command history
rm(list=ls())
cat("\014")

# Set working directory
setwd("~/Dropbox/Documents/Projects/Data_Science/Titanic_Survival/")

# Load in raw data
train <- read.csv("train.csv", header = TRUE, row.names = 1)
test <- read.csv("test.csv", header = TRUE, row.names = 1)

# Add a "Survived" variable to the test set to allow for the combination of datasets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine the two test datasets
data.combined <- rbind(train,test.survived)
# Inspect the structure of your dataset
str(data.combined)

# Convert 'Passenger class' and 'Survived' as
# factor variables (categorical data)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

# Have a look at gross survival rates, and look at the
# distribution acros the classes
table(data.combined$Survived)
table(data.combined$Pclass)

# Load up ggplot 2 library package for visualizations
library(ggplot2)

## Hypothesis 1 - Richer people (in 1st class) survived at a higher rate
# Convert 'Pclass' as factor variable
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Passenger Class") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examine the first few names in the training data set
head(as.character(train$Name))

# Find the unique names across both train & test data sets
length(unique(as.character(data.combined$Name)))

# Find the duplicates names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Take a look at the records in the combined dataset
data.combined[which(data.combined$Name %in% dup.names),]
# The names are identical but correspond to different people

# We notice that within the names there are titles
# such as 'Mr.', 'Mrs.' and 'Miss'
library(stringr)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrs <- data.combined[which(str_detect(data.combined$Name, "Mr.")),]
masters <- data.combined[which(str_detect(data.combined$Name, "Master.")),]
doctors <- data.combined[which(str_detect(data.combined$Name, " Dr. ")),]
all <- data.combined[which(str_detect(data.combined$Name, paste(c('Miss.', 'Mr.', 'Mrs.', 'Master.', ' Dr. '), collapse="|"))),]
head(misses)

## Hypothesis 2 - Name titles 'Miss' and 'Mrs.' correlate with age
head(mrses)

# Look at males to see if pattern continues
male <- data.combined[which(data.combined$Sex == "male"), ]
head(male)

# Expand the relationship between 'Survived' and 'Pclass' by adding the new
# 'Title' variable in the dataset and then explore a potential 3-D relationship

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep(" Dr. ", name)) > 0) {
    return("Dr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles)

# Plot the survival data per class and per name title
# for the training data set, first 891 lines
ggplot(data.combined[1:891,], aes(x=Title, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap("Pclass") +
  ggtitle("Passenger class") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
