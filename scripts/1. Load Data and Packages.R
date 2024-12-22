## Load data
data <- read.csv("data/heart.csv")

# required libaraies
library(caret)        # train test split
library(ggplot2)      # histograms, boxplots, correlation heatmap
library(gridExtra)    # histograms
library(RColorBrewer) # correlation heatmap
library(MASS)         # stepwise regression
library(car)          # vif
library(pROC)         # ROC curve, confusion matrix

# train-test split
set.seed(123)
trainIndex <- createDataPartition(data$`output`, p = 0.8, list =
                                    FALSE)
train <- data[trainIndex, ]
# we use this as a proxy for real world data later
# therefore they are excluded from EDA to training processes
test <- data[-trainIndex, ] 