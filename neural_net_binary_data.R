# libraries
library(tidyverse)
library(neuralnet)

# reading data
data = read.csv(file.choose(), header = TRUE)
str(data)

# Normalizing the data
data$gre <- (data$gre - min(data$gre))/(max(data$gre) - min(data$gre))
data$gpa <- (data$gpa - min(data$gpa))/(max(data$gpa) - min(data$gpa))
data$rank <- (data$rank - min(data$rank))/(max(data$rank)-min(data$rank))

# partitioning data
set.seed(123)
ind = sample(2, nrow(data), replace = TRUE, prob = c(0.6, 0.4))
train = data[ind==1,]
test = data[ind==2,]

# Neural Networks
set.seed(1234)
neural = neuralnet(admit ~ .,
               data = train,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE)
plot(neural)

# Prediction & misclassification error
prediction1 = predict(neural, train)
head(prediction1)
pred1 = ifelse(prediction1>0.5, 1, 0)
tab1 = table(pred1, train$admit)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
prediction2 = predict(neural, test)
head(prediction2)
pred2 = ifelse(prediction2>0.5, 1, 0)
tab2 = table(pred2, test$admit)
tab2
1-sum(diag(tab2))/sum(tab2)
