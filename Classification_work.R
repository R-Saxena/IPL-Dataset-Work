#For the classification Problem, I am going to use Decision Tree model and Random Forest Model.

#In this problem, I am trying to predict whether the team 1 will win or not if I have some data related to the match like whether team1 won the toss and choosed batting, etc.


#importing the dataset
Matches = read.csv("data/matches.csv")

#Viewing the dataset
View(Matches)

#<---------------------------preprocessing of the dataset---------------------------->
Matches = Matches[Matches$result == "normal",]
Matches = Matches[,c(3,5,6,7,8,11)]


Matches$team1_toss_win = ifelse(Matches$team1 == Matches$toss_winner, 1, 0)

Matches$team1_bat = ifelse((Matches$team1_toss_win == 1 & Matches$toss_decision == "bat"), 1, 0)

Matches$winner = factor(Matches$winner, levels = unique(Matches$winner))

Matches$team1_win = ifelse(Matches$team1 == Matches$winner, 1, 0)

teams_list = c("Pune Warriors",
              "Kolkata Knight Riders",
              "Rajasthan Royals",
              "Kochi Tuskers Kerala",
              "Gujarat Lions",
              "Chennai Super Kings",
              "Rising Pune Supergiants",
              "Delhi Daredevils",
              "Deccan Chargers",
              "Delhi Capitals",
              "Mumbai Indians",
              "Sunrisers Hyderabad",
              "Rising Pune Supergiant",
              "Royal Challengers Bangalore",
              "Kings XI Punjab")

teams_number = c(1:15)

Matches$team1 = factor(Matches$team1, 
                      levels = teams_list,
                      labels = teams_number)


Matches$team2 = factor(Matches$team2, 
                       levels = teams_list,
                       labels = teams_number)


Matches$city = as.numeric(factor(Matches$city, levels = unique(Matches$city)))


Preprocessed_Matches_data = Matches[,c(1,2,3,7,8,9)]

View(Preprocessed_Matches_data)

write.csv(Preprocessed_Matches_data, "Preprocessed Data/matches.csv")


#<----------------------------Spliting the dataset---------------------------->

library(caTools)
set.seed(123)
split = sample.split(Preprocessed_Matches_data$team1_win, SplitRatio = 0.75)
training_set = subset(Preprocessed_Matches_data, split == TRUE)
test_set = subset(Preprocessed_Matches_data, split == FALSE)


#<---------------------------------Modelling----------------------------------->

#<--------------------------------Decision Tree-------------------------------->

#Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = team1_win ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-6])

y_pred = y_pred > 0.5

truth = test_set[, 6]


cm = table(y_pred, truth)
print("Confusion Matrix")
cm
print('Accuracy')
print(cm[1]+cm[4] / (cm[2]+cm[3]))

#plotting the decision tree
plot(classifier)
text(classifier)



#<--------------------Random Forest Classification-------------------------->


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-6],
                          y = training_set$team1_win,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-6])

y_pred = y_pred > 0.5

truth = test_set[, 6]


cm = table(y_pred, truth)
print("Confusion Matrix")
cm
print('Accuracy')
print(cm[1]+cm[4] / (cm[2]+cm[3]))

#plotting the of error vs number of trees in random forest model training
plot(classifier)
