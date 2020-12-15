#For Regression I am going to use Linear Regression Model and Support vector regression(SVR)

#importing the dataset
matches = read.csv("data/matches.csv")
deliveries = read.csv("data/deliveries.csv")

#viewing the dataset
View(matches)
View(deliveries)


#<---------------------------------------preprocessing of the data-------------------------------------->

## Bowlers grouped by sets of data
# Data is grouped for bowlers to provide greater depth of information. Very important for the regression analysis.

library(dplyr)
library(prob)

bowlers = deliveries %>%
                        group_by(match_id, inning, bowling_team, bowler, over) %>%
                        summarise(total_runs = sum(total_runs), wide_runs = sum(wide_runs), bye_runs = sum(bye_runs), legbye_runs = sum(legbye_runs), noball_runs = sum(noball_runs))


bowlers$runs = bowlers$total_runs - bowlers$bye_runs + bowlers$legbye_runs
bowlers$extras = bowlers$wide_runs + bowlers$noball_runs

drop <- c("bye_runs","legbye_runs", "total_runs")
bowlers = bowlers[,!(names(bowlers) %in% drop)]
View(bowlers)

dismissal_kinds_for_bowler = c("bowled", "caught", "lbw", "stumped", "caught and bowled", "hit wicket")

dismissals = deliveries[deliveries$dismissal_kind %in% dismissal_kinds_for_bowler,c("match_id", "inning", "bowling_team", "bowler", "over", "dismissal_kind")]
row.names(dismissals) <- NULL

dismissals = dismissals %>%
                          group_by(match_id, inning, bowling_team, bowler, over) %>%
                          summarise(dismissal_kind = length(dismissal_kind))

dismissals = dismissals %>%
  rename(
    wickets = dismissal_kind
  )


View(dismissals)

bowlers = merge(x = bowlers, y = dismissals, by = c("match_id", "inning", "bowling_team", "bowler", "over"), all.x = TRUE)

bowlers[is.na(bowlers$wickets), 'wickets'] <- 0

View(bowlers)

bowlers_over = bowlers %>% 
                        group_by(match_id, inning, bowling_team, bowler) %>%
                        summarise(
                          over = length(over)
                        )

bowlers = bowlers %>%
                    group_by(match_id, inning, bowling_team, bowler)  %>%
                    summarise(
                      wide_runs = sum(wide_runs),
                      noball_runs = sum(noball_runs),
                      runs = sum(runs),
                      extras = sum(extras),
                      wickets = sum(wickets)
                    )
                  

bowlers = merge(x = bowlers_over, y = bowlers,by =c("match_id", "inning", "bowling_team", "bowler") , all.x = TRUE)

bowlers$Econ = round(bowlers$runs / bowlers$over, 2)


View(bowlers)

bowlers = merge(x = matches[,c('id','Season')], y = bowlers, by.x = ("id"), by.y = ('match_id'), all.x = TRUE)

bowlers$id <- NULL

write.csv("Preprocessed Data/bowlers.csv")

#<------------------------------Modelling-------------------------------------->

dataset = bowlers[, c("over", "wide_runs", "noball_runs", "runs", "extras", "wickets", "Econ")]

library(caTools)
set.seed(123)
split = sample.split(dataset$Econ, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#<-----------------------Multi Linear Regression------------------------------->

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Econ ~ .,
               data = training_set)
 

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

y_pred

#RMSE

library(Metrics) 

print("RMSE of Linear Regression")
print(rmse(test_set[,7], y_pred))


#<------------------------------------- SVR------------------------------


library(e1071)
regressor = svm(formula = Econ ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting a new result
y_pred = predict(regressor, test_set[-7])

y_pred


print("RMSE of SVR")
print(rmse(test_set[,7], y_pred))

test_set[,7]

