# library(pacman)
# p_load(plyr,tidyverse,skimr,recipes,rsample,caret,doParallel)
tr <- read.csv('data/cs-training.csv')
te <- read_csv('data/cs-test.csv') 
partition(skim(tr))  #present the data list and missing numbers
# NA appears in "MonthlyIncome" and "NumberofDependents"
# replace all NA in "MonthlyCIncome" with median, delete all NA in "NumberofDependents"
library(tidyverse)
tr<- tr %>% drop_na(NumberOfDependents)
tr<- tr %>% mutate(MonthlyIncome = replace_na(MonthlyIncome, median(MonthlyIncome, na.rm = TRUE)))
sum(is.na(tr))
.....

library(ggplot2)

#RevolvingUtilizationOfUnsecuredLines
g2 <- ggplot(data=tr, aes(x=tr$RevolvingUtilizationOfUnsecuredLines))+geom_histogram(col="green")+geom_density()
#the plot shows that there are points larger than 1. Since "RevolvingUtilizationOfUnsecuredLines" means the balance on credit cards devided by the credit limits, it could not be a large number.
#delete lines including x larger than 15
tr<- tr %>% filter(RevolvingUtilizationOfUnsecuredLines<=15) 

#age
g3 <- ggplot(data=tr, aes(x=age))+geom_histogram(col="blue")+geom_density()
#the people aging 0 are outliers, most people are between age 30-70
#remove the outliers having age smaller than 16
tr<- tr %>% filter(age>=16)

#NumberOfTime30.59DaysPastDueNotWorse 
#In logic, number of times borrowers 30 days past due in 2 years could not be more than 24 times.
#In logic, number of times borrowers 60 days past due in 2 years could not be more than 12 times.
##In logic, number of times borrowers 90 days past due in 2 years could not be more than 8 times.
tr<- tr %>% filter(NumberOfTime30.59DaysPastDueNotWorse < 24)
tr<- tr %>% filter( NumberOfTime60.89DaysPastDueNotWorse  < 12)
tr<- tr %>% filter( NumberOfTimes90DaysLate   < 8)

#DebtRatio
#75% of monthly debt payment divided by monthly income is under 0.869, but the largest number is 329664, which makes no sense in reality. 


#multilevel: group by dependents
tr %>% count(NumberOfDependents)

mod1<-glm(data=tr, SeriousDlqin2yrs~DebtRatio)
summary(mod1)
