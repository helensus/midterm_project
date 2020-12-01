# library(pacman)
# p_load(plyr,tidyverse,skimr,recipes,rsample,caret,doParallel)
tr <- read.csv('data/cs-training.csv')
te <- read_csv('data/cs-test.csv') 
skim_to_list(tr)  #present the data list and missing numbers
# NA appears in "MonthlyIncome" and "NumberofDependents"
# replace all NA in "MonthlyCIncome" with median, delete all all NA in "NumberofDependents"
library(tidyverse)
tr<- tr %>% drop_na(NumberOfDependents)
tr<- tr %>% mutate(MonthlyIncome = replace_na(MonthlyIncome, median(MonthlyIncome, na.rm = TRUE)))
sum(is.na(tr))
.....

library(ggplot2)
g1 <- ggplot(data=tr, aes(x=tr$SeriousDlqin2yrs))+geom_histogram()

#RevolvingUtilizationOfUnsecuredLines
g2 <- ggplot(data=tr, aes(x=tr$RevolvingUtilizationOfUnsecuredLines))+geom_histogram(col="green")+geom_density()

g3 <- ggplot(data=tr, aes(x=age))+geom_histogram(col="blue")+geom_density()
#the people aging 0 are outliers, most people are between age 30-70
#remove the outliers
tr<- tr %>% filter(age!=0)  

#Late Payment Columns

#DebtRatio


#multilevel: group by dependents
tr %>% count(NumberOfDependents)

