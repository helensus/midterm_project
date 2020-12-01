# library(pacman)
# p_load(plyr,tidyverse,skimr,recipes,rsample,caret,doParallel)
tr <- read.csv('data/cs-training.csv')
te <- read_csv('data/cs-test.csv') 
skim_to_list(tr)  #present the data list and missing numbers
# NA appears in "MonthlyIncome" and "NumberofDependents"
library(tidyverse)
.....

library(ggplot2)
g1 <- ggplot(data=tr, aes(x=tr$SeriousDlqin2yrs))+geom_histogram()

#RevolvingUtilizationOfUnsecuredLines
g2 <- ggplot(data=tr, aes(x=tr$RevolvingUtilizationOfUnsecuredLines))+geom_histogram()

g3 <- ggplot(data=tr, aes(x=age))+geom_density()
#the people aging 0 are outliers, most people are between age 30-70
#remove the outliers


#Late Payment Columns

#DebtRatio