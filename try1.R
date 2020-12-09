# library(pacman)
# p_load(plyr,tidyverse,skimr,recipes,rsample,caret,doParallel)
library(rstanarm)
library(lme4)
library(skimr)
library(pROC)
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


# #multilevel: group by dependents
tr<- tr %>% filter( NumberOfDependents   < 5)

# mod1<-glm(data=tr, SeriousDlqin2yrs~DebtRatio)
# summary(mod1)

#logistic
log1<-glm(SeriousDlqin2yrs  ~ age + RevolvingUtilizationOfUnsecuredLines + 
            DebtRatio + MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberRealEstateLoansOrLines + NumberOfDependents + 
            NumberOfTime30.59DaysPastDueNotWorse + NumberOfTime60.89DaysPastDueNotWorse + NumberOfTimes90DaysLate, 
          data = tr, family = "binomial")
summary(log1)


# log2<-stan_glm(SeriousDlqin2yrs  ~ age + RevolvingUtilizationOfUnsecuredLines + 
#             DebtRatio + MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberRealEstateLoansOrLines + NumberOfDependents + 
#             NumberOfTime30.59DaysPastDueNotWorse + NumberOfTime60.89DaysPastDueNotWorse + NumberOfTimes90DaysLate, 
#           data = tr, family = binomial(link = "logit"),refresh=0)
log2<- readRDS("stanlog.rds")

log3 <- glmer(SeriousDlqin2yrs  ~ age+(1|NumberOfDependents),data=tr,family="binomial")
summary(log3)
tt <- getME(log3_sc,"theta")
ll <- getME(log3_sc,"lower")
min(tt[ll==0])
#predictions <- predict(log1, newdata = te) #required to change the names

#AUC
roc.test(log1, log2, method="bootstrap")

g3 <- ggroc(list(s100b=log1, wfns=log2, ndka= log3))
g3
------------------------
# 分组 agecat 
tr <- mutate(tr, agecat = age)
tr <- within(tr,{
  agecat[age>=75]<-"elder"
  agecat[age>=45 & age <=75]<-"middle"
  agecat[age<45]<-"young"}
)

#agecat=random effect
tr %>% group_by(agecat) %>% summarize(mean=mean(SeriousDlqin2yrs) )%>% ungroup() %>% 
  ggplot() + geom_point(aes(x=agecat,y=mean,color=agecat),size=4) +
  theme_bw() + xlab("Age Class") + ylab("mean of prob of Delinquent") + ggtitle("Predation per age group") + 
  theme(legend.position="none")

# From the plot, it seems that the the younger age group has higher prob of Delinquent, so we take age as a fixed effect.
---------------
# 分组 MonthlyIncome
tr <- mutate(tr, incomecat = MonthlyIncome)
tr <- within(tr,{
  incomecat[MonthlyIncome>=7500]<-"rich"
  incomecat[MonthlyIncome>=5400 & MonthlyIncome <=7500]<-"middle rich"
  incomecat[MonthlyIncome>=3812 & MonthlyIncome <=5400]<-"middle"
  incomecat[MonthlyIncome<3812]<-"poor"
  }
)

tr %>% group_by(incomecat) %>% summarize(mean=mean(SeriousDlqin2yrs) )%>% ungroup() %>% 
  ggplot() + geom_point(aes(x=incomecat,y=mean,color=incomecat),size=4) +
  theme_bw() + xlab("Income Class") + ylab("mean of prob of Delinquent") + ggtitle("Predation per income group") + 
  theme(legend.position="none")
# From the plot, it seems that the the richer age group has lower prob of Delinquent, so we take income as a fixed effect.

--------
glmer1 <- stan_glmer(data=tr,SeriousDlqin2yrs~age+(1|incomecat),family=binomial(link="logit"), refresh=0)
--------
  # tr %>% select ( -c("NumberOfTime60.89DaysPastDueNotWorse", -"NumberOfTimes90DaysLate") )%>%
  # ggpairs(upper = list(continuous = wrap("cor")), lower = list(continuous = wrap("smooth", alpha = 0.3)), diag = list(continuous = wrap("barDiag", binwidth = 10)))

