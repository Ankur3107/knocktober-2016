setwd("/Users/Ankur/Desktop/AnalyticsVidhya/KnockTober/")

train=read.csv("Train.csv",header = T)
test=read.csv("Test.csv",header = T)
patient_prof=read.csv("Patient_Profile.csv",header = T)
first_hc=read.csv("First_Health_Camp_Attended.csv",header = T)
second_hc=read.csv("Second_Health_Camp_Attended.csv",header = T)
third_hc=read.csv("Third_Health_Camp_Attended.csv",header = T)

first_hc$X=NULL

avg1=first_hc$Health_Score/nrow(first_hc)
avg1=sum(first_hc$Health_Score)/nrow(first_hc)
avg2=sum(second_hc$Health.Score)/nrow(second_hc)
avg=avg1+avg2
avg=avg/2

result=rep(avg2,nrow(test))
solution_avg <- data.frame(Patient_ID = test$Patient_ID,
            Health_Camp_ID=test$Health_Camp_ID, Outcome = result)
write.csv(solution_avg, file = 'Solution_avg.csv', row.names = F)

library(dplyr)

trainf=read.csv("trainf.csv",stringsAsFactors = F)
testf=read.csv("testf.csv",stringsAsFactors = F)


trainf$Camp_Duration <-as.Date(as.character(trainf$Camp_End_Date), format="%Y-%m-%d")-
  as.Date(as.character(trainf$Camp_Start_Date), format="%Y-%m-%d")

#trainf$Camp_Duration <- trainf$Camp_End_Date - trainf$Camp_Start_Date
trainf$Camp_Duration <- as.numeric(as.character(trainf$Camp_Duration))

testf$Camp_Duration <- as.Date(as.character(testf$Camp_End_Date), format="%Y-%m-%d")-
  as.Date(as.character(testf$Camp_Start_Date), format="%Y-%m-%d")
test$Camp_Duration <- as.numeric(as.character(testf$Camp_Duration))



trainf$Camp_Start_Date <- NULL
trainf$Camp_End_Date <- NULL

testf$Camp_Start_Date <- NULL
testf$Camp_End_Date <- NULL

trainf$Patient_Response <-as.Date(as.character(trainf$Registration_Date), format="%Y-%m-%d")-
  as.Date(as.character(trainf$First_Interaction), format="%Y-%m-%d")
#trainf$Patient_Response <- trainf$Registration_Date - trainf$First_Interaction
trainf$Patient_Response <- as.numeric(as.character(trainf$Patient_Response))

testf$Patient_Response <- as.Date(as.character(testf$Registration_Date), format="%Y-%m-%d")-
  as.Date(as.character(testf$First_Interaction), format="%Y-%m-%d")
testf$Patient_Response <- as.numeric(as.character(testf$Patient_Response))


trainf$Registration_Date <- NULL
trainf$First_Interaction <- NULL

testf$Registration_Date <- NULL
testf$First_Interaction <- NULL

res=data.frame(Target=NULL)

isEmpty <- function(x) {
  return(length(x)==0)
}
for(i in 1:nrow(trainf)) {
  row <- trainf[1,]
  PID=row$Patient_ID
  HCID=row$Health_Camp_ID
  
  val=subset(first_hc$Health_Score,first_hc$Patient_ID==PID &
               first_hc$Health_Camp_ID==HCID)
  if(is.null(val) | isEmpty(val)){
    val=subset(second_hc$Health_Score,second_hc$Patient_ID==PID & 
                 second_hc$Health_Camp_ID==HCID)
  }
  if(is.null(val) | isEmpty(val)){
    val=subset(third_hc$Number_of_stall_visited,third_hc$Patient_ID==PID & 
                 third_hc$Health_Camp_ID==HCID)
  }
  if(is.null(val)){
    val=NA
  }
  
  res=rbind(res,val)
  # do stuff with row
  
}

library(mice)
testf$Target=NA
merge=rbind(trainf,testf)
set.seed(144)
simple=merge[c("Age","Income","Patient_Response")]
imputed=complete(mice(simple,m=1))

merge$Age=imputed$Age
merge$Income=imputed$Income
merge$Patient_Response=imputed$Patient_Response

trainm=merge[1:75278,]
testm=merge[75279:110527,]
trainm$Target=as.factor(trainm$Target)

Target<- as.list(res$X2L)
log_model <- glm(Target ~ ., data=trainm, family = binomial(link="logit"))

predict <- predict(log_model,newdata = testm,type = "response")

submission1 <- data.table(Patient_ID = testm$Patient_ID, Health_Camp_ID = testm$Health_Camp_ID, Outcome = predict)

write.csv(submission1,"1.csv",row.names = F)

target<-testm$Target
trainm$Target<-NULL
zeros <- rep(0, 35249)

#--------------------------------------------
# I will fit 50 models.
# The predictions are averaged out.
# So this is simply an ensemble of boosters.
#--------------------------------------------

control <- 50
library(xgboost)
for (i in 1:control){
  
  bst <- xgboost(data = trainm,
                 label = target,
                 eta = 0.1,
                 max_depth = 6,
                 subsample = 0.5,
                 colsample_bytree = 1,
                 nrounds = 400,
                 objective = "binary:logistic",
                 eval_metric = "merror",
                 maximize = FALSE)
  
  yhat <- predict(bst,test)
  zeros <- zeros + yhat
}

zeros <- zeros/control