#################################### INJURY TYPE PREDICTION #############################################
library(openxlsx)
library(sqldf)
library(lubridate)
library(dygraphs)
library(dplyr)
library(forecast)
library(ggplot2)

#SET DIRECTORY

setwd("/Users/shailendra/Desktop/updated_MRP/code/predictive_analytics/")
list.files()


prediction_data<-read.csv("KSI.csv")
names(prediction_data)
prediction_data1<-read.xlsx("Modified_KSI.xlsx",sheet=1)
prediction_data1<-data.frame(ACCNUM=prediction_data$ACCNUM,prediction_data1)

Injury_type_pred_data<-subset(prediction_data1,select=c(ACCNUM,INJURY,ROAD_CLASS,LOCCOORD,ACCLOC,TRAFFCTL,VISIBILITY,LIGHT,RDSFCOND,IMPACTYPE,INVTYPE,INVAGE,INITDIR,VEHTYPE,MANOEUVER,DRIVACT,DRIVCOND,SPEEDING,AG_DRIV,REDLIGHT,ALCOHOL,DISABILITY))
Injury_type_pred_data$INJURY<-as.character(Injury_type_pred_data$INJURY)
#Injury_type_pred_data$ACCLASS<-ifelse(Injury_type_pred_data$ACCLASS=="Property Damage Only","Non-Fatal Injury",Injury_type_pred_data$ACCLASS)
table(Injury_type_pred_data$INJURY)
table(Injury_type_pred_data$INJURY)/nrow(Injury_type_pred_data)

prop.table(table(Injury_type_pred_data$INJURY))

Injury_type_pred_data[is.na(Injury_type_pred_data)]<-"Missing"

#Injury_type_pred_data$IMPACTYPE

library(randomForest)
# Classification Tree with rpart
library(rpart)
library(rpart.utils)
# grow tree 
#fit <- rpart(INJURY ~ .,
#             method="class", data=Injury_type_pred_data)
#library(rattle)

#s<-summary(fit)
#data.frame(s$variable.importance)

#asRules(fit)
#install.packages("rattle", repos="http://rattle.togaware.com", type="source")


#rattle()


cc<-subset(Injury_type_pred_data,select=c(INJURY , IMPACTYPE,INVTYPE,INVAGE,VEHTYPE,DRIVACT))

cc<-data.frame(sapply(cc,"as.factor"))

#library(randomForest)
#model = randomForest(INJURY ~ IMPACTYPE+INVTYPE+INVAGE+VEHTYPE+DRIVACT, data=cc, ntree=100)
#layout(matrix(c(1,2),nrow=1),
#       width=c(4,1)) 
#par(mar=c(5,4,4,0)) #No margin on the right side
#plot(model, log="y")
#par(mar=c(5,0,4,2)) #No margin on the left side
#plot(c(0,1),type="n", axes=F, xlab="", ylab="")
#legend("top", colnames(model$err.rate),col=1:6,cex=0.8,fill=1:4)


#write.csv(model$err.rate,"OOB.csv",row.names=F)


##################################### Balanced Sampling ###################################
library(caret)
set.seed(42)
index <- createDataPartition(cc$INJURY, p = 0.7, list = FALSE)
train_data <- cc[index, ]
test_data  <- cc[-index, ]

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 2, 
                     verboseIter = FALSE,
                     sampling = "up")

#train_data<-subset(train_data,select=-c(ACCNUM,LOCCOORD))
#test_data<-subset(test_data,select=-c(ACCNUM,LOCCOORD))

set.seed(42)
model_rf_over <- caret::train(INJURY ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)


final_over <- data.frame(actual = test_data$INJURY,
                         predict(model_rf_over, newdata = test_data, type = "raw"))


fitted(model_rf_over)
print(final_over)
names(final_over)[2]="predict"

#Confussion Matrix
f <- confusionMatrix(fitted(model_rf_over), train_data$INJURY)
f
f<-table(fitted(model_rf_over),train_data$INJURY)
f

cm_over <- confusionMatrix(final_over$predict, test_data$INJURY)
cm_over
cm_over <- table(final_over$predict, test_data$INJURY)
cm_over

#test
cm_over <- round((table(final_over$predict, test_data$INJURY)/length(final_over$predict))*100,3)
cm_over
#train
f<-round((table(fitted(model_rf_over), train_data$INJURY)/length(fitted(model_rf_over)))*100,3)
f

######## Tree
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, 
                       verboseIter = FALSE, sampling = "up")
set.seed(42)
dtree_fit_over <- train(INJURY ~., data = train_data, method = "rpart", 
                        parms = list(split = "information"),
                        preProcess = c("scale", "center"),
                        trControl=trctrl,
                        tuneLength = 10)

dtree_fit_over

final_tr_over <- data.frame(actual = test_data$INJURY,
                            predict(dtree_fit, newdata = test_data, type = "raw"))

names(final_tr_over)[2]="predict"

f <- confusionMatrix(fitted(dtree_fit_over), train_data$INJURY)
f
cm_over <- confusionMatrix(final_tr_over$predict, test_data$INJURY)
cm_over

##############