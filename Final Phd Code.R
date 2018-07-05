################################Title -  PHD B37 Praful ############################################

#######Importing all required libraries##########
library(sqldf)
library(caret)
library(nnet)
library(DMwR)
library(mice)
library(C50)
library(randomForest)
############################                                      ############################  
############################ DATA COLLECTION FROM DIFFERENT FILES ############################

rm(list = ls(all = T))     #Clearing enviroment

#######Reading basic patient detail set############
train_pat_dtl <- read.csv("Train.csv",header = T)
test_pat_dtl <- read.csv("Test.csv",header = T)


############Checking for multiple records for a patient#########
sqldf("SELECT patientID, count(1) from train_pat_dtl group by patientID having count(1) >1")
sqldf("SELECT patientID, count(1) from test_pat_dtl group by patientID having count(1) >1")

##########Reading diagnosis dataset ###############
train_diag_dtl <- read.csv("Train_Diagnosis_TreatmentData.csv",header = T)
test_diag_dtl <- read.csv("Test_Diagnosis_TreatmentData.csv",header = T)

############Checking for multiple records for a patient########
sqldf("SELECT patientID, count(1) from train_diag_dtl group by patientID having count(1) >1")
sqldf("SELECT patientID, count(1) from test_diag_dtl group by patientID having count(1) >1")

##########Reading hospitalization dataset ###############
train_hos_dtl <- read.csv("Train_HospitalizationData.csv",header = T)
test_hos_dtl <- read.csv("Test_HospitalizationData.csv",header = T)

############Checking for multiple records for a patient########
sqldf("SELECT patientID, count(1) from train_hos_dtl group by patientID having count(1) >1")
sqldf("SELECT patientID, count(1) from test_hos_dtl group by patientID having count(1) >1")

############Reading diagnosis category mapping and joining it with dataset################
diag_pat <- read.csv('ICD_Mapping_pat.csv')
train_diag_dtl <- sqldf("SELECT A.* , B.* from train_diag_dtl A INNER JOIN diag_pat B ON A.patientID = B.patientID")
train_diag_dtl <- train_diag_dtl[,-c(6,7,8,36)]     #Removing reduntent columns

test_diag_dtl <- sqldf("SELECT A.* , B.* from test_diag_dtl A INNER JOIN diag_pat B ON A.patientID = B.patientID")
test_diag_dtl <- test_diag_dtl[,-c(6,7,8,36)]  #Removing reduntent columns


###################Joining different sets on patientID ############
train_pat_diag <- sqldf("SELECT * from train_pat_dtl A inner join train_diag_dtl B ON A.patientID = B.patientID")
test_pat_diag <- sqldf("SELECT * from test_pat_dtl A inner join test_diag_dtl B ON A.patientID = B.patientID")

###########No of records: train_pat_dtl - 34650 + train_diag_dtl - 34650 = train_pat_diag - 34650#######

######Removing un-necessory dataframes###########
rm(train_diag_dtl,train_pat_dtl)
rm(test_diag_dtl,test_pat_dtl)

#####Removing duplicate key column patientID#############
train_pat_diag <- train_pat_diag[,-7]
test_pat_diag <- test_pat_diag[,-6]

########Joining train_pat_diag - 34650 rows with train_hos_dtl - 34650 rows#####
final_train_set <- sqldf("SELECT * from train_pat_diag A inner join train_hos_dtl B ON A.patientID = B.patientID")
final_test_set <- sqldf("SELECT * from test_pat_diag A inner join test_hos_dtl B ON A.patientID = B.patientID")

######Removing un-necessary dataframes###########
rm(train_hos_dtl,test_hos_dtl,train_pat_diag, test_pat_diag,diag_pat)

#####Removing columns patientID and admission_id as these are not useful for our analysis and model########
final_train_set <- subset(final_train_set,select = -c(patientID,AdmissionID))
final_train_set <- subset(final_train_set,select = -c(37))
final_test_set <- subset(final_test_set,select = -c(AdmissionID))
final_test_set <- subset(final_test_set,select = -c(37))




############################                     ###########################

#                          PRE-PROCESSING OF DATA                          #

#########################                        ###########################


apply(final_train_set,2,function(x) sum(is.null(x)))        #Checking for nulls in each column -> NO NULLs
apply(final_train_set,2,function(x) sum(is.na(x)))          #Checking for NAs in each column   -> NO NAs
apply(final_train_set,2,function(x) sum(is.nan(x)))         #Checking for Nan in each column   -> NO NaN
apply(final_train_set,2,function(x) sum(x == '?'))          #Checking for ? in each column as we have seen many ? ->  

###payer_code, medical_specialty and weight have about more than 50 % missing values

apply(final_test_set,2,function(x) sum(is.null(x)))
apply(final_test_set,2,function(x) sum(is.na(x)))
apply(final_test_set,2,function(x) sum(is.nan(x)))
apply(final_test_set,2,function(x) sum(x == '?'))

########Removing column with more than 50% missing values################

final_train_set <- subset(final_train_set,select = -c(medical_specialty ,payer_code ,weight))

final_test_set <- subset(final_test_set,select = -c(medical_specialty ,payer_code ,weight))

str(final_train_set)
str(final_test_set)

#######Changing data-types,levels based on attribute information provided#############

final_train_set$admission_source_id <- factor(final_train_set$admission_source_id,levels=c(1:26))
final_train_set$discharge_disposition_id <- factor(final_train_set$discharge_disposition_id,levels = c(1:29))
final_train_set$admission_type_id <- factor(final_train_set$admission_type_id,levels = c(1:8))
final_test_set$admission_source_id <- factor(final_test_set$admission_source_id,levels=c(1:26))
final_test_set$discharge_disposition_id <- factor(final_test_set$discharge_disposition_id,levels=c(1:29))
final_test_set$admission_type_id <- factor(final_test_set$admission_type_id,levels=c(1:8))




final_train_set$diagnosis_1 <- factor(final_train_set$diagnosis_1,levels = c('D1',	'D2',	'D3',	'D4',	'D5',	'D6',	'D7',	'D8',
                                                                             'D9',	'D10',	'D11',	'D12',	'D13',	
                                                                             'D14',	'D15',	'D16',	'D17',	'D18',	'D19'))
final_train_set$diagnosis_2 <- factor(final_train_set$diagnosis_2,levels = c('D1',	'D2',	'D3',	'D4',	'D5',	'D6',	'D7',	'D8',
                                                                             'D9',	'D10',	'D11',	'D12',	'D13',	
                                                                             'D14',	'D15',	'D16',	'D17',	'D18',	'D19'))
final_train_set$diagnosis_3 <- factor(final_train_set$diagnosis_3,levels = c('D1',	'D2',	'D3',	'D4',	'D5',	'D6',	'D7',	'D8',
                                                                             'D9',	'D10',	'D11',	'D12',	'D13',	
                                                                             'D14',	'D15',	'D16',	'D17',	'D18',	'D19'))
final_test_set$diagnosis_1 <- factor(final_test_set$diagnosis_1,levels = c('D1',	'D2',	'D3',	'D4',	'D5',	'D6',	'D7',	'D8',
                                                                             'D9',	'D10',	'D11',	'D12',	'D13',	
                                                                             'D14',	'D15',	'D16',	'D17',	'D18',	'D19'))
final_test_set$diagnosis_2 <- factor(final_test_set$diagnosis_2,levels = c('D1',	'D2',	'D3',	'D4',	'D5',	'D6',	'D7',	'D8',
                                                                           'D9',	'D10',	'D11',	'D12',	'D13',	
                                                                           'D14',	'D15',	'D16',	'D17',	'D18',	'D19'))
final_test_set$diagnosis_3 <- factor(final_test_set$diagnosis_3,levels = c('D1',	'D2',	'D3',	'D4',	'D5',	'D6',	'D7',	'D8',
                                                                           'D9',	'D10',	'D11',	'D12',	'D13',	
                                                                           'D14',	'D15',	'D16',	'D17',	'D18',	'D19'))




##########Converting discharge_disposition id mapped to EXPIRED#####

#final_train_set$discharge_disposition_id <- factor(ifelse((final_train_set$discharge_disposition_id != '19' & 
#final_train_set$discharge_disposition_id != '20' &
#final_train_set$discharge_disposition_id != '21'),final_train_set$discharge_disposition_id,'Expired') )

#final_test_set$discharge_disposition_id <- factor(ifelse((final_test_set$discharge_disposition_id != '19' & 
#                      final_test_set$discharge_disposition_id != '20' &
#                     final_test_set$discharge_disposition_id != '21'),final_test_set$discharge_disposition_id,'Expired') )


####Converting date columns into date format########
final_train_set$Admission_date <- as.Date(final_train_set$Admission_date)
sum(is.null(final_train_set$Admission_date))   #Checking if nulls are generated
final_train_set$Discharge_date <- as.Date(final_train_set$Discharge_date)
sum(is.null(final_train_set$Discharge_date))
str(final_train_set)

final_test_set$Admission_date <- as.Date(final_test_set$Admission_date)
sum(is.null(final_train_set$Admission_date))
final_test_set$Discharge_date <- as.Date(final_test_set$Discharge_date)
sum(is.null(final_test_set$Discharge_date))
str(final_test_set)

#######Creating NoDaysAdmit = Discharge_date - Admission_date and Month of year#######
final_train_set$NoDaysAdmit <- difftime(final_train_set$Discharge_date,final_train_set$Admission_date,units = "days")
final_train_set$NoDaysAdmit <- as.numeric(final_train_set$NoDaysAdmit)


final_test_set$NoDaysAdmit <- difftime(final_test_set$Discharge_date,final_test_set$Admission_date,units = "days")
final_test_set$NoDaysAdmit <- as.numeric(final_test_set$NoDaysAdmit)
########################Creating month of admission from admission date###########

final_train_set$month <- strftime(final_train_set$Admission_date,'%m')
final_test_set$month <- strftime(final_test_set$Admission_date,'%m')

#################Removing discahrge date and admission date##########
final_train_set <- subset(final_train_set,select = -c(Admission_date,Discharge_date))
final_test_set <-  subset(final_test_set,select = -c(Admission_date,Discharge_date)) 

final_train_set$month <- as.factor(final_train_set$month)  #Converting month into a factor variable
final_test_set$month <- as.factor(final_test_set$month)


##################Checking distribution of data#############################
summary(final_train_set)

ggplot()   + geom_bar(aes(y = ..count..,x = readmitted,fill = race),data=final_train_set,position = position_dodge())

ggplot()   + geom_bar(aes(y = ..count..,x = readmitted,fill = gender),data=final_train_set,position = position_dodge())

ggplot()  + geom_bar(aes(y = ..count..,x = readmitted,fill = age),data=final_train_set,position = position_dodge())

ggplot()   + geom_bar(aes(y = ..count..,x = change,fill = readmitted),data=final_train_set,position = position_dodge())

ggplot()   + geom_bar(aes(y = ..count..,x = readmitted,fill = month),data=final_train_set,position = position_dodge())

ggplot()   + geom_bar(aes(y = ..count..,x = readmitted,fill = diabetesMed),data=final_train_set,position = position_dodge())

ggplot()   + geom_bar(aes(y = ..count..,x = readmitted,fill = admission_type_id),data=final_train_set,position = position_dodge())

ggplot()   + geom_bar(aes(y = ..count..,x = readmitted,fill = metformin.rosiglitazone),data=final_train_set,position = position_dodge())
barplot(table(final_train_set$readmitted,final_train_set$metformin.rosiglitazone),,col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$metformin.pioglitazone),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$glipizide.metformin),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$miglitol),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$tolazamide),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$tolbutamide),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$acetohexamide),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$troglitazone),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$glyburide.metformin),col = c('dark blue','red'),legend.text = T)

barplot(table(final_train_set$readmitted,final_train_set$glyburide.metformin),col = c('dark blue','red'),legend.text = T)

#############Removing highly skewed or columns with no prediction power#######################
final_train_set <- subset(final_train_set,select = -c(glipizide.metformin,metformin.rosiglitazone,metformin.pioglitazone,miglitol,troglitazone,tolazamide,tolbutamide,acetohexamide  ))
final_test_set <- subset(final_test_set,select = -c(glipizide.metformin,metformin.rosiglitazone,metformin.pioglitazone,miglitol,troglitazone,tolazamide,tolbutamide,acetohexamide  ))

#scale <- preProcess(final_train_set,method = "scale")
#final_train_set <- predict(scale,final_train_set)

################# Imputation of missing values in race column using logistic########
set.seed(124)
#impute_train <- subset(final_train_set,race != '?')
#impute_test <- subset(final_train_set,race == '?')
#str(impute_train)
#impute_train$race <- factor(impute_train$race)

#impute_race <- multinom(race ~ .,data = impute_train[,-c(4)])

#######CHecking how well model imputing the values#########
#confusionMatrix(factor(final_test_set[final_test_set$race != '?',2]),predict(impute_race,final_test_set[final_test_set$race != '?',-c(1,2)]))

#race <- predict(impute_race,impute_test[,-c(1,4)])
#impute_test$race <- race

#table(predict(impute_race,final_test_set[final_test_set$race != '?',-c(1,2)]))
#table(final_test_set[final_test_set$race != '?',c(2)])
#barplot(rbind(table(predict(impute_race,final_test_set[final_test_set$race != '?',-c(1,2)]))
#      ,table(final_test_set[final_test_set$race != '?',c(2)])),legend.text = T,beside = T,col = c('Dark blue','red'))
#test_data_impute <- final_test_set[final_test_set$race == '?',]
#race <- predict(impute_race,test_data_impute[,-c(1,2)])
#test_data_impute$race <- race

#final_test_set <- rbind(final_test_set[final_test_set$race != '?',],test_data_impute)

#########Imputation with MICE################

final_train_set$race <- factor(ifelse(final_train_set$race == '?',NA,as.character(final_train_set$race))) 
final_test_set$race <- factor(ifelse(final_test_set$race == '?',NA,as.character(final_test_set$race)))
mice <- mice(final_train_set,m=1,method = 'cart',maxit = 2)
final_train_set <- complete(mice,1)
mice <- mice(final_test_set,m=1,method = 'cart',maxit = 2)
final_test_set <- complete(mice,1)

table(final_test_set$race)
#final_train_set <- rbind(impute_train,impute_test) 

#save.image(file = "train.RData")
#########################Creating new variable no of change in medication################
MedCount <- function(x)
{ 
  return(sum(x == 'Up') + sum(x == 'Down'))
  
  }
final_train_set$NumOfMedChg <- apply(final_train_set[,c(11:22)],1,FUN = MedCount )
table(final_train_set$NumOfMedChg,final_train_set$readmitted)

final_test_set$NumOfMedChg <- apply(final_test_set[,c(11:22)],1,FUN = MedCount )
##################################################################
train <- final_train_set 
test <- final_test_set

train$race <- factor(train$race)   #Releveling as ? is not present now
test$race <- factor(test$race)

table(train$readmitted)

#Randomly shuffle the data
train <- train[sample(nrow(train)),]
#Creating 5 equally size folds
folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)

###########################################RANDOM FOREST################################################

#Performing 5 fold cross validation with classwt and down-sampling
for(i in 1:5){
  #Segementing into train and val 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  
  trainData <- downSample(trainData[,-4], trainData$readmitted, list = FALSE, yname = "readmitted")
  RF <- randomForest(trainData$readmitted ~ .,ntree = 50,mtry = 17,data = trainData,classwt = c(2,1))
  print(recall(predict(RF,trainData[,-33]),trainData[,33],relevant = "Within30days"))
  print(recall(predict(RF,testData[,-4]),testData[,4],relevant = "Within30days"))
}

test$readmitted <- 'NO'
train$patientID <- 1

data <- rbind(train,test)

y <- data[data$patientID == 1,4]  #saving target column for training set

#Down sampling of whole train data
trainData <- downSample(data[data$patientID == '1',-c(4)], data[data$patientID == '1',c(4)], list = FALSE, yname = "readmitted")

RF <- randomForest(trainData$readmitted ~ .,ntree = 50,mtry = 17,data = trainData[,-33],classwt = c(2,1))

#save(RF,file = 'Best.rda')
varImpPlot(RF)

readmitted <- predict(RF,data[data$patientID != '1',-c(4,34)])

patientID <- data[data$patientID != '1',34]

subm <- data.frame(patientID,readmitted)

write.csv(subm,file = "pred.csv",row.names = F)

#tuneRF(trainData[,-4], trainData$readmitted ,mtryStart = 7,ntree = 60,trace = T,doBest = T)
########################Training C5 model #######################
train <- final_train_set 
test <- final_test_set

train$race <- factor(train$race)   #Releveling as ? is not present now
test$race <- factor(test$race)

cost_mat <- matrix(c(0, 0.2, 1.2, 0), nrow = 2)   #Cost matrix for C5 
rownames(cost_mat) <- colnames(cost_mat) <- c("NO", "Within30days")

for(i in 1:5){
  #Segementing into train and val
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  
  trainData <- downSample(trainData[,-4], trainData$readmitted, list = FALSE, yname = "readmitted")
  c5_1 <- C5.0(trainData$readmitted ~ .,data = trainData,costs = cost_mat,trial = 3)
  print(recall(predict(c5_1,trainData[,-33]),trainData$readmitted,relevant = "Within30days"))
  print(confusionMatrix(predict(c5_1,trainData[,-33]),trainData$readmitted,positive = "Within30days"))
  print(recall(predict(c5_1,testData[,-4]),testData[,4],relevant = "Within30days"))
  print(confusionMatrix(predict(c5_1,testData[,-4]),testData[,4],positive = "Within30days"))
}

trainData <- downSample(train[,-4], train$readmitted, list = FALSE, yname = "readmitted")

#Training C5 on whole train data
c5_1 <- C5.0(trainData$readmitted ~ .,data = trainData,costs = cost_mat,trial = 3)

summary(c5_1)
#plot(c5_1)
readmitted <- predict(c5,test[,-1])

patientID <- test$patientID
subm <- data.frame(patientID,readmitted)

write.csv(subm,file = "pred.csv",row.names = F)



#####################################SVM ##########################
library(dummies)
library(e1071)

train_num <- train[,c(4,5,6,7,8,28)]
train_cat <- train[,-c(4,5,6,7,8,28)]
test_cat <- test[,-c(1,5,6,7,8,28)]
train_cat$f <- 'train'
test_cat$f <- 'test'
dummy <- rbind(train_cat,test_cat)
dummy <- dummy.data.frame(dummy)

train_cat <- dummy[dummy$ftrain == 1,-c(128,129)]
train_svm <- cbind(train_num,train_cat)

test_cat <- dummy[dummy$ftest == 1,-c(128,129)]
test_num <- test[,c(5,6,7,8,28)]

test_svm <- cbind(test_num,test_cat)

train_svm<-train_svm[sample(nrow(train_svm)),]

folds <- cut(seq(1,nrow(train_svm)),breaks=5,labels=FALSE)


#cost_mat <- matrix(c(0, 0.2, 1.2, 0), nrow = 2)
#rownames(cost_mat) <- colnames(cost_mat) <- c("NO", "Within30days")
#cost_mat
wts <- data.frame(c(0.1),c(0.2))
names(wts) <- c('NO',"Within30days")
wts
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train_svm[testIndexes, ]
  trainData <- train_svm[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  trainData <- downSample(trainData, trainData$readmitted, list = FALSE, yname = "readmitted")
  svm <- svm(trainData$readmitted~.,trainData,type = "C",kernel = 'polynomial',degree = 3)
  print(recall(predict(svm,trainData[,-1]),trainData[,1],relevant = "Within30days"))
  print(confusionMatrix(predict(svm,trainData[,-1]),trainData[,1],positive = "Within30days"))
  print(recall(predict(svm,testData[,-1]),testData[,1],relevant = "Within30days"))
  print(confusionMatrix(predict(svm,testData[,-1]),testData[,1],positive = "Within30days"))
}

trainData <- downSample(train_svm, train_svm$readmitted, list = FALSE, yname = "readmitted")
svm <- svm(trainData$readmitted~.,trainData,type = "C",kernel = 'polynomial',degree = 3)
print(recall(predict(svm,trainData[,-1]),trainData[,1],relevant = "Within30days"))
print(confusionMatrix(predict(svm,trainData[,-1]),trainData[,1],positive = "Within30days"))


########################Recursive Feature Elimination############################
index <- createDataPartition(final_train_set$readmitted,list = F,p = 0.7)
train_rfe <- final_train_set[index,]
test_rfe <- final_train_set[-index,]
for(i in seq(1,ncol(train_rfe)))
{
  rF_rfe <- randomForest(train_rfe$readmitted ~ ., train_rfe[,-i],ntree = 50,mtry = 17)
  print(recall(predict(rF_rfe,train_rfe[,-c(4,i)]),train_rfe[,4],relevant = "Within30days"))
  print(recall(predict(rF_rfe,test_rfe[,-c(4,i)]),test_rfe[,4],relevant = "Within30days"))
}

im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
par(mfrow=c(1,1))
plotFilterValues(im_feat,n.show = 20)

############################################################################################################
#############################################################################################################
#DAY - 1 & DAY - 2##DAY - 1 & DAY - 2##DAY - 1 & DAY - 2##DAY - 1 & DAY - 2##DAY - 1 & DAY - 2##DAY - 1 & DAY - 2#
### PHD B37 Praful ###
############################################################################################################


############################                                      ############################  
############################ DATA COLLECTION FROM DIFFERENT FILES ############################
rm(list = ls(all = T))
####Reading basic detail set############
train_pat_dtl <- read.csv("Train.csv",header = T)
test_pat_dtl <- read.csv("Test.csv",header = T)

library(sqldf)

############Checking for multiple records for a patient#########
sqldf("SELECT patientID, count(1) from train_pat_dtl group by patientID having count(1) >1")
sqldf("SELECT patientID, count(1) from test_pat_dtl group by patientID having count(1) >1")

##########Reading diagnosis dataset ###############
train_diag_dtl <- read.csv("Train_Diagnosis_TreatmentData.csv",header = T)
test_diag_dtl <- read.csv("Test_Diagnosis_TreatmentData.csv",header = T)

############Checking for multiple records for a patient########
sqldf("SELECT patientID, count(1) from train_diag_dtl group by patientID having count(1) >1")
sqldf("SELECT patientID, count(1) from test_diag_dtl group by patientID having count(1) >1")

##########Reading hospitalization dataset ###############
train_hos_dtl <- read.csv("Train_HospitalizationData.csv",header = T)
test_hos_dtl <- read.csv("Test_HospitalizationData.csv",header = T)

############Checking for multiple records for a patient########
sqldf("SELECT patientID, count(1) from train_hos_dtl group by patientID having count(1) >1")
sqldf("SELECT patientID, count(1) from test_hos_dtl group by patientID having count(1) >1")

###################Joining different sets on patientID ########
train_pat_diag <- sqldf("SELECT * from train_pat_dtl A inner join train_diag_dtl B ON A.patientID = B.patientID")
test_pat_diag <- sqldf("SELECT * from test_pat_dtl A inner join test_diag_dtl B ON A.patientID = B.patientID")

###########No of records: train_pat_dtl - 34650 + train_diag_dtl - 34650 = train_pat_diag - 34650#######

######Removing un-necessory dataframes###########
rm(train_diag_dtl,train_pat_dtl)
rm(test_diag_dtl,test_pat_dtl)

#####Removing duplicate key column patientID#############
train_pat_diag <- train_pat_diag[,-7]
test_pat_diag <- test_pat_diag[,-6]

########Joining train_pat_diag - 34650 rows with train_hos_dtl - 34650 rows#####
final_train_set <- sqldf("SELECT * from train_pat_diag A inner join train_hos_dtl B ON A.patientID = B.patientID")
final_test_set <- sqldf("SELECT * from test_pat_diag A inner join test_hos_dtl B ON A.patientID = B.patientID")

######Removing un-necessary dataframes###########
rm(train_hos_dtl,test_hos_dtl,train_pat_diag, test_pat_diag)

#####Removing columns patientID and admission_id as these are not useful for our analysis and model########
final_train_set <- subset(final_train_set,select = -c(patientID,AdmissionID))
final_train_set <- subset(final_train_set,select = -c(37))
final_test_set <- subset(final_test_set,select = -c(AdmissionID))
final_test_set <- subset(final_test_set,select = -c(37))
############################                     ###########################

#                          PRE-PROCESSING OF DATA                          #

#########################                        ###########################


apply(final_train_set,2,function(x) sum(is.null(x)))        #Checking for nulls in each column -> NO NULLs
apply(final_train_set,2,function(x) sum(is.na(x)))          #Checking for NAs in each column   -> NO NAs
apply(final_train_set,2,function(x) sum(is.nan(x)))         #Checking for Nan in each column   -> NO NaN
apply(final_train_set,2,function(x) sum(x == '?'))          #Checking for ? in each column as we have seen many ? ->  
#payer_code, medical_specialty and weight have about more than 50 % missing values

apply(final_test_set,2,function(x) sum(is.null(x)))
apply(final_test_set,2,function(x) sum(is.na(x)))
apply(final_test_set,2,function(x) sum(is.nan(x)))
apply(final_test_set,2,function(x) sum(x == '?'))

########Removing column with more than 50% missing values################

final_train_set <- subset(final_train_set,select = -c(medical_specialty ,payer_code ,weight))

final_test_set <- subset(final_test_set,select = -c(medical_specialty ,payer_code ,weight))

str(final_train_set)
str(final_test_set)

#######Changing data-types, feature creations and transformation #############

final_train_set$admission_source_id <- as.factor(final_train_set$admission_source_id)
final_train_set$discharge_disposition_id <- as.factor(final_train_set$discharge_disposition_id)
final_train_set$admission_type_id <- as.factor(final_train_set$admission_type_id)
final_test_set$admission_source_id <- as.factor(final_test_set$admission_source_id)
final_test_set$discharge_disposition_id <- as.factor(final_test_set$discharge_disposition_id)
final_test_set$admission_type_id <- as.factor(final_test_set$admission_type_id)

####Converting date columns into date format########
final_train_set$Admission_date <- as.Date(final_train_set$Admission_date)
sum(is.null(final_train_set$Admission_date))
final_train_set$Discharge_date <- as.Date(final_train_set$Discharge_date)
sum(is.null(final_train_set$Discharge_date))
str(final_train_set)

final_test_set$Admission_date <- as.Date(final_test_set$Admission_date)
sum(is.null(final_train_set$Admission_date))
final_test_set$Discharge_date <- as.Date(final_test_set$Discharge_date)
sum(is.null(final_test_set$Discharge_date))
str(final_test_set)

#######Creating NoDaysAdmit = Discharge_date - Admission_date#######
final_train_set$NoDaysAdmit <- difftime(final_train_set$Discharge_date,final_train_set$Admission_date,units = "days")
final_train_set$NoDaysAdmit <- as.numeric(final_train_set$NoDaysAdmit)


final_test_set$NoDaysAdmit <- difftime(final_test_set$Discharge_date,final_test_set$Admission_date,units = "days")
final_test_set$NoDaysAdmit <- as.numeric(final_test_set$NoDaysAdmit)

final_train_set <- subset(final_train_set,select = -c(diagnosis_1 ,diagnosis_2 ,diagnosis_3,Admission_date,Discharge_date))
final_test_set <-  subset(final_test_set,select = -c(diagnosis_1 ,diagnosis_2 ,diagnosis_3,Admission_date,Discharge_date))


str(final_train_set)

table(final_train_set$race)
##################Checking distribution of data#############################
summary(final_train_set)

final_train_set <- subset(final_train_set,select = -c(glipizide.metformin,metformin.rosiglitazone,metformin.pioglitazone,miglitol,troglitazone,tolazamide,tolbutamide,acetohexamide  ))
final_test_set <- subset(final_test_set,select = -c(glipizide.metformin,metformin.rosiglitazone,metformin.pioglitazone,miglitol,troglitazone,tolazamide,tolbutamide,acetohexamide  ))



################# Imputation of missing values in race column ########
impute_train <- subset(final_train_set,race != '?')
impute_test <- subset(final_train_set,race == '?')
str(impute_train)
impute_train$race <- factor(impute_train$race)
impute_race <- multinom(race ~ .,data = impute_train[,-c(4)])


#######CHecking how well model imputing the values#########
factor(final_test_set[final_test_set$race != '?',2])
confusionMatrix(factor(final_test_set[final_test_set$race != '?',2]),predict(impute_race,final_test_set[final_test_set$race != '?',-c(1,2)]))

race <- predict(impute_race,impute_test[,-c(1,4)])
impute_test$race <- race

final_train_set <- rbind(impute_train,impute_test) 

test_data_impute <- final_test_set[final_test_set$race == '?',]
race <- predict(impute_race,test_data_impute[,-c(1,2)])
test_data_impute$race <- race

final_test_set <- rbind(final_test_set[final_test_set$race != '?',],test_data_impute)


#########################
train <- final_train_set 
test <- final_test_set

table(train$readmitted)

idx <- createDataPartition(train$readmitted,p=0.7,list = F)

train_data <- train[idx,]
val_data <- train[-idx,]
table(train_data$readmitted)

####################Without cost - sensitive learning #############
C5 <- C5.0(train_data$readmitted ~ .,data = train_data)


confusionMatrix(predict(C5,train_data[,-4]),train_data[,4],positive = "Within30days")
confusionMatrix( predict(C5,val_data[,-4]) ,val_data[,4],positive = "Within30days")


##############Cost - sensitive learning ###############

cost_mat <- matrix(c(0, 7, 30, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c("NO", "Within30days")

C5 <- C5.0(train_data$readmitted ~ .,data = train_data,costs = cost_mat )

confusionMatrix(predict(C5,train_data[,-4]),train_data[,4],positive = "Within30days")
confusionMatrix( predict(C5,val_data[,-4]) ,val_data[,4],positive = "Within30days")


##############Cost -sensitive with down sampling ###########
train_idx <- train[sample(nrow(train)),]

folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)


cost_mat <- matrix(c(0, 0.2, 1.2, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c("NO", "Within30days")
cost_mat
for(i in 1:5){
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  
  trainData <- downSample(trainData, trainData$readmitted, list = FALSE, yname = "readmitted")
  c5 <- C5.0(trainData$readmitted ~ .,data = trainData,costs = cost_mat,trial = 3)
  print(recall(predict(c5,trainData[,-4]),trainData[,4],relevant = "Within30days"))
  print(confusionMatrix(predict(c5,trainData[,-4]),trainData[,4],positive = "Within30days"))
  print(recall(predict(c5,testData[,-4]),testData[,4],relevant = "Within30days"))
  print(confusionMatrix(predict(c5,testData[,-4]),testData[,4],positive = "Within30days"))
}

trainData <- downSample(train, train$readmitted, list = FALSE, yname = "readmitted")
c5 <- C5.0(trainData$readmitted ~ .,data = trainData,costs = cost_mat,trial = 3)

##############Cost - sensitive with up sampling #################
for(i in 1:5){
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  
  trainData <- upSample(trainData, trainData$readmitted, list = FALSE, yname = "readmitted")
  c5 <- C5.0(trainData$readmitted ~ .,data = trainData,costs = cost_mat,trial = 3)
  print(recall(predict(c5,trainData[,-4]),trainData[,4],relevant = "Within30days"))
  print(confusionMatrix(predict(c5,trainData[,-4]),trainData[,4],positive = "Within30days"))
  print(recall(predict(c5,testData[,-4]),testData[,4],relevant = "Within30days"))
  print(confusionMatrix(predict(c5,testData[,-4]),testData[,4],positive = "Within30days"))
}

trainData <- downSample(train, train$readmitted, list = FALSE, yname = "readmitted")
c5 <- C5.0(trainData$readmitted ~ .,data = trainData,costs = cost_mat,trial = 3)

