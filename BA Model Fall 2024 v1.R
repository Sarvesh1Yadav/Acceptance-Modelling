###---------------BA 05 June 2024----------------###

rm(list=ls())
library(tidyverse)
library(xgboost)
library(readxl)
library(lubridate)
library(openxlsx)
library(caret)
library(dplyr)
library(stringr)
setwd("C:/Users/virtu/OneDrive/Desktop/Acceptance Modeling/Acceptance Modeling/")
EOY=read.csv("Dummy All New Data 2016-23.csv")
Test2024=read.csv("Dummy Virtue Apps Freshmen Data 20221102-085214.csv")
# checkprevdata =read.csv("Apps All FRE for Virtue Analytics 20240404-094906.csv")
Weeklydata<- Test2024

#---dropping the unnecessary columns from the data------#
setdiff(names(Test2024), names(EOY))
Test2024$Event.Registration.Title..Rank.1.<- NULL
Test2024$Event.Registration.Date..Rank.1. <- NULL
Test2024$Application.Application.Plan<- NULL

setdiff(names(Test2024), names(EOY))
EOY$X <- NULL
Test2024$X<-NULL

#-------Removing empty cells from Campus Id
Test2024 <- Test2024[complete.cases(Test2024$Campus.ID), ]

#------checking for the duplicates-----#
duplicatesEOY <- EOY[duplicated(EOY), ]
duplicatesTest2024 <- Test2024[duplicated(Test2024), ]

#---- removing duplicates
EOY <- EOY %>% distinct(Campus.ID, .keep_all = TRUE)
Test2024 <- Test2024 %>% distinct(Campus.ID, .keep_all = TRUE)

#------checking for the duplicates-----#
duplicatesEOY <- EOY[duplicated(EOY), ]
duplicatesTest2024 <- Test2024[duplicated(Test2024), ]

Test2024<-Test2024 %>% filter(Campus.ID!=123456789)
Test2024<-Test2024 %>% filter(Campus.ID!=98798798998)

#----Filtering Business Administration ----#
unique(EOY$Admitted.to.College)
college_name="Business Administration"
EOY=subset(EOY, EOY$Admitted.to.College==college_name)
Test2024=subset(Test2024, Test2024$Admitted.to.College==college_name)

#----Making of Deposit Flag from Deposit Date----#
EOY$Deposit.Date[is.na(EOY$Deposit.Date)] <- ""
EOY$Deposit_flag=ifelse(EOY$Deposit.Date!="",1,0)
EOY$Deposit_flag=as.numeric(EOY$Deposit_flag)
EOY$Deposit.Date=NULL

# - May be we'll comment this out
Test2024$Deposit.Date[is.na(Test2024$Deposit.Date)] <- ""
Test2024$Deposit_flag=ifelse(Test2024$Deposit.Date!="",1,0)
Test2024$Deposit_flag=as.numeric(Test2024$Deposit_flag)
Test2024$Deposit.Date=NULL

Deposited=subset(EOY,EOY$Deposit_flag==1)
Non_Deposited=subset(EOY,EOY$Deposit_flag!=1)
Sample=sample_frac(Non_Deposited,.6,replace = FALSE)

EOY=rbind(Sample,Deposited)
rm(Deposited,Non_Deposited,Sample)

#----Data Cleaning & NULL treatment----#
Null_treatment<-function(column_name)
{
  column_name<-as.character(column_name)
  column_name[which(column_name == "NULL")]<-NA
  return(column_name)
}
for (i in 1:ncol(EOY)) {
  
  EOY[,i]=Null_treatment(EOY[,i])}

for (i in 1:ncol(Test2024)) {
  
  Test2024[,i]=Null_treatment(Test2024[,i])}

#----Removing NAN and infinite from the both the Data_sets----#
for (i in 1:ncol(EOY)){
  EOY[,i]=ifelse(is.infinite(EOY[,i])==T,"",EOY[,i])
  EOY[,i]=ifelse(is.nan(EOY[,i])==T,"",EOY[,i])
  EOY[,i]=ifelse(is.na(EOY[,i])==T,"",EOY[,i])}

for (i in 1:ncol(Test2024)){
  Test2024[,i]=ifelse(is.infinite(Test2024[,i])==T,"",Test2024[,i])
  Test2024[,i]=ifelse(is.nan(Test2024[,i])==T,"",Test2024[,i])
  Test2024[,i]=ifelse(is.na(Test2024[,i])==T,"",Test2024[,i])
}

#----Normalizing GPA and ACT----#
EOY$Raw.GPA=as.numeric(EOY$Raw.GPA)
Test2024$Raw.GPA=as.numeric(Test2024$Raw.GPA)

EOY$ACT.Equivalent=as.numeric(EOY$ACT.Equivalent)
Test2024$ACT.Equivalent=as.numeric(Test2024$ACT.Equivalent)

EOY$Re_cal_GPA=ifelse(EOY$Raw.GPA>4,(EOY$Raw.GPA/as.integer(EOY$Raw.GPA+1))*4,EOY$Raw.GPA)
Test2024$Re_cal_GPA=ifelse(Test2024$Raw.GPA>4,(Test2024$Raw.GPA/as.integer(Test2024$Raw.GPA+1))*4,Test2024$Raw.GPA)

EOY$Raw.GPA=NULL
Test2024$Raw.GPA=NULL

EOY$Re_cal_ACT=ifelse(EOY$ACT.Equivalent>36,(EOY$ACT.Equivalent/as.integer(EOY$ACT.Equivalent+1))*36,EOY$ACT.Equivalent)
Test2024$Re_cal_ACT=ifelse(Test2024$ACT.Equivalent>36,(Test2024$ACT.Equivalent/as.integer(Test2024$ACT.Equivalent+1))*36,Test2024$ACT.Equivalent)

EOY$ACT.Equivalent=NULL
Test2024$ACT.Equivalent=NULL

EOY$Re_cal_ACT[EOY$Re_cal_ACT>36]=NA
EOY$Re_cal_ACT[EOY$Re_cal_ACT<8]=NA

Test2024$Re_cal_ACT[Test2024$Re_cal_ACT>36]=NA
Test2024$Re_cal_ACT[Test2024$Re_cal_ACT<8]=NA

#----Adding Merit Grant in both the Data set----#
source("MU_Merit grid Calculator New.R")

EOY=subset(EOY,is.na(EOY$Re_cal_GPA)==F)
Test2024=subset(Test2024,is.na(Test2024$Re_cal_GPA)==F)

#----Adding Column of Merit Award in EOY----#
for(i in 1 :nrow(EOY)){
  
  EOY$Merit_grant[i]=MU_Merit_calculator(EOY$Admitted.to.College[i],EOY$Re_cal_GPA[i])}

#----Adding Column of Merit Award in Test2024----#
for(i in 1 :nrow(Test2024)){
  
  Test2024$Merit_grant[i]=MU_Merit_calculator(Test2024$Admitted.to.College[i],Test2024$Re_cal_GPA[i])}

#----Matching the Headers from the Historical File's Headers----#
setdiff(names(EOY),names(Test2024))

#----Treating the Null columns----#
Null_Cols=names(which(colSums(is.na(EOY))==nrow(EOY)))
Null_Cols1=names(which(colSums(is.na(Test2024))==nrow(Test2024)))
Null_Cols=union(Null_Cols, Null_Cols1);
setdiff(names(Test2024), names(EOY))

EOY[,Null_Cols]=NULL
Test2024[,Null_Cols]=NULL

common_names=intersect(names(EOY), names(Test2024));
EOY=EOY[,common_names];
Test2024=Test2024[,common_names];

unwanted_column<- c( "Preferred","Last","Suffix","Hispanic.Origin","Phone","App.Center","Standing","Non.Admit.Flag",
                     "Cancel.Flag","Complete.Date","Admit.Date","Deny.Date","Cancel.Reason","School","School.City","Cancel.Date",
                     "GPA.Type","Recalc.GPA", "GPA.Scale","Category","Organization.Flag","Physical.Therapy.Status","EOP","Twin",
                     "Undocumented" ,"Entry.Term.App","ONE.Created.Date","ONE.Submitted.Date","Applied.to.College","Admitted.to.College",
                     "Admitted.to.Major","Admit.Reason","Deny.Reason","Athletic.Training.Status","Pere.Marquette.Award",
                     "Year_Extract.Completed","Common.IDs","Current.Bin.Name","School.State","Type","Zip")

EOY[,unwanted_column]=NULL
Test2024[,unwanted_column]=NULL

# #----Normalizing the pin code----#
# EOY$CEEB=as.numeric(substr(as.character(EOY$CEEB),(nchar(as.character(EOY$CEEB))+1)-5,nchar(as.character(EOY$CEEB))))
# Test2024$CEEB=as.numeric(substr(as.character(Test2024$CEEB),(nchar(as.character(Test2024$CEEB))+1)-5,nchar(as.character(Test2024$CEEB))))

#--------------Treating CEEB----------------#
EOY$CEEB[EOY$CEEB==""]<- NA
Test2024$CEEB[Test2024$CEEB==""]<- NA

#----Transform the variable type to maintain the consistency----#
EOY$Ref=as.numeric(EOY$Ref)
Test2024$Ref=as.numeric(Test2024$Ref)

EOY$Sex=as.character(EOY$Sex)
Test2024$Sex=as.character(Test2024$Sex)

EOY$Hispanic=as.character(EOY$Hispanic)
Test2024$Hispanic=as.character(Test2024$Hispanic)

EOY$Race=as.character(EOY$Race)
Test2024$Race=as.character(Test2024$Race)

EOY$Primary.Ethnicity=as.character(EOY$Primary.Ethnicity)
Test2024$Primary.Ethnicity=as.character(Test2024$Primary.Ethnicity)

EOY$Street=as.character(EOY$Street)
Test2024$Street=as.character(Test2024$Street)

EOY$City=as.character(EOY$City)
Test2024$City=as.character(Test2024$City)

EOY$State=as.character(EOY$State)
Test2024$State=as.character(Test2024$State)

EOY$First.Gen=as.character(EOY$First.Gen)
Test2024$First.Gen=as.character(Test2024$First.Gen)

EOY$Legacy=as.character(EOY$Legacy)
Test2024$Legacy=as.character(Test2024$Legacy)

EOY$Faculty.Child=as.character(EOY$Faculty.Child)
Test2024$Faculty.Child=as.character(Test2024$Faculty.Child)

EOY$Tuition.Remission=as.character(EOY$Tuition.Remission)
Test2024$Tuition.Remission=as.character(Test2024$Tuition.Remission)

EOY$Admit_flag=ifelse(EOY$Admit.Flag!="",1,0)
EOY$Admit_flag=as.numeric(EOY$Admit_flag)
EOY$Admit.Flag=NULL

Test2024$Admit_flag=ifelse(Test2024$Admit.Flag!="",1,0)
Test2024$Admit_flag=as.numeric(Test2024$Admit_flag)
Test2024$Admit.Flag=NULL

EOY$Submitted_Flag=ifelse(EOY$Submitted.Date!="",1,0)
EOY$Submitted.Date=NULL

Test2024$Submitted_Flag=ifelse(Test2024$Submitted.Date!="",1,0)
Test2024$Submitted.Date=NULL

EOY$Submitted_Flag=as.numeric(EOY$Submitted_Flag)
Test2024$Submitted_Flag=as.numeric(Test2024$Submitted_Flag)

EOY$Deposit_flag=as.numeric(EOY$Deposit_flag)
EOY$Deposit.Date=NULL

Test2024$Deposit_flag=as.numeric(Test2024$Deposit_flag)
Test2024$Deposit.Date=NULL

EOY$CEEB=as.numeric(EOY$CEEB)
Test2024$CEEB=as.numeric(Test2024$CEEB)

EOY$MU.Feeder=if_else(EOY$MU.Feeder!="",1,0)
Test2024$MU.Feeder=if_else(Test2024$MU.Feeder!="",1,0)

EOY$Rigor=as.numeric(EOY$Rigor)
Test2024$Rigor=as.numeric(Test2024$Rigor)

EOY$Essay=as.numeric(EOY$Essay)
Test2024$Essay=as.numeric(Test2024$Essay)

EOY$Blue.and.Gold=as.character(EOY$Blue.and.Gold)
Test2024$Blue.and.Gold=as.character(Test2024$Blue.and.Gold)

EOY$Cristo.Rey=as.character(EOY$Cristo.Rey)
Test2024$Cristo.Rey=as.character(Test2024$Cristo.Rey)

EOY$Jesuit=as.character(EOY$Jesuit)
Test2024$Jesuit=as.character(Test2024$Jesuit)

EOY$Staff.Assigned=as.character(EOY$Staff.Assigned)
Test2024$Staff.Assigned=as.character(Test2024$Staff.Assigned)

EOY$Sport=as.character(EOY$Sport)
Test2024$Sport=as.character(Test2024$Sport)

EOY$FACHEX=as.character(EOY$FACHEX)
Test2024$FACHEX=as.character(Test2024$FACHEX)

EOY$Tuition.Exchange=as.character(EOY$Tuition.Exchange)
Test2024$Tuition.Exchange=as.character(Test2024$Tuition.Exchange)

EOY$Honors=as.character(EOY$Honors)
Test2024$Honors=as.character(Test2024$Honors)

EOY$Campus_Visit_Flag=ifelse(EOY$Campus.Visit!="",1,0)
Test2024$Campus_Visit_Flag=if_else(Test2024$Campus.Visit!="",1,0)

EOY$Campus.Visit=NULL
Test2024$Campus.Visit=NULL

EOY$Campus_Visit_Flag=as.numeric(EOY$Campus_Visit_Flag)
Test2024$Campus_Visit_Flag=as.numeric(Test2024$Campus_Visit_Flag)

EOY$Merit_grant=as.numeric(EOY$Merit_grant)
Test2024$Merit_grant=as.numeric(Test2024$Merit_grant)

EOY$Re_cal_GPA=as.numeric(EOY$Re_cal_GPA)
Test2024$Re_cal_GPA=as.numeric(Test2024$Re_cal_GPA)

EOY$Re_cal_ACT=as.numeric(EOY$Re_cal_ACT)
Test2024$Re_cal_ACT=as.numeric(Test2024$Re_cal_ACT)

EOY$Test.Optional=as.character(EOY$Test.Optional)
Test2024$Test.Optional=as.character(Test2024$Test.Optional)

#----Again removing NAN and infinite from the both the Data_sets----#
for (i in 1:ncol(EOY)){
  EOY[,i]=ifelse(is.infinite(EOY[,i])==T,NA,EOY[,i])
  EOY[,i]=ifelse(is.nan(EOY[,i])==T,NA,EOY[,i])
  EOY[,i]=ifelse(is.na(EOY[,i])==T,NA,EOY[,i])}

for (i in 1:ncol(Test2024)){
  Test2024[,i]=ifelse(is.infinite(Test2024[,i])==T,NA,Test2024[,i])
  Test2024[,i]=ifelse(is.nan(Test2024[,i])==T,NA,Test2024[,i])
  Test2024[,i]=ifelse(is.na(Test2024[,i])==T,NA,Test2024[,i])
}
setdiff(names(EOY),names(Test2024))

df_all_test=Test2024

# Treating the columns for modeling

EOY$Sex<-ifelse(EOY$Sex == "X", NA, EOY$Sex)
EOY$Sex<-ifelse(EOY$Sex == "e", NA, EOY$Sex)
EOY$Sex<-ifelse(EOY$Sex == "3", NA, EOY$Sex)
EOY$Sex<- as.factor(EOY$Sex)

EOY$First.Gen<-ifelse(EOY$First.Gen == "", NA, EOY$First.Gen)
EOY$First.Gen<- as.factor(EOY$First.Gen)

EOY$Legacy<-ifelse(EOY$Legacy == "", NA, EOY$Legacy)
EOY$Legacy<-ifelse(is.na(EOY$Legacy == T), 0, 1)
EOY$Legacy<- as.factor(EOY$Legacy)

EOY$Faculty.Child<-ifelse(EOY$Faculty.Child == "None", NA, EOY$Faculty.Child)
EOY$Faculty.Child<-ifelse(EOY$Faculty.Child == "", NA, EOY$Faculty.Child)
EOY$Faculty.Child<-ifelse(is.na(EOY$Faculty.Child == T), 0, 1)
EOY$Faculty.Child<- as.factor(EOY$Faculty.Child)

EOY$CEEB<-as.character(EOY$CEEB)

EOY$Test.Optional<-ifelse(EOY$Test.Optional == "", NA, EOY$Test.Optional)
EOY$Test.Optional<- as.factor(EOY$Test.Optional)

EOY$Rigor<- as.factor(EOY$Rigor)
EOY$Essay<- as.factor(EOY$Essay)

EOY$Cristo.Rey<-ifelse(EOY$Cristo.Rey == "", NA, EOY$Cristo.Rey)
EOY$Cristo.Rey<- as.factor(EOY$Cristo.Rey)

EOY$Jesuit<-ifelse(EOY$Jesuit == "", NA, EOY$Jesuit)
EOY$Jesuit<- as.factor(EOY$Jesuit)

EOY$Staff.Assigned<-ifelse(EOY$Staff.Assigned == "", NA, EOY$Staff.Assigned)

EOY$Blue.and.Gold<-ifelse(EOY$Blue.and.Gold == "", NA, EOY$Blue.and.Gold)

EOY$Sport<-ifelse(EOY$Sport == "", NA, EOY$Sport)

EOY$Campus_Visit_Flag<- as.factor(EOY$Campus_Visit_Flag)

EOY$Hispanic<-ifelse(EOY$Hispanic == "", NA, EOY$Hispanic)
EOY$Hispanic<-as.factor(EOY$Hispanic)

EOY$Race<-ifelse(EOY$Race == "", NA, EOY$Race)

EOY$State<-ifelse(EOY$State == "", NA, EOY$State)

EOY$Tuition.Remission<-ifelse(EOY$Tuition.Remission == "", NA, EOY$Tuition.Remission)

EOY$FACHEX<-ifelse(EOY$FACHEX == "", NA, EOY$FACHEX)

EOY$Tuition.Exchange<-ifelse(EOY$Tuition.Exchange == "", NA, EOY$Tuition.Exchange)

EOY$Honors<-ifelse(EOY$Honors == "", NA, EOY$Honors)

#----Source the gbm simulation file----# 


seed <- (1:1)*1000;
set.seed(22)
dt =sort(sample(nrow(EOY), nrow(EOY)*.70))

data_hist<-EOY[dt,]
val_data<-EOY[-dt,]

data_Test=Test2024
nrun=2;
id_num=data_Test$Campus.ID

train_data=data_hist
validation_data=val_data
test_data=data_Test

library(tidyverse)
library(xgboost)
predicted_msi=as.data.frame(matrix(0, nrow=nrow(test_data), ncol=nrun));
predicted_msi$ID_NUM=test_data$ID_NUM;
set.seed(1)
eta_vec=runif(nrun,0.01,0.02);
set.seed(2)
depth_vec=round(runif(nrun, 3,4));
set.seed(13)
subsample_vec=runif(nrun, 0.67, 0.69);
set.seed(12)
min_child_weight_vec=runif(nrun, 0.8, 1.0);
set.seed(15)
colsample_bytree_vec=runif(nrun, 0.5, 0.51);
set.seed(2)
gamma_vec=runif(nrun, 0, 0.025);
set.seed(6)
max_delta_step_vec=runif(nrun, 0, 0);

NAString <- NA
EARLY_STOPPING <- 1000
print.every.n <- 10
nthread <- parallel::detectCores()

TARGET <- "Deposit_flag" ;
isMaximize <- F;
nrounds <- 1000;

for(i in 1:nrun){
  EOY2021=rbind(train_data, validation_data);
  set.seed(i)
  rand_sample=sample(1:nrow(EOY2021),round(0.7*nrow(EOY2021)));
  remaining_sample=setdiff(1:nrow(EOY2021), rand_sample);
  
  train_data=EOY2021[rand_sample,];
  validation_data=EOY2021[remaining_sample,]
  
  model_features <- c("Sex","Primary.Ethnicity","City","Country","First.Gen","Legacy","Faculty.Child","CEEB","Test.Optional", "Rigor", "Essay","Blue.and.Gold","Cristo.Rey" ,"Jesuit" ,"Sport","Re_cal_GPA","Re_cal_ACT","Merit_grant", "Staff.Assigned")

  dtrain <- xgb.DMatrix(data = data.matrix(train_data[, model_features]),
                        label = data.matrix(train_data[[TARGET]]),
                        missing = NAString)
  dval <- xgb.DMatrix(data = data.matrix(validation_data[, model_features]),
                      label = data.matrix(validation_data[[TARGET]]),
                      missing = NAString)
  dtest <- xgb.DMatrix(data = data.matrix(test_data[, model_features]),
                       missing = NAString)
  watchlist <- list(train = dtrain, val = dval);
  
  param <- list(
    objective           = "binary:logistic", 
    booster             = "gbtree",
    eval_metric         = "error",
    eta                 = eta_vec[i],
    max_depth           = depth_vec[i],
    subsample           = subsample_vec[i],
    min_child_weight    = min_child_weight_vec[i],
    colsample_bytree    = colsample_bytree_vec[i],
    gamma               = gamma_vec[i],
    max_delta_step      = max_delta_step_vec[i],
    nthread             = nthread,
    num_parallel_tree   = 4
  )
  
  print(param)
  seed <- (1:1)*1000
  set.seed(seed[1])
  bst <- xgb.train(             params              = param,
                                data                = dtrain,
                                nrounds             = nrounds,
                                verbose             = 1,
                                print_every_n       = print.every.n,
                                early_stopping_rounds= EARLY_STOPPING,
                                watchlist           = watchlist,
                                maximize            = isMaximize )
  
  tmp <- predict(bst, dtest);
  tmp <- ifelse(tmp < 0, 0, tmp);
  
  predicted_msi[,i]=tmp;
  
}

xgb.ggplot.importance(feature_names = model_features,model = bst,top_n = 20,importance_matrix = xgb.importance(feature_names = model_features,model = bst),n_clusters = 1)
# importance_matrix = xgb.importance(feature_names = model_features,model = bst)
# print(importance_matrix)

model_features

msi_new=predicted_msi

MSI_Output=data.frame(Ref=id_num, Min=rep(0, nrow(msi_new)), Mean=rep(0, nrow(msi_new)), Max=rep(0, nrow(msi_new)))

for(i in 1:nrow(msi_new)){
  gar_vec=sort(as.numeric(msi_new[i, 1:nrun]));
  MSI_Output$Min[i]=min(head(tail(gar_vec, round(0.9*length(gar_vec))),round(0.80*length(gar_vec))));
  MSI_Output$Mean[i]=mean(as.numeric(head(tail(gar_vec, round(0.9*length(gar_vec))),round(0.80*length(gar_vec)))));
  MSI_Output$Max[i]=max(head(tail(gar_vec, round(0.9*length(gar_vec))),round(0.80*length(gar_vec))));
}

names(MSI_Output)[names(MSI_Output)=="Ref"]<-"Campus.ID"
data_Test$Campus.ID=as.numeric(data_Test$Campus.ID)
MSI_Output$Campus.ID=as.numeric(MSI_Output$Campus.ID)
MSI_Output=left_join(data_Test, MSI_Output, by="Campus.ID");
hist(MSI_Output$Mean)

# cutoff <- 0.5
# Confusion_Matrix=ModelMetrics::confusionMatrix(val_data[[TARGET]], tmp, cutoff)
# Confusion_Matrix
# 
# # Accuracy=positive_True+ All_result
# Accuracy=(Confusion_Matrix[1,1]+Confusion_Matrix[2,2])/sum(Confusion_Matrix)
# Accuracy

# write.csv(MSI_Output,"Business Administration.csv")

# Making Client Delivery File
#rm(list = ls())
#MSI_Output=read.csv("Business Administration.csv")

dim(MSI_Output)
File=MSI_Output
File_names=str_replace_all(str_to_title(names(File)), "[^[:alnum:]]", "_")

Raw_Test2024 = Weeklydata
Raw_Test2024$Sat=""

Wanted_column=c("Ref","Campus.ID","Preferred","Last","ACT.Equivalent","Raw.GPA","Athletic.Training.Status","Legacy","Faculty.Child","Twin","First.Gen","Rigor","Test.Optional","Sat","Admitted.to.College","Submitted.Date","Complete.Date","School","Deposit.Date")
Raw_Test2024=Raw_Test2024[,Wanted_column]
MSI_Output=MSI_Output[,c("Campus.ID","Re_cal_GPA","Re_cal_ACT","Merit_grant","Mean")]

Business_Adm_Report=right_join(Raw_Test2024,MSI_Output, by="Campus.ID")
Business_Adm_Report=Business_Adm_Report[order(Business_Adm_Report$Mean,decreasing = T),]
Business_Adm_Report$Rank=1:nrow(Business_Adm_Report)

# Change the date to the current Date
Business_Adm_Report$Week_code=rep(as.POSIXct("2024-06-05", format="%Y-%m-%d", tz = "UTC"),nrow(Business_Adm_Report))
Business_Adm_Report$week_code=as.numeric(Business_Adm_Report$Week_code)
Business_Adm_Report$MSI=round(Business_Adm_Report$Mean*100)

# For Back up <- save the file
# write.csv(Business_Adm_Report,file = "BA_back_up_file.csv",row.names = FALSE,na="")
# 
# # Adjusting MSI
# 
# rm(list = ls())
# Business_Adm_Report=read.csv("BA_back_up_file.csv")

hist(Business_Adm_Report$Mean)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0 & Business_Adm_Report$Mean<0.10,Business_Adm_Report$Mean*1,Business_Adm_Report$Mean)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.10 & Business_Adm_Report$Mean<0.20,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.20 & Business_Adm_Report$Mean<0.30,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.30 & Business_Adm_Report$Mean<0.40,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.40 & Business_Adm_Report$Mean<0.50,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.50 & Business_Adm_Report$Mean<0.60,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI) 
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.60 & Business_Adm_Report$Mean<0.70,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.70 & Business_Adm_Report$Mean<0.80,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.80 & Business_Adm_Report$Mean<0.90,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report$Adjusted_MSI=ifelse(Business_Adm_Report$Mean>=0.90 & Business_Adm_Report$Mean<1.00,Business_Adm_Report$Mean*1,Business_Adm_Report$Adjusted_MSI)

hist(Business_Adm_Report$Mean)
hist(Business_Adm_Report$Adjusted_MSI)
Business_Adm_Report=Business_Adm_Report[order(Business_Adm_Report$Adjusted_MSI,decreasing = T),]
Business_Adm_Report$Adjusted_Rank=1:nrow(Business_Adm_Report)

Business_Adm_Report=Business_Adm_Report[,c("Campus.ID","Preferred","Last","ACT.Equivalent","Test.Optional","Re_cal_GPA","Athletic.Training.Status","Legacy","Faculty.Child","Twin","First.Gen","Rigor","Sat","Adjusted_Rank","week_code","Adjusted_MSI","Admitted.to.College","Submitted.Date","Complete.Date","Week_code","School","Deposit.Date")]
names(Business_Adm_Report)[names(Business_Adm_Report)=="Campus.ID"]="campus_id"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Adjusted_Rank"]="Rank"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Adjusted_MSI"]="MSI"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Re_cal_GPA"]="Recalc_gpa"
names(Business_Adm_Report)[names(Business_Adm_Report)=="ACT.Equivalent"]="ACT_Equivalent"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Test.Optional"]="Test_Optional"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Athletic.Training.Status"]="athletic_training_status"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Faculty.Child"]="Faculty_Child"
names(Business_Adm_Report)[names(Business_Adm_Report)=="First.Gen"]="First_Gen"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Week"]="WeeK_Code"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Admitted.to.College"]="admitted_to_college"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Submitted.Date"]="Submitted_Date"
names(Business_Adm_Report)[names(Business_Adm_Report)=="Complete.Date"]="Complete_Date"

Business_Adm_Report$MSI=round(Business_Adm_Report$MSI*100)
Business_Adm_Report$Recalc_gpa=format(round(Business_Adm_Report$Recalc_gpa,2))
names(Business_Adm_Report)=tolower(names(Business_Adm_Report))
Business_Adm_Report$Week_code=as.numeric(Business_Adm_Report$week_code)
Business_Adm_Report$week_code=NULL

Weekly_report=Business_Adm_Report[,c("campus_id","preferred","last","act_equivalent","recalc_gpa","rigor","rank","msi","week_code","submitted_date","complete_date","admitted_to_college","school","deposit.date")]

write.csv(Weekly_report,file = "Business Administration 060524.csv",row.names = FALSE,na="")
#getwd()


# Extracting New Entries from the Data Last week Data Set

