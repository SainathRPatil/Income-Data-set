# R_Language_Building Logistics Regression_on_Income-Data-set

#library(rpart)
library(ggplot2)
library(rlist)
library(gridExtra)
library(grid)
library(corrplot)
#library(DAAG)
#Beautify tree
# library(rattle)
# library(rpart.plot)
library(RColorBrewer)
library(pROC)
library(e1071)

#read income file

getwd()
setwd("C:/Users/sainath/Documents")
income <- read.csv("adult-training.csv",na.strings =c("?","NA")) 
income
###View(income)
str(income)
summary(income)


###renaming the column names#####
names(income)<- c("Age","Workclass","fnlgwt","Education","Education_num",
         "Marital_Status","Occupation","Relationship","Race","Sex",
         "Capital_Gain","Capital_Loss","Hours_Week","Native_country","Income")
colnames(income)
str(income)
summary(income)

colnames(income)[15]<-"Target_var"

##########replacing special character "?" into "NA".
?gsub
income$Workclass<-gsub("?",NA,income$Workclass,fixed = TRUE)
income$Occupation<-gsub("?",NA,income$Occupation,fixed = TRUE)
income$Native_country<-gsub("?",NA,income$Native_country,fixed = TRUE)
str(income)
summary(income)
###View(income)
income$Workclass<-as.factor(income$Workclass)
income$Occupation<-as.factor(income$Occupation)
income$Native_country<-as.factor(income$Native_country)
str(income)

skewness(income.factor)
# #detecting and treating missing values
colSums(is.na(income))
apply(is.na(income), c(2), sum)

# % of NA's in incomeframe
sum(is.na(income))/prod(dim(income)) *100

# % of NA's contains as row
nrow(income[is.na(income),])/nrow(income) *100


#segregating Numeric & factor income
income.numeric <- income[sapply(income, is.numeric)]

#Analysing & treating NA's in numeric income
str(income.numeric)
summary(income.numeric)
income.factor <- income[sapply(income, is.factor)]
str(income.factor)
summary(income.factor)

#####################Imputation with mean
for(i in seq(income.numeric)) {
  income.numeric[i]<- ifelse(is.na(income.numeric[,i]), 
                           mean(income.numeric[,i], na.rm = T), income.numeric[,i])
}
summary(income.numeric)

#####EDA analysis###
#Analysing histogram of each numeric values
numplot <- function(column, income)
{
  ggplot(income, aes_string(x=column))+
    geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
    geom_density(fill='blue', alpha=0.2)+
    xlab(column)
}

income$Capital_Gain =as.factor(income$Capital_Gain)

np <- lapply(colnames(income.numeric), numplot, income=income.numeric)
do.call("grid.arrange", np)

#Check with skewness
income.skewed <- apply(income.numeric, c(2), skewness)
income.skewed
skewness(income.numeric$Hours_Week)

#boxplot analysis
boxplot(income.numeric$Hours_Week)

#mode function
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

##checking summary again
summary(income.factor)


#imputation with mode
for(i in seq(income.factor))
  income.factor[,i][is.na(income.factor[,i])] <- getmode(income.factor[,i])


#check summary again
summary(income.factor)

#Do outlier analysis etc...preprocess the income.


#to check outliers by means of quantile.
out_qua_fix = function(x){
  m=mean(x)
  s=sd(x)
  lc=m-3*s #lower cut-off
  uc=m+3*s
  # Q1 = quantile(x, 0.25, na.rm = T)
  # Q3 = quantile(x, 0.75, na.rm = T)
  # IQR = Q3 - Q1
  # lc = Q1 - m*IQR #lower cut-off
  # uc = Q3 + m*IQR #upper cut-off
  #n=sum(x>uc | x<lc)
  out_value <- which(x > uc | x < lc)
  #aq_qua_fix <- aq[-out_value,]
  return(aq_qua_fix)
}

out_std_check(income$Target_var) 

out_std_fix = function(x, y, f){
  m=mean(x[,y])
  s=sd(x[,y])
  lc=m-f*s #lower cut-off
  uc=m+f*s #upper cut-off
  out_value <- which(x[,y] > uc | x[,y] < lc)
  #aq_sd_fix <- aq[-out_value,]
  x[out_value,y] <- m
  x[,y]<-floor(x[,y])
  return(x)
}

income.numeric <- out_std_fix(income.numeric,1, 3) # calling fix function to fix outlier.
skewness(income.numeric)

###chisq.test and anova test
chisq.test(income.factor$Education, income.factor$Marital_Status)
chisq.test(income.factor$Relationship, income.factor$Race)
chisq.test(income.factor$Sex,income.factor$Target_var)

x <- aov(income.numeric$Hours_Week~ income.factor$Target_var)
summary(x)
summary(income.factor)
income.factor <- income.factor
##############################################
#Significance with target variable.
sigficant_var <- list()
incm<- income.factor
for(i in 1:(length(incm)))
{    k= i+1
y<-  chisq.test(table(incm[,i], incm[,k]))
vec<-c(Var=paste(colnames(incm[length(incm)]),"-",colnames(incm[i])), pval=y$p.value, significant = (if(y$p.value<0.05 & y$p.value!='NaN') "yes" else "no"))
sigficant_var<-list.append(sigficant_var, vec)
}

print(sigficant_var)
str(sigficant_var)


chisq.test(income.factor$Target_var, income.factor$Workclass)

chisq.test(income.factor$Target_var, income.factor$Education)

# y <- income.factor$Loan_Status
# x <- income.numeric$LoanAmount
#
# aov(y~ income.numeric$ApplicantIncome+income.numeric$CoapplicantIncome)
#

incm <- income.factor    #this supposed to be a income frame
x<-list()       # return var as a list
for (i in 1:length(incm))
{
  if(i < length(incm)){ k= i+1 }
  if((class(incm[,i])== class(incm[,k])) & class(incm[,i])=='factor' )
  {
    y<-  chisq.test(table(incm[,i], incm[,k]))
    vec<-c(Var=paste(colnames(incm[i]),"-",colnames(incm[k])), y$statistic, pval=y$p.value, significant = (if(y$p.value<0.05 & y$p.value!='NaN') "yes" else "no"))
    x<-list.append(x, vec)
  }
}
print(x)
#
#
#Checking co-relation matrix.
descrCor <- cor(income.numeric)
print(descrCor)
corrplot(descrCor, type = "upper", method = "number")
corrplot.mixed(descrCor, lower.col = "black", number.cex = .7)

############################################################################
#hot encoding


income.factor$SexM<- ifelse(income.factor$Sex=="Male",1,0)
income.factor$SexF <- ifelse(income.factor$Sex=="Female",1,0)

str(income.factor)
income2 <- dummy.income.frame(income.factor)
str(income2)


?dummy

####sampling#####

set.seed(20)
s=sample(1:nrow(income),0.70*nrow(income))
train.income<- income[s,]
test.income <- income[-s,]
# dim(test.income)
# View(test.income)
#test.income.new <- test.income[,-12]
test.income.new <- test.income[!colnames(test.income) %in% "Target_var"]

colnames(train.income)

#View(train.income)
#train.income <- train.income[-c(13:15)]

model=glm(Target_var~.,income =train.income,family=binomial("logit"))
summary(model)

###pridiction######

fitted.results = predict(model,newincome=test.income.new, type='response')
View(fitted.results)

fitted.results1 = ifelse(fitted.results >=0.5  ,1,0)


#confusion matrix
table(fitted.results1)
table(test.income$Income)
cf1 = table(test.income$Income, fitted.results1)
cf1

TP=117
FP=35
FN=2
TN=31

Accuracy = (TN+TP) / (TP+FP+FN+TN)    
Sensitivity=TP/(TP+FN)   
Sensitivity
specificity=TN/(TN+FP)   
specificity
Accuracy
Accuracy
error=(FP+FN)/(TP+FP+FN+TN)
error



test.income$Target_var=ifelse(test.income$Target_var=="Y",1,0)

library(pROC)
roccurve=roc(test.income$Target_var, fitted.results1)
plot(roccurve)
auc(roccurve)
