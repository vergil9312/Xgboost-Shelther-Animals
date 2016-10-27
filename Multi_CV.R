# Please read
# Fit multinomical logistic regression with different or new variables(line 10)
# If you think it overfits, change the seed and see if the multi-log-loss is still low (line 313)

finaltrain <- read.csv("~/Documents/Academics/2016 Spring/Stats 101C/Data Sets/finaltrain.csv")

## 75% of the sample size
smp_size <- floor(0.75 * nrow(finaltrain))
## set the seed to make your partition reproductible
set.seed(122231)
train_ind <- sample(seq_len(nrow(finaltrain)), size = smp_size)
train <- finaltrain[train_ind, ]
test <- finaltrain[-train_ind, ]

# change some levels of OutCatg to "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="FOSTER"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="INVENTORY"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="RTO"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="TRANSFER"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="UNKNOWN"] <- "OTHER"

# first spliting the intake date data into year, month, and day. 
Intake_date<- as.character.Date(train$Intake.Date)
Intake_day<- substr(Intake_date, nchar(Intake_date)-1,nchar(Intake_date))
Intake_month<- substr(Intake_date, nchar(Intake_date)-4, nchar(Intake_date)-3)
Intake_year <- substr(Intake_date, nchar(Intake_date)-9, nchar(Intake_date)-6)
data_intake<- data.frame(Intake_year, Intake_day, Intake_month)
results_train<- train$OutCatg
data_intake$results <- results_train
### separate intake date, into year month day and attach to the training data. 
train$Intake_year <-factor(Intake_year)
train$Intake_month <-factor(Intake_month)
train$Intake_day <- factor(Intake_day)
train <- train[,-8]

### separete outcome date into year month day
outcome_date <- as.character.Date(train$Outcome.Date)
outcome_day<- substr(outcome_date, nchar(outcome_date)-1,nchar(outcome_date))
outcome_month<- substr(outcome_date, nchar(outcome_date)-4, nchar(outcome_date)-3)
outcome_year <- substr(outcome_date, nchar(outcome_date)-9, nchar(outcome_date)-6)
train$outcome_day <- factor(outcome_day)
train$outcome_month <-factor(outcome_month)
train$outcome_year <- factor(outcome_year)
train <- train[,-11]
### convert 
train$outcome_day <- as.character(train$outcome_day)
train$outcome_day[train$outcome_day == "  "] <- NA   ## 372 missing data, match with our orginal data. 
train$outcome_day <- as.factor(train$outcome_day)
train$outcome_month <- as.character(train$outcome_month)
train$outcome_month[train$outcome_month == "  "] <- NA
train$outcome_month <- as.factor(train$outcome_month)
sum(is.na(train$outcome_month)) ## 372 missing data, match with our orginal data. 
train$outcome_year <- as.character(train$outcome_year)
train$outcome_year[train$outcome_year == "NA  "] <- NA
train$outcome_year <- as.factor(train$outcome_year)
sum(is.na(train$outcome_year)) ## 372 missing data, match with our orginal data. 

### separate license date into year month and day
sum(is.na(train$License.Date))  # 110014 missing value.
license_date <- as.character.Date(train$License.Date)
license_day<- substr(license_date, nchar(license_date)-1, nchar(license_date))
license_month<- substr(license_date, nchar(license_date)-4, nchar(license_date)-3)
license_year<- substr(license_date, nchar(license_date)-9, nchar(license_date)-6)
train$license_day <- license_day
train$license_month <- license_month
train$license_year <- license_year
train<- train[,-13]
train$license_day <- as.character(train$license_day)
train$license_day[train$license_day == "  "] <- NA
train$license_day <- factor(train$license_day)
train$license_month <- as.character(train$license_month)
train$license_month[train$license_month == "  "] <- NA
train$license_month <- factor(train$license_month)
train$license_year <- as.character(train$license_year)
train$license_year[train$license_year == "NA  "] <- NA
train$license_year <- factor(train$license_year)
sum(is.na(train$license_year)) # match

### separate BOD date into year month and day. 
sum(is.na(train$DOB))  # 8832 missing
BOD<- as.character.Date(train$DOB)
BOD_day<- substr(BOD, nchar(BOD)-1,nchar(BOD))
BOD_month<- substr(BOD, nchar(BOD)-4, nchar(BOD)-3)
BOD_year <- substr(BOD, nchar(BOD)-9, nchar(BOD)-6)
train$BOD_day <- factor(BOD_day)
train$BOD_month <-factor(BOD_month)
train$BOD_year <- factor(BOD_year)
train <- train[,-6]
train$BOD_day <- as.character(train$BOD_day)
train$BOD_day[train$BOD_day == "  "] <- NA
train$BOD_day <- factor(train$BOD_day)
sum(is.na(train$BOD_day)) # match
train$BOD_month <- as.character(train$BOD_month)
train$BOD_month[train$BOD_month == "  "] <- NA
train$BOD_month <- factor(train$BOD_month)
sum(is.na(train$BOD_month))  # match
train$BOD_year <- as.character(train$BOD_year)
train$BOD_year[train$BOD_year == "NA  "] <- NA
train$BOD_year <- factor(train$BOD_year)
sum(is.na(train$BOD_year))  # match

### separate S.N date into year month and day. 
sum(is.na(train$S.N.Date)) # 74068
S.N.Date<- as.character.Date(train$S.N.Date)
S.N_day<- substr(S.N.Date, nchar(S.N.Date)-1,nchar(S.N.Date))
S.N_month<- substr(S.N.Date, nchar(S.N.Date)-4, nchar(S.N.Date)-3)
S.N_year <- substr(S.N.Date, nchar(S.N.Date)-9, nchar(S.N.Date)-6)
train$S.N_day <- factor(S.N_day)
train$S.N_month <-factor(S.N_month)
train$S.N_year <- factor(S.N_year)
train <- train[,-6]
train$S.N_day <- as.character(train$S.N_day)
train$S.N_day[train$S.N_day == "  "] <- NA
train$S.N_day <- factor(train$S.N_day)
sum(is.na(train$S.N_day)) # match
train$S.N_month <- as.character(train$S.N_month)
train$S.N_month[train$S.N_month == "  "] <- NA
train$S.N_month <- factor(train$S.N_month)
sum(is.na(train$S.N_month))  # match
train$S.N_year <- as.character(train$S.N_year)
train$S.N_year[train$S.N_year == "NA  "] <- NA
train$S.N_year <- factor(train$S.N_year)
sum(is.na(train$S.N_year))  # match

##################################################################################################
# deleting the name variable
train <- train[,-2]

# create variable of "license", with no license(0) or with license(1)
length(which(is.na(train[,"license_year"])))
train$license <- NA
train[which(is.na(train[,"license_year"])),]$license <- 0
train[which(is.na(train[,"license"])),]$license <- 1

# change varibles "Microchip.Status" to dummy variable
length(which(is.na(train[,"Microchip.Status"])))
train$micro <- NA
train[which(is.na(train[,"Microchip.Status"])),]$micro <- 0
train[which(is.na(train[,"micro"])),]$micro <- 1

train$license <- as.factor(train$license)
train$micro <- as.factor(train$micro)

##################################################################################################
#reformat testing data. 

# first spliting the intake date data into year, month, and day. 
Intake_date<- as.character.Date(test$Intake.Date)
Intake_day<- substr(Intake_date, nchar(Intake_date)-1,nchar(Intake_date))
Intake_month<- substr(Intake_date, nchar(Intake_date)-4, nchar(Intake_date)-3)
Intake_year <- substr(Intake_date, nchar(Intake_date)-9, nchar(Intake_date)-6)
data_intake<- data.frame(Intake_year, Intake_day, Intake_month)
results_test<- test$OutCatg
data_intake$results <- results_test
### separate intake date, into year month day and attach to the testing data. 
test$Intake_year <-factor(Intake_year)
test$Intake_month <-factor(Intake_month)
test$Intake_day <- factor(Intake_day)
test <- test[,-8]
### separete outcome date into year month day
outcome_date <- as.character.Date(test$Outcome.Date)
outcome_day<- substr(outcome_date, nchar(outcome_date)-1,nchar(outcome_date))
outcome_month<- substr(outcome_date, nchar(outcome_date)-4, nchar(outcome_date)-3)
outcome_year <- substr(outcome_date, nchar(outcome_date)-9, nchar(outcome_date)-6)
test$outcome_day <- factor(outcome_day)
test$outcome_month <-factor(outcome_month)
test$outcome_year <- factor(outcome_year)
test <- test[,-11]
### convert
test$outcome_day <- as.character(test$outcome_day)
test$outcome_day[test$outcome_day == "  "] <- NA   ## 372 missing data, match with our orginal data. 
test$outcome_day <- as.factor(test$outcome_day)
test$outcome_month <- as.character(test$outcome_month)
test$outcome_month[test$outcome_month == "  "] <- NA
test$outcome_month <- as.factor(test$outcome_month)
test$outcome_year <- as.character(test$outcome_year)
test$outcome_year[test$outcome_year == "NA  "] <- NA
test$outcome_year <- as.factor(test$outcome_year)
sum(is.na(test$outcome_year)) ## 372 missing data, match with our orginal data. 

### separate license date into year month and day
sum(is.na(test$License.Date))  # 110014 missing value.
license_date <- as.character.Date(test$License.Date)
license_day<- substr(license_date, nchar(license_date)-1, nchar(license_date))
license_month<- substr(license_date, nchar(license_date)-4, nchar(license_date)-3)
license_year<- substr(license_date, nchar(license_date)-9, nchar(license_date)-6)
test$license_day <- license_day
test$license_month <- license_month
test$license_year <- license_year
test<- test[,-13]
test$license_day <- as.character(test$license_day)
test$license_day[test$license_day == "  "] <- NA
test$license_day <- factor(test$license_day)
sum(is.na(test$license_day))  # match
test$license_month <- as.character(test$license_month)
test$license_month[test$license_month == "  "] <- NA
test$license_month <- factor(test$license_month)
sum(is.na(test$license_month))  # match
test$license_year <- as.character(test$license_year)
test$license_year[test$license_year == "NA  "] <- NA
test$license_year <- factor(test$license_year)
sum(is.na(test$license_year)) # match

### separate BOD date into year month and day. 
BOD<- as.character.Date(test$DOB)
BOD_day<- substr(BOD, nchar(BOD)-1,nchar(BOD))
BOD_month<- substr(BOD, nchar(BOD)-4, nchar(BOD)-3)
BOD_year <- substr(BOD, nchar(BOD)-9, nchar(BOD)-6)
test$BOD_day <- factor(BOD_day)
test$BOD_month <-factor(BOD_month)
test$BOD_year <- factor(BOD_year)
test <- test[,-6]
test$BOD_day <- as.character(test$BOD_day)
test$BOD_day[test$BOD_day == "  "] <- NA
test$BOD_day <- factor(test$BOD_day)
sum(is.na(test$BOD_day)) # match
test$BOD_month <- as.character(test$BOD_month)
test$BOD_month[test$BOD_month == "  "] <- NA
test$BOD_month <- factor(test$BOD_month)
test$BOD_year <- as.character(test$BOD_year)
test$BOD_year[test$BOD_year == "NA  "] <- NA
test$BOD_year <- factor(test$BOD_year)
sum(is.na(test$BOD_year))  # match

### separate S.N date into year month and day. 
sum(is.na(test$S.N.Date)) # 74068
S.N.Date<- as.character.Date(test$S.N.Date)
S.N_day<- substr(S.N.Date, nchar(S.N.Date)-1,nchar(S.N.Date))
S.N_month<- substr(S.N.Date, nchar(S.N.Date)-4, nchar(S.N.Date)-3)
S.N_year <- substr(S.N.Date, nchar(S.N.Date)-9, nchar(S.N.Date)-6)
test$S.N_day <- factor(S.N_day)
test$S.N_month <-factor(S.N_month)
test$S.N_year <- factor(S.N_year)
test <- test[,-6]
test$S.N_day <- as.character(test$S.N_day)
test$S.N_day[test$S.N_day == "  "] <- NA
test$S.N_day <- factor(test$S.N_day)
sum(is.na(test$S.N_day)) # match
test$S.N_month <- as.character(test$S.N_month)
test$S.N_month[test$S.N_month == "  "] <- NA
test$S.N_month <- factor(test$S.N_month)
sum(is.na(test$S.N_month))  # match
test$S.N_year <- as.character(test$S.N_year)
test$S.N_year[test$S.N_year == "NA  "] <- NA
test$S.N_year <- factor(test$S.N_year)
sum(is.na(test$S.N_year))  # match

# deleting the name variable
test <- test[,-2]

# create variable of "license", with no license(0) or with license(1)
length(which(is.na(test[,"license_year"])))
test$license <- NA
test[which(is.na(test[,"license_year"])),]$license <- 0
test[which(is.na(test[,"license"])),]$license <- 1

# change varibles "Microchip.Status" to dummy variable
length(which(is.na(test[,"Microchip.Status"])))
test$micro <- NA
test[which(is.na(test[,"Microchip.Status"])),]$micro <- 0
test[which(is.na(test[,"micro"])),]$micro <- 1

test$license <- as.factor(test$license)
test$micro <- as.factor(test$micro)

#####################################################################################

# further delete some variables
train_3 <- train[,-c(3,5,6,8)]   ### daataset use for tree. 
train_3$outcome_year <- as.numeric(as.character(train_3$outcome_year))
train_3$BOD_year <- as.numeric(as.character(train_3$BOD_year))
age <- train_3$outcome_year - train_3$BOD_year
train_3$Age <-age
train_4 <- train_3[,-c(18)]
train_4$Age <- train_4$Age
Intake.Type <- train$Intake.Type
train_4$Intake.Type <- Intake.Type

test$outcome_year <- as.numeric(as.character(test$outcome_year))
test$BOD_year <- as.numeric(as.character(test$BOD_year))
age <- test$outcome_year - test$BOD_year
test$Age <-age

Primary.Breed <- finaltrain$Primary.Breed
train_4$Primary.Breed <- Primary.Breed

# deleting license month use tree and data set without missing data, use na.roughfix to fix the missing value
train_4<- train_4[,-13]        ### train_4 with missing data



################################################################################################
# Final Manipulation
# remove primary breed in train_5 and test and replace it with dummy variable "pure" (1 if pure, 0 if impure)

library(randomForest)
test1 <- na.roughfix(test)  ####   make sure testing dataset has no missing value

################################################################################################
## use multinomial logistic regression with train_5(the one without missing value)

library(nnet)
model <- multinom(OutCatg~micro+Sex+Age+Species+Shelter+outcome_month+Intake_month+Intake_year+license+Intake.Type, data=train_5)
# summary(model)
pred.tree2 <- predict(model, newdata = test1, type = "prob")
pred.tree2 <- as.data.frame(pred.tree2)
name_test2  <- test$ARN
pred.tree2$ARN <- name_test2
pred.tree2 <- pred.tree2[c("ARN", "ADOPTION","EUTHANASIA","OTHER")]

# Evaluate Performance
library(MLmetrics)
MultiLogLoss(y_true = test1$OutCatg, y_pred = pred.tree2[,-1]) #multi-log-loss

#####################################################################################################

# save.image("~/Documents/Academics/2016 Spring/Stats 101C/Final/Models/CV/CV.RData")
