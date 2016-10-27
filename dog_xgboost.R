finaltrain <- read.csv("finaltrain.csv")
train<- finaltrain
finaltest <- read.csv("finaltest.csv")
test<- finaltest

train <- train[which(train$Species=="DOG"),]
test <- test[which(test$Species=="DOG"),]


train.p = read.csv("training.pattern.csv")
test.p <- read.csv("testing.pattern.csv")



####Training Date

# variable pattern
train <- merge(train,train.p,by="ARN")
train$pattern <- train$OutCatg2
train$OutCatg2 <- NULL
train$pattern <- as.character(train$pattern)
train$pattern[which(is.na(train$pattern))] <- "missing"
train$pattern <- as.factor(train$pattern)

# add outcome and chip 
train$Microchip.Date <- as.character(train$Microchip.Date)
train$Outcome.Date <- as.character(train$Outcome.Date)
train$chip_outcome <- ""
train$chip_outcome[which(train$Microchip.Date==train$Outcome.Date)] <- "yes"
train$chip_outcome[-which(train$Microchip.Date==train$Outcome.Date)] <- "no"
train$chip_outcome <- as.factor(train$chip_outcome)
train$Microchip.Date <- as.factor(train$Microchip.Date)
train$Outcome.Date <- as.factor(train$Outcome.Date)


#Outcome level

levels(train$OutCatg)[levels(train$OutCatg)=="FOSTER"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="INVENTORY"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="RTO"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="TRANSFER"] <- "OTHER"
levels(train$OutCatg)[levels(train$OutCatg)=="UNKNOWN"] <- "OTHER"

# Create Outcome Weekday
library(lubridate)
Outcome.weekday <- weekdays(as.Date(train$Outcome.Date))
train$Outcome.weekday <- factor(Outcome.weekday)

# Create Outcome month, year, day
Outcome.month <- month(as.Date(train$Outcome.Date))
train$Outcome.month <- factor(Outcome.month)
Outcome.year <- year(as.Date(train$Outcome.Date))
train$Outcome.year <- factor(Outcome.year)
Outcome.day <- day(as.Date(train$Outcome.Date))
train$Outcome.day <- factor(Outcome.day)

# Create Age
DOB.year <- year(as.Date(train$DOB))
age <- as.numeric(as.character(train$Outcome.year)) - as.numeric(as.character(DOB.year))
train$age <- factor(age)

# Create DOB year, month, day
train$DOB.year <- factor(DOB.year)
DOB.month <- month(as.Date(train$DOB))
train$DOB.month <- factor(DOB.month)
DOB.day <- day(as.Date(train$DOB))
train$DOB.day <- factor(DOB.day)

# Create Intake year, month, day
Intake.year <- year(as.Date(train$Intake.Date))
train$Intake.year <- factor(Intake.year)
Intake.month <- month(as.Date(train$Intake.Date))
train$Intake.month <- factor(Intake.month)
Intake.day <- day(as.Date(train$Intake.Date))
train$Intake.day <- factor(Intake.day)

# Create Shelter Duration
duration <- difftime(as.Date(train$Outcome.Date), as.Date(train$Intake.Date), units = c("days"))
train$duration <- factor(duration)


# Create pure
#train$Primary.Breed <- as.character(train$Primary.Breed)
#train$Primary.Breed[is.na(train$Primary.Breed)] <- "Unknown"
#train$Primary.Breed[train$Primary.Breed != 0] <- 1


# Arrange S.N Date to Yes and No and uk
train$S.N.Date <- as.character(train$S.N.Date)
train$S.N.Date[is.na(train$S.N.Date)] <- 1
train$S.N.Date[train$Sex == "F"] <- 0
train$S.N.Date[train$Sex == "M"] <- 0
train$S.N.Date[train$Sex == "N"] <- 1
train$S.N.Date[train$Sex == "S"] <- 1
train$S.N.Date[train$Sex == "U"] <- 2


# arrange mircochip and licence 
train$ml <- ""
train$ml[which(is.na(train$Microchip.Status) & is.na(train$License.Date))] <- "no"
train$ml[-which(is.na(train$Microchip.Status) & is.na(train$License.Date))] <- "yes"


# Arrange License to License and NoLic
train$License.Date <- as.character(train$License.Date)
train$License.Date[is.na(train$License.Date)] <- 0
train$License.Date[train$License.Date != 0] <- 1
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)

# import zipcode data in. 
library(zipcode)
colnames(train)[11] <- "zip"
data("zipcode")
train <- merge.data.frame(train, zipcode, by.x = "zip", by.y = "zip",
                          all.x = TRUE, sort = FALSE)
train$zip <- as.numeric(train$zip)
train$city <- as.factor(train$city)
train$state  <- as.factor(train$state)

# adding gov data in 
gov <- read.csv("gov.data.csv")
gov1 <- gov[,c(2,3,4,5)]
compensation <- gov$N02300
gov1$num_compen <- compensation
colnames(gov1) <- c("STATE", "zip", "agi_stub", "N_return", "num_compen")

gov2 <-  gov1[gov1$agi_stub== 5,]
head(gov2,100)
list(levels(gov2$STATE))

n1 <- gov1[which(gov1$agi_stub == 1), ]$N_return 
n2 <- gov1[which(gov1$agi_stub == 2), ]$N_return 
n3 <- gov1[which(gov1$agi_stub == 3), ]$N_return
n4 <- gov1[which(gov1$agi_stub == 4), ]$N_return 
n5 <- gov1[which(gov1$agi_stub == 5) ,]$N_return
n6 <- gov1[which(gov1$agi_stub == 6) ,]$N_return

rich_percent <- (n4 +n5 +n6)/ (n1+n2+n3+n4+n5+n6)
gov2$rich_percent <- rich_percent
gov3 <- gov2[-1,-c(3,4,5)]
#gov3[which(gov3$zip==0),]$zip <-NA
train[which(train$zip==0),]$zip <- NA
train <- merge(train,gov3, by.x = "zip",all.x = TRUE)


# add name 
# adding name 
train$NAME <- as.character(train$NAME)
train$NAME[is.na(train$NAME)] <- 0
train$NAME[train$NAME!= 0]<- 1
train$NAME <- as.factor(train$NAME)

# 
library(randomForest)
train <- na.roughfix(train)

train$Species <- droplevels(train$Species)

###############################################
# variable pattern
test <- merge(test,test.p,by="ARN")
test$pattern <- test$OutCatg2
test$OutCatg2 <- NULL
test$pattern <- as.character(test$pattern)
test$pattern[which(is.na(test$pattern))] <- "missing"
test$pattern <- as.factor(test$pattern)


test$Microchip.Date <- as.character(test$Microchip.Date)
test$Outcome.Date <- as.character(test$Outcome.Date)
test$chip_outcome <- ""
test$chip_outcome[which(test$Microchip.Date==test$Outcome.Date)] <- "yes"
test$chip_outcome[-which(test$Microchip.Date==test$Outcome.Date)] <- "no"
test$chip_outcome <- as.factor(test$chip_outcome)
test$Microchip.Date <- as.factor(test$Microchip.Date)
test$Outcome.Date <- as.factor(test$Outcome.Date)



library(lubridate)
Outcome.weekday <- weekdays(as.Date(test$Outcome.Date))
test$Outcome.weekday <- factor(Outcome.weekday)

# Create Outcome month, year, day
Outcome.month <- month(as.Date(test$Outcome.Date))
test$Outcome.month <- factor(Outcome.month)
Outcome.year <- year(as.Date(test$Outcome.Date))
test$Outcome.year <- factor(Outcome.year)
Outcome.day <- day(as.Date(test$Outcome.Date))
test$Outcome.day <- factor(Outcome.day)

# Create Age
DOB.year <- year(as.Date(test$DOB))
age <- as.numeric(as.character(test$Outcome.year)) - as.numeric(as.character(DOB.year))
test$age <- factor(age)

# Create DOB year, month, day
test$DOB.year <- factor(DOB.year)
DOB.month <- month(as.Date(test$DOB))
test$DOB.month <- factor(DOB.month)
DOB.day <- day(as.Date(test$DOB))
test$DOB.day <- factor(DOB.day)

# Create Intake year, month, day
Intake.year <- year(as.Date(test$Intake.Date))
test$Intake.year <- factor(Intake.year)
Intake.month <- month(as.Date(test$Intake.Date))
test$Intake.month <- factor(Intake.month)
Intake.day <- day(as.Date(test$Intake.Date))
test$Intake.day <- factor(Intake.day)

# Create Shelter Duration
duration <- difftime(as.Date(test$Outcome.Date), as.Date(test$Intake.Date), units = c("days"))
test$duration <- factor(duration)



# Create pure
#test$Primary.Breed <- as.character(test$Primary.Breed)
#test$Primary.Breed[is.na(test$Primary.Breed)] <- "Unknown"
#test$Primary.Breed[test$Primary.Breed != 0] <- 1


# Arrange S.N Date to Yes and No and uk
test$S.N.Date <- as.character(test$S.N.Date)
test$S.N.Date[is.na(test$S.N.Date)] <- 1
test$S.N.Date[test$Sex == "F"] <- 0
test$S.N.Date[test$Sex == "M"] <- 0
test$S.N.Date[test$Sex == "N"] <- 1
test$S.N.Date[test$Sex == "S"] <- 1
test$S.N.Date[test$Sex == "U"] <- 2

# arrage mirc and licence 
test$ml <- ""
test$ml[which(is.na(test$Microchip.Status) & is.na(test$License.Date))] <- "no"
test$ml[-which(is.na(test$Microchip.Status) & is.na(test$License.Date))] <- "yes"



# Arrange License to License and NoLic
test$License.Date <- as.character(test$License.Date)
test$License.Date[is.na(test$License.Date)] <- 0
test$License.Date[test$License.Date != 0] <- 1
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], as.factor)

# adding zipcode data
colnames(test)[11] <- "zip"
test <- merge.data.frame(test, zipcode, by.x = "zip", by.y = "zip",
                         all.x = TRUE, sort = FALSE)

test$zip <- as.numeric(test$zip)
test$city <- as.factor(test$city)
test$state  <- as.factor(test$state)

# adding gov data in 
test[test$zip==0,]$zip <-NA 
test <- merge(test,gov3, by= "zip", all.x = TRUE)


# adding name
test$NAME <- as.character(test$NAME)
test$NAME[is.na(test$NAME)] <- 0
test$NAME[test$NAME!= 0]<- 1
test$NAME <- as.factor(test$NAME)



test <- na.roughfix(test)

test$Species <- droplevels(test$Species)



train$Primary.Breed <- as.character(train$Primary.Breed)
breed <- intersect(train$Primary.Breed, test$Primary.Breed)
for (i in 1:length(train$Primary.Breed)){
  if (!(train$Primary.Breed[i] %in% breed)) {train$Primary.Breed[i] <- "UNKNOWN "}
}

train$Primary.Breed <- as.factor(train$Primary.Breed)


test$Primary.Breed <- as.character(test$Primary.Breed)
for (i in 1:length(test$Primary.Breed)){
  
  if (!(test$Primary.Breed[i] %in% breed)){test$Primary.Breed[i] <- "UNKNOWN "}
}
test$Primary.Breed <- as.factor(test$Primary.Breed)

# color marking
train$Color.Markings <- as.character(train$Color.Markings)
test$Color.Markings <- as.character(test$Color.Markings)
color <- intersect(train$Color.Markings, test$Color.Markings)

for (i in 1:length(train$Color.Markings)){
  if (!(train$Color.Markings[i] %in% color)){train$Color.Markings[i] <- "UNKNOWN"}
  
}
train$Color.Markings <- as.factor(train$Color.Markings)

for (i in 1:length(test$Color.Markings)){
  
  if (!(test$Color.Markings[i] %in% color)) {test$Color.Markings[i] <- "UNKNOWN"}
  
  
}

test$Color.Markings <- as.factor(test$Color.Markings)
#################################################################################

library(xgboost)
library(methods)
library(data.table)
library(magrittr)
library(caret)
outcome <- train[, "OutCatg"]
num.class <- length(levels(outcome))
levels(outcome) <- 1 : num.class
train$OutCatg = NULL
test.ARN <- test$ARN
train$ARN = NULL
test$ARN = NULL
train$STATE = NULL
test$STATE = NULL

zero.var <- nearZeroVar(train, saveMetrics = TRUE)
zero.var ## Testing variables with near zero variance
## Deletig variables with near zero var
train$DOB = NULL
train$Microchip.Date = NULL
train$License.Date = NULL
test$DOB = NULL
test$Microchip.Date = NULL
test$License.Date = NULL

train$Intake.Date = NULL
test$Intake.Date = NULL


levels(train$Intake.Type) <- 1:8
levels(test$Intake.Type) <- 1:8
levels(train$Shelter) <- 1:8
levels(test$Shelter) <- 1:8
levels(train$Outcome.weekday) <- 1:7
levels(test$Outcome.weekday) <- 1:7
levels(train$Primary.Breed) <- 1:190
levels(test$Primary.Breed) <- 1:190
levels(train$Color.Markings) <- 1:265
levels(test$Color.Markings) <- 1:265
train$Outcome.Date = NULL
test$Outcome.Date = NULL

train$zip = NULL
test$zip = NULL


train$state = NULL
test$state = NULL

train$Sex = NULL
test$Sex = NULL

train$city = NULL
test$city = NULL

train$Microchip.Status = NULL
test$Microchip.Status = NULL

train$Color.Markings = NULL
test$Color.Markings = NULL

levels(train$Species) <- 1
levels(test$Species) <-1

levels(train$chip_outcome) <- 1:2
levels(test$chip_outcome) <- 1:2

levels(train$ml) <- 1:2
levels(test$ml) <- 1:2

levels(train$pattern) <- 1:4
levels(test$pattern) <- 1:4

train.matrix <- as.matrix(train)
mode(train.matrix) <- "numeric"
test.matrix <- as.matrix(test)
mode(test.matrix) <- "numeric"
y <- as.matrix(as.integer(outcome) -1)
param <- list("objective" = "multi:softprob", "num_class" = num.class,
              "eval_metric" = "mlogloss")
nround.cv <- 140
bst.cv <- xgb.cv(param = param, data = train.matrix, label = y, nfold = 4, nrounds = nround.cv, prediction = TRUE, verbose = FALSE)


bst <- xgboost(param = param, data = train.matrix, label = y, nrounds = 140, verbose = 1)
pred <- predict(bst, test.matrix)
pred1 <- data.frame(t(matrix(pred, nrow = num.class, ncol = length(pred)/num.class)))
names(pred1) <- c("ADOPTION","EUTHANASIA","OTHER")
pred1$ARN <- test.ARN
pred1 <- pred1[c("ARN", "ADOPTION","EUTHANASIA","OTHER")]
##plotting importance graph
model <- xgb.dump(bst, with.stats = TRUE)
names <- dimnames(train.matrix)[[2]]
importance_matrix <- xgb.importance(names, model= bst)
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix[1:23,])


write.csv(pred1, "GOG222.csv", row.names = FALSE)





