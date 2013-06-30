#load training data
trainsal <- read.csv('train.csv')

#drop FullDescription, LocationRaw, SalaryRaw because not using 
trainsal$FullDescription <- NULL
trainsal$LocationRaw <- NULLtrainsal$SalaryRaw <- NULL

#now let's make a new column for presence of management title
trainsal[, "MgmtTitle"] <- grepl("Senior|Snr|Manager|Supervisor|Deputy", trainsal$Title)

#get just top 20 towns
town.counts <- summary(trainsal$LocationNormalized)
top.towns <- names(town.counts[order(town.counts, decreasing=TRUE)][1:20])
trainsal$TopTown <- factor(trainsal$LocationNormalized, levels=top.towns)

#set NA as level
trainsal$TopTown <- addNA(trainsal$TopTown)
length(levels(trainsal$TopTown))

#split trainsal into two data frames, one for training and one for testing
set.seed(50)
trainsal$fold <- sample(1:10, nrow(trainsal), replace=TRUE)

#save training and testing sets
train <- subset(trainsal, fold != 3)
test <- subset(trainsal, fold == 3)

#build, test, eval models
model1 <- lm(SalaryNormalized ~ TopTown, data = train)
model2 <- lm(SalaryNormalized ~ TopTown + Category, data = train)
model3 <- lm(SalaryNormalized ~ Category, data = train)
model4 <- lm(SalaryNormalized ~ Category + ContractType + ContractTime, data = train)
model5 <- lm(SalaryNormalized ~ TopTown + ContractType + ContractTime, data = train)
model6 <- lm(SalaryNormalized ~ TopTown + Category + MgmtTitle, data = train)
model7 <- lm(SalaryNormalized ~ Category + ContractType + ContractTime + MgmtTitle, data = train)

test.model1 <- predict(model1, test)
test.model2 <- predict(model2, test)
test.model3 <- predict(model3, test)
test.model4 <- predict(model4, test)
test.model5 <- predict(model5, test)
test.model6 <- predict(model6, test)
test.model7 <- predict(model7, test)

mae(test.model1, test$SalaryNormalized)
mae(test.model2, test$SalaryNormalized)
mae(test.model3, test$SalaryNormalized)
mae(test.model4, test$SalaryNormalized)
mae(test.model5, test$SalaryNormalized)
mae(test.model6, test$SalaryNormalized)
mae(test.model7, test$SalaryNormalized)

#cross validate 
install.packages('DAAG')
library('DAAG')
chosenmodel <- formula(SalaryNormalized ~ TopTown + Category + MgmtTitle, data = train)testchosen <- cv.lm(df = train, form.lm = chosenmodel, m=3)

#bring in location tree file
location <- read.csv('Location_Tree2.csv')

#rename column headers
colnames(location) <- c("Country", "County", "District", "LocationNormalized")

#merge location into trainsal on LocationNormalized
trainsal1 <- merge(trainsal, location, by='LocationNormalized',all.x=T) 

#split trainsal1 into train and test again
set.seed(50)
trainsal1$fold <- sample(1:10, nrow(trainsal1), replace=TRUE)
train1 <- subset(trainsal1, fold != 3)
test1 <- subset(trainsal1, fold == 3)

#build new models with additional location data. Best model last time was model 6
# model6 <- lm(SalaryNormalized ~ TopTown + Category + MgmtTitle, data = train)
model6a <- lm(SalaryNormalized ~ County + Category + MgmtTitle, data = train1)
test.model6a <- predict(model6a, test1)
mae(test.model6a, test1$SalaryNormalized)
summary(model6a)

#load in actual test data
actualtest <- read.csv('test.csv')

#merge location into actual test
actualtest1 <- merge(actualtest, location, by='LocationNormalized',all.x=T) 

#add mgmttitle column to actualtest1
actualtest1[, "MgmtTitle"] <- grepl("Senior|Snr|Manager|Supervisor|Deputy", actualtest1$Title)

#build final model 
finalmodel <- lm(SalaryNormalized ~ County + Category + MgmtTitle, data = trainsal1)
predictions <- predict(finalmodel, actualtest1)

#set actualtest1 Category column to characters, in order to set new value as NA
actualtest1$Category <- as.character(actualtest1$Category)
actualtest1$Category[actualtest1$Category == "Part time Jobs"] = NA

#predict again
predictions <- predict(finalmodel, actualtest1)

#submit
submission <- data.frame(Id=actualtest1$Id, Salary=predictions)

View(submission)

#still seeing NAs. Until I figure out why, I'm subbing NAs for the mean salary
mean(trainsal1$SalaryNormalized)
submission[is.na(submission)] <- 28171
write.csv(submission, "cw_kaggle_submission.csv", row.names=FALSE)
