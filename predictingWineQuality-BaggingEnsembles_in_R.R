# print('An End to End Applied Machine Learning Recipe in R: 
#        Wine Quality Identification using Bagging Ensembles')

# Such As :-- 
# Bagging
# 01. Random Forest : cforest - Binary & MultiClass Classification
# 02. Random Forest : parRF   - Binary & MultiClass Classification
# 04. Extra Tree : extraTrees - Binary & MultiClass Classification 

# print('Available Algorithms/Models in caret package: 
#      http://topepo.github.io/caret/available-models.html')
# here Algorithms such as LMT should be used as examples

# Load necessary Libraries
# load library
library(DBI)
library(RMySQL)
library(corrgram)
library(caret)

# For MAC OSX and Unix like System
# library(doMC)
# registerDoMC(cores = 4)

# set working directory where CSV is located
getwd()
setwd("/Users/nilimesh//Desktop/Data Science Products/003 - Tabular Data Analytics in Python - Wine Quality Dataset/Program 08 - Predicting Wine Quality - BBagging Ensembles in R")
getwd()

# Load the DataSets: IRIS Flower Data
dataSet <- read.csv("winequality.white.data.csv", header = FALSE, sep = ';', skip = 1)
colnames(dataSet) <- c('fixed_acidity', 'volatile_acidity', 'citric_acid', 'residual_sugar', 
                       'chlorides', 'free_sulfur_dioxide', 'total_sulfur_dioxide', 'density', 
                       'pH','sulphates', 'alcohol', 'quality')

# Print top 10 rows in the dataSet
head(dataSet, 10)
# Print last 10 rows in the dataSet
tail(dataSet, 10)
# Dimention of Dataset
dim(dataSet)
# Check Data types of each column
table(unlist(lapply(dataSet, class)))
#Check column names
colnames(dataSet)
# Check Data types of individual column
data.class(dataSet$fixed_acidity)
data.class(dataSet$volatile_acidity)
data.class(dataSet$citric_acid)
data.class(dataSet$residual_sugar)
data.class(dataSet$chlorides)
data.class(dataSet$free_sulfur_dioxide)
data.class(dataSet$total_sulfur_dioxide)
data.class(dataSet$density)
data.class(dataSet$pH)
data.class(dataSet$sulphates)
data.class(dataSet$alcohol)
data.class(dataSet$quality)

# Change the Data type of variables
# from numeric to character and vice versa
dataSet$residual_sugar = as.numeric(dataSet$residual_sugar)
dataSet$quality = as.character(dataSet$quality)

data.class(dataSet$quality)

## Connect to a MySQL Database 
# create a MySQL driver 
m = dbDriver("MySQL")
myHost <- 'localhost' #'127.0.0.1'
myUsername = 'root'
myDbname = 'datasciencerecipes'
myPort = 3306
myPassword = 'root888'
con = dbConnect(m, user= myUsername, host= myHost, password= myPassword, dbname= myDbname, port= myPort)

if(dbIsValid(con)) {
  print('MySQL Connection is Successful')
} else {print('MySQL Connection is Unsuccessful')}

# Export DataFrame to a MySQL table 
response <- dbWriteTable(conn = con, name = 'whitewinequalitydata', value = dataSet, 
                         row.names = FALSE, overwrite = TRUE)
if(response) {print('Data export to MySQL is successful')
} else {print('Data export to MySQL is unsuccessful')}

## Write a query here and execute it to retrive data from MySQL Database
sql = 'SELECT * 
FROM whitewinequalitydata;'
result = dbSendQuery(conn = con, statement = sql)
dataset <- dbFetch(res = result, n = -1)
dbClearResult(result)
dbDisconnect(conn = con)

## Check dataset that retrived from MySQL database
# Print top 10 rows in the dataSet
head(dataset, 10)
# Print last 10 rows in the dataSet
tail(dataset, 10)
# Dimention of Dataset
dim(dataset)
# Check Data types of each column
table(unlist(lapply(dataset, class)))
#Check column names
colnames(dataset)

# Change the Data type of Class variables to "character"
dataset$quality = as.character(dataset$quality)
data.class(dataset$quality)

## Exploring or Summarising dataset with descriptive statistics
# Find out if there is missing value
rowSums(is.na(dataset))
colSums(is.na(dataset))
# Missing data treatment if exists
#dataset[dataset$columnName=="& ","columnName"] <- NA 
#drop columns
#dataset <- within(dataset, rm(columnName))

# Summary of dataset
#lapply - When you want to apply a function to each element of a list in turn and get a list back.
lapply(dataset[1:11], FUN = sum)
lapply(dataset[1:11], FUN = mean)
lapply(dataset[1:11], FUN = median)
lapply(dataset[1:11], FUN = min)
lapply(dataset[1:11], FUN = max)
lapply(dataset[1:11], FUN = length)

#sapply - When you want to apply a function to each element of a list in turn, 
#but you want a vector back, rather than a list.
sapply(dataset[1:11], FUN = sum)
sapply(dataset[1:11], FUN = mean)
sapply(dataset[1:11], FUN = median)
sapply(dataset[1:11], FUN = min)
sapply(dataset[1:11], FUN = max)
sapply(dataset[1:11], FUN = length)

# Using Aggregate FUNCTION
aggregate(dataset$fixed_acidity, list(dataset$quality), summary)
aggregate(dataset$volatile_acidity, list(dataset$quality), summary)
aggregate(dataset$citric_acid, list(dataset$quality), summary)
aggregate(dataset$residual_sugar, list(dataset$quality), summary)
aggregate(dataset$chlorides, list(dataset$quality), summary)
aggregate(dataset$free_sulfur_dioxide, list(dataset$quality), summary)
aggregate(dataset$total_sulfur_dioxide, list(dataset$quality), summary)
aggregate(dataset$density, list(dataset$quality), summary)
aggregate(dataset$pH, list(dataset$quality), summary)
aggregate(dataset$sulphates, list(dataset$quality), summary)
aggregate(dataset$alcohol, list(dataset$quality), summary)

# Using "by"
by(dataset[1:11], dataset[12], FUN = summary)
by(dataset[1:11], dataset$quality, FUN = summary)

## Visualising DataSet
# Print Column Names
colnames(dataset)
# Print Data Types of each column
for(i in 1:length(dataset)) {
  print(data.class(dataset[,i]))
}

# Histogram
par(mfrow=c(3,3))

x <- dataset$fixed_acidity
hist(x,  xlab = "fixed_acidity", ylab = "Count", main = "")
x <- dataset$volatile_acidity
hist(x,  xlab = "volatile_acidity", ylab = "Count", main = "")
x <- dataset$citric_acid
hist(x,  xlab = "citric_acid", ylab = "Count", main = "")

x <- dataset$residual_sugar
hist(x,  xlab = "residual_sugar", ylab = "Count", main = "")
x <- dataset$chlorides
hist(x,  xlab = "chlorides", ylab = "Count", main = "")
x <- dataset$free_sulfur_dioxide
hist(x,  xlab = "free_sulfur_dioxide", ylab = "Count", main = "")

x <- dataset$total_sulfur_dioxide
hist(x,  xlab = "total_sulfur_dioxide", ylab = "Count", main = "")
x <- dataset$density
hist(x,  xlab = "density", ylab = "Count", main = "")
x <- dataset$pH
hist(x,  xlab = "pH", ylab = "Count", main = "")


# Histogram with Density graph
par(mfrow=c(3,3))

x <- dataset$fixed_acidity
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$volatile_acidity
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$citric_acid
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$residual_sugar
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$chlorides
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$free_sulfur_dioxide
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$total_sulfur_dioxide
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$density
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

x <- dataset$pH
h <- hist(x,  xlab = "", ylab = "Count", main = "")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=2)

# Barplot of categorical data
par(mfrow=c(2,2))
barplot(table(dataset$quality), ylab = "Count", 
        col=c("darkblue","red", "green"))
barplot(prop.table(table(dataset$quality)), ylab = "Proportion", 
        col=c("darkblue","red", "green"))
barplot(table(dataset$quality), xlab = "Count", horiz = TRUE, 
        col=c("darkblue","red", "green"))
barplot(prop.table(table(dataset$quality)), xlab = "Proportion", horiz = TRUE, 
        col=c("darkblue","red", "green"))

# Box Plot of Numerical Data
par(mfrow=c(3,3))
boxplot(dataset$fixed_acidity, ylab = "")
boxplot(dataset$volatile_acidity, ylab = "")
boxplot(dataset$citric_acid, ylab = "")
boxplot(dataset$residual_sugar, ylab = "")
boxplot(dataset$chlorides, ylab = "")
boxplot(dataset$free_sulfur_dioxide, ylab = "")
boxplot(dataset$total_sulfur_dioxide, ylab = "")
boxplot(dataset$density, ylab = "")
boxplot(dataset$pH, ylab = "")

# Scatter Plots
par(mfrow=c(2,2))
plot(dataset$fixed_acidity, pch = 20)
plot(dataset$chlorides, pch = 20)
plot(dataset$pH, pch = 20)
plot(dataset$alcohol, pch = 20)

par(mfrow=c(2,2))
plot(dataset$fixed_acidity, dataset$alcohol, pch = 20)
plot(dataset$residual_sugar, dataset$alcohol, pch = 20)
plot(dataset$citric_acid, dataset$alcohol, pch = 20)
plot(dataset$density, dataset$alcohol, pch = 20)

# Corelation Diagram using "corrgram" package
x <- dataset[1:11]

#x is a data frame with one observation per row.
corrgram(x)

#order=TRUE will cause the variables to be ordered using principal component analysis of the correlation matrix.
corrgram(x, order = TRUE)

# lower.panel= and upper.panel= to choose different options below and above the main diagonal respectively. 

# (the filled portion of the pie indicates the magnitude of the correlation)
# lower.panel=  
corrgram(x, order = TRUE, lower.panel = panel.pie, upper.panel = NULL)
corrgram(x, order = TRUE, lower.panel = panel.shade, upper.panel = NULL)
corrgram(x, order = TRUE, lower.panel = panel.ellipse, upper.panel = NULL)
corrgram(x, order = TRUE, lower.panel = panel.pts, upper.panel = NULL)

#off diagonal panels
# lower.panel= & upper.panel=
corrgram(x, order = TRUE, lower.panel = panel.pie, upper.panel = panel.pts)
corrgram(x, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie)
corrgram(x, order = TRUE, lower.panel = panel.ellipse, upper.panel = panel.shade)
corrgram(x, order = TRUE, lower.panel = panel.pts, upper.panel = panel.pie)

# upper.panel=
corrgram(x, order = TRUE, lower.panel = NULL, upper.panel = panel.pts)
corrgram(x, order = TRUE, lower.panel = NULL, upper.panel = panel.pie)
corrgram(x, order = TRUE, lower.panel = NULL, upper.panel = panel.shade)
corrgram(x, order = TRUE, lower.panel = NULL, upper.panel = panel.ellipse)

#text.panel= and diag.panel= refer to the main diagnonal. 
corrgram(x, order = TRUE, lower.panel = panel.pie, upper.panel = panel.pts,
         text.panel=panel.txt)
corrgram(x, order = TRUE, lower.panel = panel.pie, upper.panel = panel.pts,
         text.panel=panel.txt, diag.panel=panel.minmax,
         main="Correlation Diagram")

# visualise a correlation matrix for numeric variables
# Scatterplot Matrices from the glus Package 
library(gclus)
x <- dataset[1:11]
dta.r <- abs(cor(x)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(x, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

# Pie Chart
par(mfrow=c(1,1))
x <- table(dataset$quality)
lbls <- paste(names(x), "\nTotal Count:", x, sep="")
pie(x, labels = lbls, main="Pie Chart of Different Classes", col = c("red","blue","green"))

## ---------------------------------------------------
## Pre-Processing of DataSet i.e. train : test split
## ---------------------------------------------------
train_test_index <- createDataPartition(dataset$quality, p=0.67, list=FALSE)
training_dataset <- dataset[train_test_index,]
testing_dataset <- dataset[-train_test_index,]

dim(training_dataset)
dim(testing_dataset)

## Evaluating Algorithm i.e. training, testing and evaluation
# Check available ML Algorithms
names(getModelInfo())

## Turn Off warnings
options( warn = -1 )

## Turn On warnings
#options( warn = 0 )

##############################################################
# cross Validation and control parameter setup
##############################################################
control <- trainControl(method="repeatedcv", # repeatedcv / adaptive_cv
                        number=3, repeats = 3, verbose = TRUE, search = "grid",
                        allowParallel = TRUE)

###################################################################
###################################################################
# Machine Learning Algorithm and parameter tuning 
# 1. Without paramet tuning or using default

## There three ways of parameter tuning
# 2. Using Data Pre-Processing: 
# caret method -> preProcess 
# default value is NULL
# other value ["BoxCox", "YeoJohnson", "expoTrans", "center", 
#              "scale", "range", "knnImpute", "bagImpute", 
#              "medianImpute", "pca", "ica" and "spatialSign"]

# 3. Using Automatic Grid
# caret method -> tuneLength [Note: it takes INTEGER Val]
# Example: tuneLength = 3

# 4. Using Manual Grid
# caret method -> tuneGrid [Note: grid needs to be defined manually]
# Example: grid <- expand.grid(size=c(5,10), k=c(3,4,5)) [parameters of LVQ]
#          tuneGrid = grid   

###################################################################
###################################################################

# -----------------------------------------------------------------------------
## 1. Training - without explicit parameter tuning / using default
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Bagging
# 01. Random Forest : cforest - Binary & MultiClass Classification
# 02. Random Forest : parRF   - Binary & MultiClass Classification
# 04. Extra Tree : extraTrees - Binary & MultiClass Classification 
# -----------------------------------------------------------------------------

# 01. Random Forest : cforest - Binary & MultiClass Classification
fit.cForest_1 <- caret::train(quality~., data=training_dataset, method="cforest", 
                              metric="Accuracy", trControl=control)
print(fit.cForest_1)
#plot(fit.cForest_1)

# 02. Random Forest : parRF   - Binary & MultiClass Classification
fit.parRF_1 <- caret::train(quality~., data=training_dataset, method="parRF", 
                            metric="Accuracy", trControl=control)
print(fit.parRF_1)
#plot(fit.parRF_1)

# 04. Randon Forest : RRF - Binary & MultiClass Classification 
fit.rRF_1 <- caret::train(quality~., data=training_dataset, method="RRF", 
                          metric="Accuracy", trControl=control)
print(fit.rRF_1)
#plot(fit.rRF_1)

# -----------------------------------------------------------------------------
## 2. Training - with explicit parameter tuning using preProcess method
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Bagging
# 01. Random Forest : cforest - Binary & MultiClass Classification
# 02. Random Forest : parRF   - Binary & MultiClass Classification
# 04. Extra Tree : extraTrees - Binary & MultiClass Classification 
# -----------------------------------------------------------------------------

# 01. Random Forest : cforest - Binary & MultiClass Classification
fit.cForest_2 <- caret::train(quality~., data=training_dataset, method="cforest", 
                              metric="Accuracy", trControl=control,
                              preProcess = c('center', 'scale','pca'))
print(fit.cForest_2)
#plot(fit.cForest_2)

# 02. Random Forest : parRF   - Binary & MultiClass Classification
fit.parRF_2 <- caret::train(quality~., data=training_dataset, method="parRF", 
                            metric="Accuracy", trControl=control,
                            preProcess = c('center', 'scale','pca'))
print(fit.parRF_2)
#plot(fit.parRF_2)

# 04. Randon Forest : RRF - Binary & MultiClass Classification 
fit.rRF_2 <- caret::train(quality~., data=training_dataset, method="RRF", 
                          metric="Accuracy", trControl=control,
                          preProcess = c('center', 'scale','pca'))
print(fit.rRF_2)
#plot(fit.rRF_2)

# -----------------------------------------------------------------------------
## 3. Training - with explicit parameter tuning using preProcess method 
## & Automatic Grid i.e. tuneLength
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Bagging
# 01. Random Forest : cforest - Binary & MultiClass Classification
# 02. Random Forest : parRF   - Binary & MultiClass Classification
# 04. Extra Tree : extraTrees - Binary & MultiClass Classification 
# -----------------------------------------------------------------------------

# 01. Random Forest : cforest - Binary & MultiClass Classification
fit.cForest_3 <- caret::train(quality~., data=training_dataset, method="cforest", 
                              metric="Accuracy", trControl=control,
                              preProcess = c('center', 'scale','pca'),
                              tuneLength = 3)
print(fit.cForest_3)
#plot(fit.cForest_3)

# 02. Random Forest : parRF   - Binary & MultiClass Classification
fit.parRF_3 <- caret::train(quality~., data=training_dataset, method="parRF", 
                            metric="Accuracy", trControl=control,
                            preProcess = c('center', 'scale','pca'),
                            tuneLength = 3)
print(fit.parRF_3)
#plot(fit.parRF_3)

# 04. Randon Forest : RRF - Binary & MultiClass Classification 
fit.rRF_3 <- caret::train(quality~., data=training_dataset, method="RRF", 
                          metric="Accuracy", trControl=control,
                          preProcess = c('center', 'scale','pca'),
                          tuneLength = 3)
print(fit.rRF_3)
#plot(fit.rRF_3)

# collect resampling statistics of ALL trained models
results <- resamples(list(CF_1        = fit.cForest_1,
                          parRF_1     = fit.parRF_1,
                          RRF_1       = fit.rRF_1,
                          
                          CF_2        = fit.cForest_2,
                          parRF_2     = fit.parRF_2,
                          RRF_2       = fit.rRF_2,
                          
                          CF_3        = fit.cForest_3,
                          parRF_3     = fit.parRF_3,
                          RRF_3       = fit.rRF_3
))

# Summarize the fitted models
summary(results)

# Plot and rank the fitted models
# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

# density plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)

# pair-wise scatterplots of predictions to compare models
splom(results)

# Test skill of the BEST trained model on validation/testing dataset
predictions_parRF_1 <- predict(fit.parRF_1, newdata=testing_dataset)

# Evaluate the BEST trained model and print results
res_parRF_1  <- caret::confusionMatrix(predictions_parRF_1, testing_dataset$quality)

print("Results from the BEST trained model ... ...\n"); 
print(res_parRF_1) 
print(round(res_parRF_1$overall, digits = 3))

## Save the BEST trained model to disk
final_model <- fit.parRF_1;        saveRDS(final_model, "./final_model_parRF_1.rds")

# Connecting a MySQL Database
m = dbDriver("MySQL")
myHost <- 'localhost' #'127.0.0.1'
myUsername = 'root'
myDbname = 'datasciencerecipes'
myPort = 3306
myPassword = 'root888'
con = dbConnect(m, user= myUsername, host= myHost, password= myPassword, dbname= myDbname, port= myPort)

if(dbIsValid(con)) {
  print('MySQL Connection is Successful')
} else {print('MySQL Connection is Unsuccessful')}

## Write a query here and execute it to retrive data from MySQL Database
sql = 'SELECT *
FROM whitewinequalitydata;'
result = dbSendQuery(conn = con, statement = sql)
dataset <- dbFetch(res = result, n = -1)
dbClearResult(result)
dim(dataset)

## Load the trained model from disk
trained_model <- readRDS("./final_model_parRF_1.rds")

# make a predictions on "new data" using the final model
cols <- c(1:11)
predicted_Class <- predict(trained_model, dataset[cols])
predicted_ClassProb <- predict(trained_model, dataset[cols], 
                               type = "prob")
dim(predicted_ClassProb)

# Save result in a CSV file and/ or MySQL Table
result <- data.frame(predicted_Class)
dim(result)
dim(dataset)

# merge prediction with dataset 
finalResult <- cbind(dataset, result, predicted_ClassProb)
dim(finalResult)

# in CSV file
cols <- c(1:20)
write.csv(finalResult[cols], file = "finalResult.csv", row.names = FALSE)

# in MySQL Table
dbWriteTable(conn = con, name = 'whitewinequalityresult', value = finalResult[cols], 
             row.names = FALSE, overwrite = TRUE)
dbDisconnect(conn = con)

# KAPPA Interpretation :
# In plain English, it measures how much better the classier is comparing with guessing 
# with the target distribution.
# Poor agreement        = 0.20 or less
# Fair agreement        = 0.20 to 0.40
# Moderate agreement    = 0.40 to 0.60
# Good agreement        = 0.60 to 0.80
# Very good agreement   = 0.80 to 1.00

