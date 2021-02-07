#Load libraries
library(sqldf)
library(writexl)
library(ggcorrplot)
library(corrplot)
library(eeptools)
library(formattable)



#Read in dataset
setwd("Directory")
credit_raw <- read.csv(file = "crx.csv", header = FALSE)



# --- Data Cleaning --- #

#Add column names to dataset
colnames(credit_raw) <- c("Male","Age","Debt","Married","BankCustomer","EducationLevel","Ethnicity","YearsEmployed","PriorDefault","Employed","CreditScore","DriversLicense","Citizen","ZipCode","Income","Approved")

#Summarize dataset
dim(credit_raw)
colnames(credit_raw)
summary(credit_raw)
str(credit_raw)
head(credit_raw)

#Drop extraneous columns (multi-level columns) and group categorical/quantitative variables together
credit <- sqldf('select Age,
                        Debt,
                        YearsEmployed,
                        CreditScore,
                        Income,
                        Male,
                        PriorDefault,
                        Employed,
                        DriversLicense,
                        Approved

                from    credit_raw')

#Summarize dataset
dim(credit)
colnames(credit)
summary(credit)
str(credit)
head(credit)

#Check columns for null values
sapply(credit, function(x) sum(is.na (x)))

#Check columns for value ?
sapply(credit, function(x) sum(x == '?'))

#Convert Age column to numeric
credit$Age <- as.numeric(as.character(credit$Age))
sapply(credit, class)
sapply(credit, function(x) sum(is.na (x)))
summary(credit)

#Replace all null values in Age column with mean of the column
credit$Age[is.na(credit$Age)] <- mean(credit$Age, na.rm = TRUE)
sapply(credit, function(x) sum(is.na (x)))
summary(credit)

#Replace all ? values in Male column with mode of the column
credit$Male[credit$Male == "?"] <- "b"
sapply(credit, function(x) sum(x == '?'))
summary(credit)

#For Approved Factor columns, change to Y and N coding
#First need to add 2 additional levels to the factor, then convert
levels(credit$Approved) <- c(levels(credit$Approved), "A","D")
credit$Approved[credit$Approved == '-'] <- "A"
credit$Approved[credit$Approved == "+"] <- "D"
summary(credit)

#Drop unused levels from dataset
credit <- droplevels(credit)
summary(credit)
str(credit)

#Write out dataset for Tableau analysis (if needed) and Decision Tree
write.csv(credit, file = "credit_dataset_final_ks.csv")


# --- Check Correlations --- #

numeric_cols <- credit[, sapply(credit, is.numeric)]
corr <- cor(numeric_cols)
round(corr,2)
#ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "black")
#ggcorrplot(corr, type = "lower", outline.col = "black")
corrplot(corr, type="lower", tl.col = "black", tl.srt=45)



# --- Create Logistic Regression Model - All Variables --- #

#Separate data into test and training sets
set.seed(2019)
trainIndex <- sample(nrow(credit), 0.7*nrow(credit))
trainset <- credit[trainIndex,]
testset <- credit[-trainIndex,]
dim(trainset)
dim(testset)
summary(trainset$Approved)
summary(testset$Approved)

#Model 1: All variables
glm_train <- glm(Approved ~ ., family = binomial, data = trainset)
summary(glm_train)
glm_test.probs = predict(glm_train, testset, type = "response")
glm_test.probs[1:10]
glm_test.pred = rep("A", nrow(testset))
glm_test.pred[glm_test.probs > 0.5] = "D"
table(glm_test.pred, testset$Approved)
a1 <- mean(glm_test.pred == testset$Approved)
m1 <- 1-mean(glm_test.pred == testset$Approved)
tpr1 <- 87/(87+26)
fpr1 <- 7/(7+88)



# --- Backward Selection ---#

#Model 2: Remove least significant variable
glm_train2 <- glm(Approved ~ . -Age, data = trainset, family = binomial)
summary(glm_train2)
glm_test.probs2 = predict(glm_train2, testset, type = "response")
glm_test.probs2[1:10]
glm_test.pred2 = rep("A", nrow(testset))
glm_test.pred2[glm_test.probs2 > 0.5] = "D"
table(glm_test.pred2, testset$Approved)
a2 <- mean(glm_test.pred2 == testset$Approved)
m2 <- 1-mean(glm_test.pred2 == testset$Approved)
tpr2 <- 87/(87+26)
fpr2 <- 7/(7+88)

#Model 3: Remove 2 least significant variables
glm_train3 <- glm(Approved ~ . -Age -DriversLicense, data = trainset, family = binomial)
summary(glm_train3)
glm_test.probs3 = predict(glm_train3, testset, type = "response")
glm_test.probs3[1:10]
glm_test.pred3 = rep("A", nrow(testset))
glm_test.pred3[glm_test.probs3 > 0.5] = "D"
table(glm_test.pred3, testset$Approved)
a3 <- mean(glm_test.pred3 == testset$Approved)
m3 <- 1-mean(glm_test.pred3 == testset$Approved)
tpr3 <- 86/(86+27)
fpr3 <- 6/(6+89)

#Model 4: Remove 3 least significant variables
glm_train4 <- glm(Approved ~ . -Age -DriversLicense -Debt, data = trainset, family = binomial)
summary(glm_train4)
glm_test.probs4 = predict(glm_train4, testset, type = "response")
glm_test.probs4[1:10]
glm_test.pred4 = rep("A", nrow(testset))
glm_test.pred4[glm_test.probs4 > 0.5] = "D"
table(glm_test.pred4, testset$Approved)
a4 <- mean(glm_test.pred4 == testset$Approved)
m4 <- 1-mean(glm_test.pred4 == testset$Approved)
tpr4 <- 86/(86+27)
fpr4 <- 7/(7+88)

#Model 5: Remove 4 least significant variables
glm_train5 <- glm(Approved ~ . -Age -DriversLicense -Debt -Employed, data = trainset, family = binomial)
summary(glm_train5)
glm_test.probs5 = predict(glm_train5, testset, type = "response")
glm_test.probs5[1:10]
glm_test.pred5 = rep("A", nrow(testset))
glm_test.pred5[glm_test.probs5 > 0.5] = "D"
table(glm_test.pred5, testset$Approved)
a5 <- mean(glm_test.pred5 == testset$Approved)
m5 <- 1-mean(glm_test.pred5 == testset$Approved)
tpr5 <- 86/(86+27)
fpr5 <- 5/(5+90)

#Model 6: Only significant variables
glm_train6 <- glm(Approved ~ . -Age -DriversLicense -Debt -Employed -Male, data = trainset, family = binomial)
summary(glm_train6)
glm_test.probs6 = predict(glm_train6, testset, type = "response")
glm_test.probs6[1:10]
glm_test.pred6 = rep("A", nrow(testset))
glm_test.pred6[glm_test.probs6 > 0.5] = "D"
table(glm_test.pred6, testset$Approved)
a6 <- mean(glm_test.pred6 == testset$Approved)
m6 <- 1-mean(glm_test.pred6 == testset$Approved)
tpr6 <- 86/(86+27)
fpr6 <- 5/(5+90)


#Model 7: Significant variables + highly correlated variables
glm_train7 <- glm(Approved ~ PriorDefault + CreditScore + Income + YearsEmployed + Age + Debt, data = trainset, family = binomial)
summary(glm_train7)
glm_test.probs7 = predict(glm_train7, testset, type = "response")
glm_test.probs7[1:10]
glm_test.pred7 = rep("A", nrow(testset))
glm_test.pred7[glm_test.probs7 > 0.5] = "D"
table(glm_test.pred7, testset$Approved)
a7 <- mean(glm_test.pred7 == testset$Approved)
m7 <- 1-mean(glm_test.pred7 == testset$Approved)
tpr7 <- 86/(86+27)
fpr7 <- 5/(5+90)



# --- Compare Models --- #
CompareModels <- data.frame( "Models" = c("Model 1","Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"),
                   "TPR" = c(tpr1, tpr2, tpr3, tpr4, tpr5, tpr6, tpr7),
                   "FPR" = c(fpr1, fpr2, fpr3, fpr4, fpr5, fpr6, fpr7),
                   "Error_Rate" = c(m1, m2, m3, m4, m5, m6, m7),
                   "Accuracy" = c(a1, a2, a3, a4, a5, a6, a7))
head(CompareModels)
CompareModels$TPR <- percent(CompareModels$TPR)
CompareModels$FPR <- percent(CompareModels$FPR)
CompareModels$Error_Rate <- percent(CompareModels$Error_Rate)
CompareModels$Accuracy <- percent(CompareModels$Accuracy)
CompareModels



# --- For Presentation Deck --- #

#Model 1 = Model 1 from Above
#Model 2 = Model 3 from Above
#Model 3 = Model 6 from Above
#Model 4 = Model 7 from Above

Present <- data.frame( "Models" = c("Model 1","Model 2", "Model 3", "Model 4"),
                            "TPR" = c(tpr1, tpr3, tpr6, tpr7),
                            "FPR" = c(fpr1, fpr3, fpr6, fpr7),
                            "Error_Rate" = c(m1, m3, m6, m7),
                            "Accuracy" = c(a1, a3, a6, a7))
head(Present)
Present$TPR <- percent(Present$TPR)
Present$FPR <- percent(Present$FPR)
Present$Error_Rate <- percent(Present$Error_Rate)
Present$Accuracy <- percent(Present$Accuracy)
Present



# --- Create ROC Curves for 4 Models Above --- #
# Model 1
m1 <- glm(Approved ~ ., data = trainset, family = binomial)
summary(m1)

pred <- predict(m1, testset, type = "response")
head(pred)

head(testset['Approved'])

df <- data.frame (score = pred, true.class = testset[,'Approved'])
head(df)
tail(df)

n <- nrow(testset)
n

df <- df[order(-df$score),]
head(df)
rownames(df) <- NULL
head(df)

P = sum(df$true.class == 'D')
P
N = sum(df$true.class == 'A')
N

TPR = c(0,vector(mode="numeric", length=n))
FPR = c(0,vector(mode="numeric", length=n))

head(df)

for(k in 1:n){
  if(df[k,"true.class"] == 'D'){
    TPR[k+1] = TPR[k] + 1/P
    FPR[k+1] = FPR[k]
  } else{
    TPR[k+1] = TPR[k]
    FPR[k+1] = FPR[k] + 1/N
  }
}

# Color scheme: red
color = c("red","red","red")

# Plot the ROC curve
plot(FPR, TPR, main=paste0("ROC Curves"," (n = ", n, ")"), pch=20, col=color[(2+c(-1,df$true.class))], cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
lines(FPR,TPR)


# Model 2
m2 <- glm(Approved ~ . -Age -DriversLicense, data = trainset, family = binomial)    
summary(m2)

pred <- predict(m2, testset, type = "response")
head(pred)

head(testset['Approved'])

df <- data.frame (score = pred, true.class = testset[,'Approved'])
head(df)
tail(df)

n <- nrow(testset)
n

df <- df[order(-df$score),]
head(df)
rownames(df) <- NULL
head(df)

P = sum(df$true.class == 'D')
P
N = sum(df$true.class == 'A')
N

TPR = c(0,vector(mode="numeric", length=n))
FPR = c(0,vector(mode="numeric", length=n))

head(df)

for(k in 1:n){
  if(df[k,"true.class"] == 'D'){
    TPR[k+1] = TPR[k] + 1/P
    FPR[k+1] = FPR[k]
  } else{
    TPR[k+1] = TPR[k]
    FPR[k+1] = FPR[k] + 1/N
  }
}

# Color scheme: blue
color = c("blue","blue","blue")

# Plot the ROC curve
points(FPR, TPR, main=paste0("ROC Curves"," (n = ", n, ")"), pch=20, col=color[(2+c(-1,df$true.class))], cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
lines(FPR,TPR)

# Model 3
m3 <- glm_train2 <- glm(Approved ~ PriorDefault + Income + CreditScore + YearsEmployed, data = trainset, family = binomial)
summary(m3)

pred <- predict(m3, testset, type = "response")
head(pred)

head(testset['Approved'])

df <- data.frame (score = pred, true.class = testset[,'Approved'])
head(df)
tail(df)

n <- nrow(testset)
n

df <- df[order(-df$score),]
head(df)
rownames(df) <- NULL
head(df)

P = sum(df$true.class == 'D')
P
N = sum(df$true.class == 'A')
N

TPR = c(0,vector(mode="numeric", length=n))
FPR = c(0,vector(mode="numeric", length=n))

head(df)

for(k in 1:n){
  if(df[k,"true.class"] == 'D'){
    TPR[k+1] = TPR[k] + 1/P
    FPR[k+1] = FPR[k]
  } else{
    TPR[k+1] = TPR[k]
    FPR[k+1] = FPR[k] + 1/N
  }
}

# Color scheme: green
color = c("green","green","green")

# Plot the ROC curve
points(FPR, TPR, main=paste0("ROC Curves"," (n = ", n, ")"), pch=20, col=color[(2+c(-1,df$true.class))], cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
lines(FPR,TPR)

# Model 4
m4 <- glm_train2 <- glm(Approved ~ PriorDefault + Income + CreditScore + YearsEmployed + Age + Debt, data = trainset, family = binomial)
summary(m4)

pred <- predict(m4, testset, type = "response")
head(pred)

head(testset['Approved'])

df <- data.frame (score = pred, true.class = testset[,'Approved'])
head(df)
tail(df)

n <- nrow(testset)
n

df <- df[order(-df$score),]
head(df)
rownames(df) <- NULL
head(df)

P = sum(df$true.class == 'D')
P
N = sum(df$true.class == 'A')
N

TPR = c(0,vector(mode="numeric", length=n))
FPR = c(0,vector(mode="numeric", length=n))

head(df)

for(k in 1:n){
  if(df[k,"true.class"] == 'D'){
    TPR[k+1] = TPR[k] + 1/P
    FPR[k+1] = FPR[k]
  } else{
    TPR[k+1] = TPR[k]
    FPR[k+1] = FPR[k] + 1/N
  }
}

# Color scheme: purple
color = c("purple","purple","purple")

# Plot the ROC curve
points(FPR, TPR, main=paste0("ROC Curves"," (n = ", n, ")"), pch=20, col=color[(2+c(-1,df$true.class))], cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
lines(FPR,TPR)
legend("bottomright", c("Model 1", "Model 2", "Model 3", "Model 4"), lty = 1, lwd=3, col = c("red", "blue", "green","purple"), bty="n", inset=c(0,0))



# --- Create Decision Tree --- #

setwd("C:\\Users\\Ken\\Desktop")
credit <- read.csv(file = "credit_dataset_final_ks.csv", header = T)

library("rpart")
treeAnalysis<-rpart(Approved ~ PriorDefault + Income + CreditScore + Debt, data=credit)
treeAnalysis
install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(treeAnalysis)
