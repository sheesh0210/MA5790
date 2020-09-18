
#######################
# Heart Failure 
#######################
# age: age of the patient (years)
# anaemia: decrease of red blood cells or hemoglobin (boolean)
# high blood pressure: if the patient has hypertension (boolean)
# creatinine phosphokinase (CPK): level of the CPK enzyme in the blood (mcg/L)
# diabetes: if the patient has diabetes (boolean)
# ejection fraction: percentage of blood leaving the heart at each contraction (percentage)
# platelets: platelets in the blood (kiloplatelets/mL)
# sex: woman or man (binary)
# serum creatinine: level of serum creatinine in the blood (mg/dL)
# serum sodium: level of serum sodium in the blood (mEq/L)
# smoking: if the patient smokes or not (boolean)
# time: follow-up period (days)
# [target] death event: if the patient deceased during the follow-up period (boolean)


#####################################
# Uploading Data/Summary Information
#####################################
heart_data <- read.csv('~/Desktop/Grad School/MA 5790/Final Project/HeartFailure/heart.csv')
str(heart_data) # 299 rows, 13 columns
head(heart_data)

binary <- heart_data[, c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking")]; binary
quantitative <- heart_data[, c("age", "creatinine_phosphokinase", "ejection_fraction", 
                               "platelets", "serum_creatinine", "serum_sodium", "time")]

####################################
# 1. Basic Variable Distributions
####################################

# Non-binary attributes - Checking for outliers
par(mfrow=c(3,3))
boxplot(heart_data$age, main = 'Age', ylab = 'years')
boxplot(heart_data$creatinine_phosphokinase, main = 'creatinine_phosphokinase', ylab = 'mcg/L')
boxplot(heart_data$ejection_fraction, main = 'ejection fraction', ylab = 'percentage')
boxplot(heart_data$platelets, main = 'platelets', ylab = 'kiloplatelets/mL')
boxplot(heart_data$serum_creatinine, main = 'serum_creatinine', ylab = 'mg/dL')
boxplot(heart_data$serum_sodium, main = 'serum_sodium', ylab = 'mEq/L')
boxplot(heart_data$time, main = 'time', ylab = 'days')

# Non-binary attribures - Checking for skew 
par(mfrow=c(3,3))
hist(heart_data$age, main = 'Age', ylab = 'years')
hist(heart_data$creatinine_phosphokinase, main = 'creatinine_phosphokinase', ylab = 'mcg/L')
hist(heart_data$ejection_fraction, main = 'ejection fraction', ylab = 'percentage')
hist(heart_data$platelets, main = 'platelets', ylab = 'kiloplatelets/mL')
hist(heart_data$serum_creatinine, main = 'serum_creatinine', ylab = 'mg/dL')
hist(heart_data$serum_sodium, main = 'serum_sodium', ylab = 'mEq/L')
hist(heart_data$time, main = 'time', ylab = 'days')

# Find Skewness For all 7 Quantitative Factors 
skewValues <- apply(quantitative, 2, skewness) #2 means by column 
typeof(skewValues)
data.frame(skewValues)

# Determine which transformations are appropriate 
trans <- preProcess(quantitative, method = c("BoxCox", "center", "scale", "pca")); trans
transformed <- predict(trans, quantitative) # apply the transformation 
head(transformed) #PCA didn't result in decreased attributes... removing that step to keep colnames the same

trans <- preProcess(quantitative, method = c("BoxCox", "center", "scale")); trans
transformed <- predict(trans, quantitative) # apply the transformation 
par(mfrow=c(3,3))
for (v in colnames(transformed)){
  subtitle <- sprintf("transformed$%s", v)
  hist(transformed[,v], main = v, xlab = subtitle)
}
# Find updated skewness values 
skewValues <- apply(transformed, 2, skewness) #2 means by column 
typeof(skewValues)
data.frame(skewValues)


#####################
# Binary attributes
#####################
# Wondering if we should convert these into proportions? 
par(mfrow=c(3,2))
barplot(table(heart_data$anaemia), main = 'anaemia', ylim = c(0,200))
barplot(table(heart_data$diabetes), main = 'diabetes', ylim = c(0,200))
barplot(table(heart_data$high_blood_pressure), main = 'high_blood_pressure', ylim = c(0,200))
barplot(table(heart_data$sex), main = 'sex', ylim = c(0,200))
barplot(table(heart_data$smoking), main = 'smoking', ylim = c(0,200))
barplot(table(heart_data$DEATH_EVENT), main = 'DEATH_EVENT', ylim = c(0,200))


# 2. Check for missing data
summary(heart_data)



# 3. Correlation Matrix - 
# doesn't really look like any of the quantitative predictors are highly correlated
par(mfrow=c(1,1))
correlations <- cor(quantitative); correlations
corrplot(correlations, type = "upper", order = "hclust", tl.col = "black")






