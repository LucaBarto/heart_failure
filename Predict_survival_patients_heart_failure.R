# Install all the necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(knitr)
library(funModeling)
library(caret)
library(ggthemes)

# Read data from file and save in a data frame
data <- read.csv('https://raw.githubusercontent.com/LucaBarto/heart_failure/main/heart_failure_clinical_records_dataset.csv')

# Explore structure
str(data)

# Explore dimension
dim(data)

# Summary of data
summary(data)

# Find correlation between predictors

# Change column name
colnames(data)[13] <- "target"

data <- data %>% dplyr::select(-time)

# Set all numeric outputs to 3 digits
options(digits = 3)

# Check for missing values
missing_value <- map_int(data, function(.x) sum(is.na(.x)))

# Check for duplicate
duplicated_value <- sum(duplicated(data))


# Correlation matrix
correlationMatrix <- cor(data[,1:ncol(data) - 1])

# The corrplot package is a graphical display of a correlation matrix, 
# confidence interval or general matrix
corrplot(correlationMatrix, order = "hclust", tl.cex = 1, addrect = 8)

# Find attributes that are highly corrected
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.6)

# Print indexes of highly correlated attributes
highlyCorrelated

# The correlation is below 0.60, therefore well below the cut-off of 0.70 
# below which it can be said that it does not exist high collinearity between
# predictors, but it is not enough to be confident in using Naive Bayes algorithm.

# Change target variable to factor
data$target <- as.factor(ifelse(data$target == 1, "yes", "no"))

# Check proportion of target
prop.table(table(data$target))

# We investigate the individual variables to understand if they are normally distributed.
# For this purpose, we use graphs and then implement the Shapiro-Wilk test.

##################################
##Check Normal distribution#######
##################################
qqplot_normal_detect <- function(x, yLab) {
  #Let's construct a QQ-plot using R code. Start by defining the vector of proportions.
  p <- seq(0.05, 0.95, 0.05)
  
  #To obtain the quantiles from the data, we can use the quantile function like this:
  sample_quantiles <- quantile(x, p)
  
  #To obtain the theoretical normal distribution quantiles with the corresponding average and SD, we use the qnorm function:
  theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
  
  #To see if they match or not, we plot them against each other and draw the identity line:
  qplot(theoretical_quantiles, sample_quantiles) + geom_abline() + 
    labs(x = "Theoretical normal distribution") +
    labs(y = paste(yLab, " quantiles"))
}

qqplot_normal_detect(data$age, 'Age')

qqplot_normal_detect(data$anaemia, 'Anaemia')

qqplot_normal_detect(data$creatinine_phosphokinase, 'Creatinine phosphokinase')

qqplot_normal_detect(data$diabetes, 'Diabetes')

qqplot_normal_detect(data$ejection_fraction, 'Ejection fraction')

qqplot_normal_detect(data$high_blood_pressure, 'High blood pressure')

qqplot_normal_detect(data$platelets, 'Platelets')

qqplot_normal_detect(data$serum_creatinine, 'Serum creatinine')

qqplot_normal_detect(data$serum_sodium, 'Serum sodium')

qqplot_normal_detect(data$sex, 'Sex')

qqplot_normal_detect(data$smoking, 'Smoking')

# Only Age and Serum sodium they appear to have a normal distribution

# To understand how much each variable differs from the normal distribution we implement the Shapiro-Wilk test

########################################################
###############Shapiro-Wilk test########################
########################################################

shapiro_age <- shapiro.test(data$age)

shapiro_anaemia <- shapiro.test(data$anaemia)

shapiro_creatinine_phosphokinase <- shapiro.test(data$creatinine_phosphokinase)

shapiro_diabetes <- shapiro.test(data$diabetes)

shapiro_ejection_fraction <- shapiro.test(data$ejection_fraction)

shapiro_high_blood_pressure <- shapiro.test(data$high_blood_pressure)

shapiro_platelets <- shapiro.test(data$platelets)

shapiro_serum_creatinine <- shapiro.test(data$serum_creatinine)

shapiro_serum_sodium <- shapiro.test(data$serum_sodium)

shapiro_sex <- shapiro.test(data$sex)

shapiro_smoking <- shapiro.test(data$smoking)

variables <- c('age', 'anaemia', 'creatinine phosphokinase', 'diabetes', 
               'ejection fraction', 'high blood pressure', 'platelets', 
               'serum creatinine', 'serum sodium', 'sex', 'smoking')

pvalues <- c(shapiro_age$p.value, shapiro_anaemia$p.value, shapiro_creatinine_phosphokinase$p.value,
             shapiro_diabetes$p.value, shapiro_ejection_fraction$p.value,
             shapiro_high_blood_pressure$p.value, shapiro_platelets$p.value,
             shapiro_serum_creatinine$p.value, shapiro_serum_sodium$p.value,
             shapiro_sex$p.value, shapiro_smoking$p.value)

df_shapiro <- data.frame(variables,pvalues)

colnames(df_shapiro) <- c('Variable', 'P-VALUE')

str(df_shapiro)

df_shapiro %>% kable(digits = 40)

# By analyzing the p-value we understand that variable  follow a normal distribution

########################################################
################Check Homoscedasticity##################
########################################################
homoscedasticity_test <- function(x, y, data, title) {
  lmMod <- lm(x ~ y, data=data) # initial model
  
  par(mfrow=c(2,2)) # init 4 charts in 1 panel
  plot(lmMod, caption = title, cex.caption = 1.2)
}

homoscedasticity_test(data$age, data$target, data, 'Age')
homoscedasticity_test(data$anaemia, data$target, data, 'Anaemia')
homoscedasticity_test(data$creatinine_phosphokinase, data$target, data, 'Creatinine phosphokinase')
homoscedasticity_test(data$diabetes, data$target, data, 'Diabetes')
homoscedasticity_test(data$ejection_fraction, data$target, data, 'Ejection fraction')
homoscedasticity_test(data$high_blood_pressure, data$target, data, 'High blood pressure')
homoscedasticity_test(data$platelets, data$target, data, 'Platelets')
homoscedasticity_test(data$serum_creatinine, data$target, data, 'Serum creatinine')
homoscedasticity_test(data$serum_sodium, data$target, data, 'Serum sodium')
homoscedasticity_test(data$sex, data$target, data, 'Sex')
homoscedasticity_test(data$smoking, data$target, data, 'Smoking')

par(mfrow=c(1,1))

# By analyzing the plot of Residuals (top-left) and the one of square root of Standard residuals (bottom-left),
# we see that no variable respects the hypothesis of Homoscedasticity

# Plot distribution of target
ggplot(data, aes(x=target)) +
  geom_bar(fill="blue",alpha=0.5) +
  theme_economist() +
  labs(title="Distribution of target")

# Plotting Numerical Data
plot_num(data, bins=10) 


# Target correlation with predictors
# Plot and facet wrap density plots
data %>% 
  gather("feature", "value", -target) %>%
  ggplot(aes(value, fill = target)) +
  geom_density(alpha = 0.5) +
  xlab("Feature values") +
  ylab("Density") +
  theme(legend.position = "top",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  facet_wrap(~ feature, scales = "free", ncol = 3)


################################################################################
#############################MACHINE LEARNING###################################
################################################################################

# Principal Component Analysis (PCA)
if(!require(rstudioapi)) install.packages("rstudioapi", repos = "http://cran.us.r-project.org")
if(!require(adabag)) install.packages("adabag", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071")

library(rstudioapi)
library(adabag) 
library(plyr) 
library(MASS)
library(gbm)
library(pROC)
library(rpart.plot)
library(knitr)
library(nnet)
library(randomForest)
library(e1071)

pca <- prcomp(data[,1:ncol(data) - 1], center = TRUE, scale = TRUE)
plot(pca, type="l")

# Summary of data after PCA
summary(pca)

# We need 11 variables to reach 95% of the variance

pca_df <- as.data.frame(pca$x)
ggplot(pca_df, aes(x=PC1, y=PC2, col=data$target)) + geom_point(alpha=0.5)

# The data of the first 2 components cannot be easly separated into two classes.

# We need 11 variables to reach 95% of the variance, so there is no point to 
# implement PCA to reduce the number of predictors.
# 
# We are going to use all predictors on dataset.


# Start training data

# Creation of the partition 80% and 20%
set.seed(1, sample.kind="Rounding")
target_index <- createDataPartition(data$target, times=1, p=0.8, list = FALSE)
train_data <- data[target_index, ]
test_data <- data[-target_index, ]

# Define train control 
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

# For each model we will set up a grid of adjustment parameters which, by 
# adapting to the model and calculating its performance, will allow us 
# to determine the values that provide optimal performance.


##############################################################
################Support Vector Machine########################
##############################################################

# Tuning parameters:
#   
#   sigma (Sigma)
#   C (Cost)

svmRadial_grid <- expand.grid(C = seq(0.1, 2, length = 20), sigma = 0.5)

set.seed(1, sample.kind="Rounding")
svmRadial_model <- train(target~., data=train_data,
                   method = "svmRadial", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = svmRadial_grid,
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   metric = "ROC")

svmRadial_model

plot(svmRadial_model)

# Predict data
set.seed(1, sample.kind="Rounding")
svmRadial_pred <- predict(svmRadial_model, newdata = test_data)

#Evaluate confusion matrix
svmRadial_confusionMatrix <- confusionMatrix(svmRadial_pred, test_data$target)

svmRadial_confusionMatrix

# Plot 10 most important variables
plot(varImp(svmRadial_model), top=10, main="Top variables Adaptive Boosting")

# serum creatinine, ejection fraction and age are the most important

# Define ROC Curve
svmRadial_rocCurve   <- roc(response = test_data$target,
                      predictor = as.numeric(svmRadial_pred),
                      levels = rev(levels(test_data$target)),
                      plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(svmRadial_rocCurve, print.thres = "best")


##############################################################
#####################Adaptive Boosting########################
##############################################################
# Set up tuning grid
am1_grid <- expand.grid(mfinal = (1:3)*3,         
                        maxdepth = c(1, 3),       
                        coeflearn = c("Zhu"))

# Train model
set.seed(1, sample.kind="Rounding")
am1_model <- train(target~., data=train_data,
                   method = "AdaBoost.M1", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = am1_grid,
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   metric = "ROC")

am1_model

plot(am1_model)

# Predict data
set.seed(1, sample.kind="Rounding")
am1_pred <- predict(am1_model, newdata = test_data)

#Evaluate confusion matrix
am1_confusionMatrix <- confusionMatrix(am1_pred, test_data$target)

am1_confusionMatrix

# Plot 10 most important variables
plot(varImp(am1_model), top=10, main="Top variables Adaptive Boosting")

# serum creatinine, age and ejection fraction are the most important

# Define ROC Curve
am1_rocCurve   <- roc(response = test_data$target,
                      predictor = as.numeric(am1_pred),
                      levels = rev(levels(test_data$target)),
                      plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(am1_rocCurve, print.thres = "best")

##############################################################
############Stochastic Gradient Boosting######################
##############################################################

# Set up tuning grid
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                         n.trees = (1:30)*50, 
                         shrinkage = 0.1,
                         n.minobsinnode = 20)

# Train model
set.seed(1, sample.kind="Rounding")
gbm_model <- train(target~., data=train_data,
                   method = "gbm", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = gbm_grid,
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   metric = "ROC")

gbm_model

plot(gbm_model)

# Predict data
set.seed(1, sample.kind="Rounding")
gbm_pred <- predict(gbm_model, newdata = test_data)

#Evaluate confusion matrix
gbm_confusionMatrix <- confusionMatrix(gbm_pred, test_data$target)

gbm_confusionMatrix

# Plot 10 most important variables
plot(varImp(gbm_model), top=10, main="Top variables Stochastic Gradient Boosting")

# serum creatinine, ejection fraction and age are the most important

# Define ROC Curve
gbm_rocCurve   <- roc(response = test_data$target,
                      predictor = as.numeric(gbm_pred),
                      levels = rev(levels(test_data$target)),
                      plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(gbm_rocCurve, print.thres = "best")

############################################################
###############Classification Trees#########################
############################################################

# Set up tuning grid
ct_grid <-  data.frame(cp = seq(0.0, 0.1, len = 25))

# Train model
set.seed(1, sample.kind="Rounding")
ct_model <- train(target~., data=train_data,
                  method = "rpart",
                  metric="ROC",
                  # center, scale - centering and scaling data
                  preProcess = c("center", "scale"), 
                  tuneGrid = ct_grid,
                  trControl = fitControl)

ct_model

plot(ct_model)

# Predict data
set.seed(1, sample.kind="Rounding")
ct_pred <- predict(ct_model, newdata = test_data)

# Evaluate confusion matrix
ct_confusionMatrix <- confusionMatrix(ct_pred, test_data$target)

ct_confusionMatrix

# Plot 10 most important variables
plot(varImp(ct_model), top=10, main="Top variables Classification Tree")

# age, serum creatinine and ejection fraction are the most important

# Define ROC Curve
ct_rocCurve   <- roc(response = test_data$target,
                     predictor = as.numeric(ct_pred),
                     levels = rev(levels(test_data$target)),
                     plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(ct_rocCurve, print.thres = "best")

# The graph below shows the decision tree
rpart.plot(ct_model$finalModel)

###########################################################
#################Random Forest#############################
###########################################################

# Set up tuning grid
rf_grid <-  data.frame(mtry = seq(1, 10))

# Train model
set.seed(1, sample.kind="Rounding")
rf_model <- train(target~., data=train_data,
                  method = "rf",
                  metric = "ROC",
                  # center, scale - centering and scaling data
                  preProcess = c("center", "scale"), 
                  tuneGrid = rf_grid,
                  ntree = 100,
                  trControl = fitControl)

rf_model

plot(rf_model)

# Predict data
set.seed(1, sample.kind="Rounding")
rf_pred <- predict(rf_model, newdata = test_data)

# Evaluate confusion matrix
rf_confusionMatrix <- confusionMatrix(rf_pred, test_data$target)

rf_confusionMatrix

# Plot 10 most important variables
plot(varImp(rf_model), top=10, main="Top variables Random Forest")

# serum creatinine, ejection fraction and age are the most important

# Define ROC Curve
rf_rocCurve   <- roc(response = test_data$target,
                     predictor = as.numeric(rf_pred),
                     levels = rev(levels(test_data$target)),
                     plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(rf_rocCurve, print.thres = "best")


###############################################################
#############K Nearest Neighbor (KNN) Model####################
###############################################################

# Set up tuning grid
knn_grid <-  data.frame(k = seq(1, 50, 1))

# Train model
set.seed(1, sample.kind="Rounding")
knn_model <- train(target~., data=train_data,
                   method="knn",
                   metric="ROC",
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   tuneGrid = knn_grid,
                   trControl=fitControl)

knn_model

plot(knn_model)

# Predict data
set.seed(1, sample.kind="Rounding")
knn_pred <- predict(knn_model, newdata = test_data)

# Evaluate confusion matrix
knn_confusionMatrix <- confusionMatrix(knn_pred, test_data$target)

knn_confusionMatrix

# Plot 10 most important variables
plot(varImp(knn_model), top=10, main="Top variables K Nearest Neighbor (KNN) Model")

# serum creatinine, ejection fraction and age are the most important

# Define ROC Curve
knn_rocCurve   <- roc(response = test_data$target,
                      predictor = as.numeric(knn_pred),
                      levels = rev(levels(test_data$target)),
                      plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(knn_rocCurve, print.thres = "best")

###############################################################
#####################Neural Network############################
###############################################################

# Set up tuning grid
nn_grid <- expand.grid(size = c(1:5, 10),
                       decay = c(0, 0.05, 0.1, 1, 2))

# Train model
set.seed(1, sample.kind="Rounding")
nn_model <- train(target~., data=train_data,
                  method="nnet",
                  metric="ROC",
                  # center, scale - centering and scaling data
                  preProcess = c("center", "scale"), 
                  tuneGrid = nn_grid,
                  trace=FALSE,
                  trControl=fitControl)

nn_model

plot(nn_model)

# Predict data
set.seed(1, sample.kind="Rounding")
nn_pred <- predict(nn_model, newdata = test_data)

# Evaluate confusion matrix
nn_confusionMatrix <- confusionMatrix(nn_pred, test_data$target)

nn_confusionMatrix

# Plot 10 most important variables
plot(varImp(nn_model), top=10, main="Top variables Neural Network Model")

# ejection fraction, serum creatinine and age are the most important

# Define ROC Curve
nn_rocCurve   <- roc(response = test_data$target,
                     predictor = as.numeric(nn_pred),
                     levels = rev(levels(test_data$target)),
                     plot = TRUE, col = "blue", auc = TRUE)

# Plot ROC curve
plot(nn_rocCurve, print.thres = "best")


##################################################
#############Compare algorithms###################
##################################################

#List of all algorithms
models_list <- list(SVM = svmRadial_model,
                    ADP_Boost = am1_model,
                    Gradient_Bost=gbm_model,
                    Class_Tree = ct_model,
                    RND_Forest=rf_model,
                    KNN=knn_model,
                    NN=nn_model) 

models_results <- resamples(models_list)

# Summary of algorithms
summary(models_results)

# Confusion matrix of the algorithms
confusion_matrix_list <- list(
  SVM = svmRadial_confusionMatrix,
  ADP_Boost=am1_confusionMatrix, 
  Gradient_Bost=gbm_confusionMatrix,
  Class_Tree = ct_confusionMatrix,
  RND_Forest=rf_confusionMatrix,
  KNN=knn_confusionMatrix,
  NN=nn_confusionMatrix) 

confusion_matrix_list_results <- sapply(confusion_matrix_list, function(x) x$byClass)
confusion_matrix_list_results %>% kable()


################################################################################
###########Use only serum creatinine, ejection fraction and age#################
################################################################################

data <- data %>% dplyr::select(serum_sodium, ejection_fraction, age, target)


# Start training data

# Creation of the partition 80% and 20%
set.seed(1, sample.kind="Rounding")
target_index <- createDataPartition(data$target, times=1, p=0.8, list = FALSE)
train_data <- data[target_index, ]
test_data <- data[-target_index, ]

# Define train control 
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

# For each model we will set up a grid of adjustment parameters which, by 
# adapting to the model and calculating its performance, will allow us 
# to determine the values that provide optimal performance.


##############################################################
################Support Vector Machine########################
##############################################################

svmRadial_grid <- expand.grid(C = seq(0.1, 2, length = 20), sigma = 0.5)

set.seed(1, sample.kind="Rounding")
svmRadial_model <- train(target~., data=train_data,
                         method = "svmRadial", 
                         trControl = fitControl, 
                         verbose = FALSE, 
                         tuneGrid = svmRadial_grid,
                         # center, scale - centering and scaling data
                         preProcess = c("center", "scale"), 
                         metric = "ROC")

set.seed(1, sample.kind="Rounding")
svmRadial_pred <- predict(svmRadial_model, newdata = test_data)

#Evaluate confusion matrix
svmRadial_confusionMatrix <- confusionMatrix(svmRadial_pred, test_data$target)

##############################################################
#####################Adaptive Boosting########################
##############################################################
# Set up tuning grid
am1_grid <- expand.grid(mfinal = (1:3)*3,         
                        maxdepth = c(1, 3),       
                        coeflearn = c("Zhu"))

# Train model
set.seed(1, sample.kind="Rounding")
am1_model <- train(target~., data=train_data,
                   method = "AdaBoost.M1", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = am1_grid,
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   metric = "ROC")

set.seed(1, sample.kind="Rounding")
am1_pred <- predict(am1_model, newdata = test_data)

# Evaluate confusion matrix
am1_confusionMatrix <- confusionMatrix(am1_pred, test_data$target)

##############################################################
############Stochastic Gradient Boosting######################
##############################################################

# Set up tuning grid
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                         n.trees = (1:30)*50, 
                         shrinkage = 0.1,
                         n.minobsinnode = 20)

# Train model
set.seed(1, sample.kind="Rounding")
gbm_model <- train(target~., data=train_data,
                   method = "gbm", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = gbm_grid,
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   metric = "ROC")

set.seed(1, sample.kind="Rounding")
gbm_pred <- predict(gbm_model, newdata = test_data)

# Evaluate confusion matrix
gbm_confusionMatrix <- confusionMatrix(gbm_pred, test_data$target)

############################################################
###############Classification Trees#########################
############################################################

# Set up tuning grid
ct_grid <-  data.frame(cp = seq(0.0, 0.1, len = 25))

# Train model
set.seed(1, sample.kind="Rounding")
ct_model <- train(target~., data=train_data,
                  method = "rpart",
                  metric="ROC",
                  # center, scale - centering and scaling data
                  preProcess = c("center", "scale"), 
                  tuneGrid = ct_grid,
                  trControl = fitControl)

set.seed(1, sample.kind="Rounding")
ct_pred <- predict(ct_model, newdata = test_data)

# Evaluate confusion matrix
ct_confusionMatrix <- confusionMatrix(ct_pred, test_data$target)

###########################################################
#################Random Forest#############################
###########################################################

# Set up tuning grid
rf_grid <-  data.frame(mtry = seq(1, 10))

# Train model
set.seed(1, sample.kind="Rounding")
rf_model <- train(target~., data=train_data,
                  method = "rf",
                  metric = "ROC",
                  # center, scale - centering and scaling data
                  preProcess = c("center", "scale"), 
                  tuneGrid = rf_grid,
                  ntree = 100,
                  trControl = fitControl)

set.seed(1, sample.kind="Rounding")
rf_pred <- predict(rf_model, newdata = test_data)

# Evaluate confusion matrix
rf_confusionMatrix <- confusionMatrix(rf_pred, test_data$target)

###############################################################
#############K Nearest Neighbor (KNN) Model####################
###############################################################

# Set up tuning grid
knn_grid <-  data.frame(k = seq(1, 50, 1))

# Train model
set.seed(1, sample.kind="Rounding")
knn_model <- train(target~., data=train_data,
                   method="knn",
                   metric="ROC",
                   # center, scale - centering and scaling data
                   preProcess = c("center", "scale"), 
                   tuneGrid = knn_grid,
                   trControl=fitControl)

set.seed(1, sample.kind="Rounding")
knn_pred <- predict(knn_model, newdata = test_data)

# Evaluate confusion matrix
knn_confusionMatrix <- confusionMatrix(knn_pred, test_data$target)

###############################################################
#####################Neural Network############################
###############################################################

# Set up tuning grid
nn_grid <- expand.grid(size = c(1:5, 10),
                       decay = c(0, 0.05, 0.1, 1, 2))

# Train model
set.seed(1, sample.kind="Rounding")
nn_model <- train(target~., data=train_data,
                  method="nnet",
                  metric="ROC",
                  # center, scale - centering and scaling data
                  preProcess = c("center", "scale"), 
                  tuneGrid = nn_grid,
                  trace=FALSE,
                  trControl=fitControl)

set.seed(1, sample.kind="Rounding")
nn_pred <- predict(nn_model, newdata = test_data)

# Evaluate confusion matrix
nn_confusionMatrix <- confusionMatrix(nn_pred, test_data$target)

##################################################
#############Compare algorithms###################
##################################################

#List of all algorithms
models_list <- list(SVM = svmRadial_model,
                    ADP_Boost = am1_model,
                    Gradient_Bost=gbm_model,
                    Class_Tree = ct_model,
                    RND_Forest=rf_model,
                    KNN=knn_model,
                    NN=nn_model) 

models_results <- resamples(models_list)

# Summary of algorithms
summary(models_results)

# Confusion matrix of the algorithms
confusion_matrix_list <- list(
  SVM = svmRadial_confusionMatrix,
  ADP_Boost=am1_confusionMatrix, 
  Gradient_Bost=gbm_confusionMatrix,
  Class_Tree = ct_confusionMatrix,
  RND_Forest=rf_confusionMatrix,
  KNN=knn_confusionMatrix,
  NN=nn_confusionMatrix) 

confusion_matrix_list_results <- sapply(confusion_matrix_list, function(x) x$byClass)
confusion_matrix_list_results %>% kable()
