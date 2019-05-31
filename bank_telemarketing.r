if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org"); library(tidyverse)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org"); library(caret)
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org"); library(Rborist)
if(!require(e1071))  install.packages("e1071", repos = "http://cran.us.r-project.org");library(e1071)
if(!require(klaR))  install.packages("klaR", repos = "http://cran.us.r-project.org");library(klaR)
if(!require(pROC))  install.packages("pROC", repos = "http://cran.us.r-project.org");library(pROC)
if(!require(mice))  install.packages("mice", repos = "http://cran.us.r-project.org");library(mice)
if(!require(VIM))  install.packages("VIM", repos = "http://cran.us.r-project.org"); library(VIM)
if(!require(glmnet))  install.packages("glmnet", repos = "http://cran.us.r-project.org"); library(glmnet)
if(!require(fastICA))  install.packages("fastICA", repos = "http://cran.us.r-project.org"); library(fastICA)
if(!require(kernlab))  install.packages("kernlab", repos = "http://cran.us.r-project.org"); library(kernlab)
if(!require(randomForest))  install.packages("randomForest", repos = "http://cran.us.r-project.org"); library(randomForest)
if(!require(reshape2))  install.packages("reshape2", repos = "http://cran.us.r-project.org"); library(reshape2)


#############################################################
## Load data
#############################################################
dl <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip", dl)

unzip(dl, "bank-additional/bank-additional-full.csv")

rm(dl)

calls <- read.csv("./bank-additional/bank-additional-full.csv",sep=";")

#############################################################
## EDA
#############################################################
summary(calls)


# Get correlation matrix
(cormat <- Filter(is.numeric,calls) %>%
    cor(method="pearson"))

melted_cormat <- melt(cormat)

melted_cormat %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", high = "red", mid="white",
                       midpoint=0, limit= c(-1,1), space="Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
  coord_fixed()

## Variable importance
calls_y = calls$y
calls_x <- subset(calls, select=-c(duration, y))

rf <- randomForest(calls_x, calls_y, ntree = 100)
imp <- as.data.frame(importance(rf))
variables <- names(calls_x)
imp <- cbind(imp,variables) %>%
  arrange(desc(MeanDecreaseGini))
# set variables as factor to maintain orden in the plot
imp$variables <- factor(imp$variables, levels=imp$variables)
ggplot(imp, aes(x=variables,y=MeanDecreaseGini)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

## euribor3m histogram
calls %>%
  ggplot(aes(x=euribor3m, fill=y)) +
  geom_histogram(bins=6)


## Age plots
calls %>%
  ggplot(aes(x=age, fill=y)) +
  geom_histogram(binwidth=5)

age_success <- calls %>%
  mutate(age_round = floor(age/10)*10, bin=paste(age_round, age_round+10,sep="-")) %>%
  group_by(bin,y) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(bin) %>%
  spread(y,n, fill=0) %>%
  mutate(success_percent = (yes /(no+yes))*100)

age_success %>%
  ggplot(aes(x=bin, y=success_percent)) +
  geom_bar(stat="identity")

## job plots
calls %>%
  ggplot(aes(x=job, fill=y)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

job_success <- calls %>%
  group_by(job,y) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(job) %>%
  spread(y,n) %>%
  mutate(success_percent = (yes /(no+yes))*100) %>%
  arrange(desc(success_percent))

# best rates regarding jobs
head(job_success,5)

# worse rates regarding jobs
tail(job_success,5)


### Education plots
calls %>%
  ggplot(aes(x=education, fill=y)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

table(calls$education)

# Fusion illiterate category in education variable to the lowest education level
levels_ed <- levels(calls$education)
levels(calls$education) <- ifelse(levels_ed=="illiterate","basic.4y",levels_ed)
rm(levels_ed)

education_success <- calls %>%
  group_by(education,y) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(education) %>%
  spread(y,n) %>%
  mutate(success_percent = (yes /(no+yes))*100) %>%
  arrange(desc(success_percent))

education_success

## Near zero variance
nzv <- nearZeroVar(calls)
names(calls)[nzv]

table(calls$pdays)

calls %>%
  group_by(pdays) %>%
  summarize(n = n()) %>%
  mutate(freq = (n / sum(n)) * 100) %>%
  filter(pdays==999) %>% .$freq

calls %>%
  filter(pdays != 999) %>%
  group_by(y) %>%
  summarise(n=n())

## Variable default
entropy = function(vector) {
  px = table(vector) / length(vector)
  lpx = log(px, base=2)
  -sum(px * lpx)
}

entropy(droplevels(calls$default[calls$default != "unknown"]))

## Missing values
# replace unknown by NA

calls$job <- as.character(calls$job)
calls$job <- ifelse(calls$job == "unknown", NA, calls$job) 
calls$job <- factor(calls$job)

calls$marital <- as.character(calls$marital)
calls$marital <- ifelse(calls$marital == "unknown", NA, calls$marital) 
calls$marital <- factor(calls$marital)

calls$education <- as.character(calls$education)
calls$education <- ifelse(calls$education == "unknown", NA, calls$education) 
calls$education <- factor(calls$education)

calls$housing <- as.character(calls$housing)
calls$housing <- ifelse(calls$housing == "unknown", NA, calls$housing) 
calls$housing <- factor(calls$housing)

calls$loan <- as.character(calls$loan)
calls$loan <- ifelse(calls$loan == "unknown", NA, calls$loan) 
calls$loan <- factor(calls$loan)


aggr_plot <- aggr(calls[,c("job","marital","education", "housing", "loan")], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=c("job","marital","education", "housing", "loan"), cex.axis=.7,
                  gap=3, ylab=c("Missing data", "Pattern"))


#####################################################################
##  PREPROCESSING
#####################################################################

#####################################################################
# Remove variables with low predictive power

# get get out duration as well as this is a parameter that we can't know before hand
calls <- subset(calls, select=-c(duration, default, emp.var.rate))


#####################################################################
# Imputation of missing values
############################

## Impute variables
init = mice(calls, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# Remove some variables as predictors
predM[, c("contact","month","day_of_week", "campaign", "pdays",
          "previous","poutcome", "cons.price.idx","cons.conf.idx",
          "euribor3m","y")]=0

# set methods for prediction
meth[c("job", "marital","education")] = "polyreg"
meth[c("housing","loan")] = "logreg"


set.seed(103)
imputed = mice(calls, method=meth, predictorMatrix=predM, m=5)

imputed <- complete(imputed)

## ordered factors
imputed$education <- factor(imputed$education, ordered=TRUE, levels = c("basic.4y","basic.6y","basic.9y","high.school","professional.course","university.degree"))

summary(imputed)

#################################################################################
## divide in 90% train set and 10% test set
set.seed(204)
test_index <- createDataPartition(y = imputed$y, times = 1, p = 0.1, list = FALSE)

train_set <- imputed[-test_index,]
test_set <- imputed[test_index,]


rm(test_index)

#################################################################################
## Testing algorithms
#################################################################################

## This is a sample that will be used to train the hiperparameters of several algorithms
n <- 6500
index <- sample(nrow(train_set), n)


######################################
## glmn
fit_glm <- glm(y ~ . , 
               data=train_set, 
               family="binomial")
p_glm <- predict(fit_glm, test_set, type="response")

ROC <- roc(test_set$y, p_glm)
plot(ROC, col = "red", print.auc=TRUE)

auc_results <- tibble(method = "glm", AUC = pROC::auc(ROC))

######################################
## glmnet
glmnet_control <- trainControl(
  method = "cv", number=5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

glmnet_grid <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 20))

train_glmnet <- train(
  y ~ ., train_set,
  tuneGrid = glmnet_grid,
  method = "glmnet",
  trControl = glmnet_control,
  metric="ROC"
)

# plot the model
plot(train_glmnet)

p_glmnet <- predict(train_glmnet, test_set, type="prob")


ROC <- roc(test_set$y, p_glmnet[,"yes"])
plot(ROC, col = "red", print.auc=TRUE)

auc_results <- rbind(auc_results,
                          tibble(method="glmnet",  
                                     AUC = pROC::auc(ROC) ))
######################################
## knn

control_knn <- trainControl(
  method="cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE)


train_knn <- train(y ~ ., 
                   data=train_set[index,],
                   method = "knn", 
                   tuneGrid = data.frame(k = c(9,14,18,21,25)),
                   trControl = control_knn,
                   metric="ROC")

# plot the model
plot(train_knn)

fit_knn <- knn3(y ~., train_set,  k = train_knn$bestTune$k)
p_knn  <- predict(fit_knn, test_set)

ROC <- roc(test_set$y, p_knn[,"yes"])
plot(ROC, col = "red", print.auc=TRUE)

auc_results <- rbind(auc_results,
                         tibble(method="knn",  
                                    AUC = pROC::auc(ROC) ))

######################################
## Naive Bayes

control_nb <- trainControl(
  method="cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE)


nb_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL=0:3,
  adjust = c(0.5, 1, 1.5, 2)
)

preProc_nb <- preProcess(subset(train_set, select=-y), method=c("center", "scale","ica"),n.comp=5)
preProc_train_set <- predict(preProc_nb,train_set)
preProc_test_set <- predict(preProc_nb,test_set)

train_nb <- train(y ~.,
                  data=preProc_train_set[index,],
                  method="nb",
                  trControl = control_nb,
                  tuneGrid = nb_grid,
                  metric="ROC")

#Plot the model
plot(train_nb)

fit_nb <- NaiveBayes(y ~ .,
                     data=preProc_train_set,
                     fL = train_nb$bestTune$fL,
                     usekernel = train_nb$bestTune$usekernel,
                     adjust=train_nb$bestTune$adjust)
p_nb <- predict(fit_nb, preProc_test_set)$posterior

ROC <- roc(test_set$y, p_nb[,"yes"])
plot(ROC, col = "red", print.auc=TRUE)
auc_results <- rbind(auc_results,
                         tibble(method="Naive Bayes",  
                                    AUC = pROC::auc(ROC) ))

######################################
## RBorist

# Preprocessing: Rborist returns an error of unsopported type where there are ordered factors
preProc_train_set <- train_set
preProc_train_set$education <- factor(preProc_train_set$education, ordered=FALSE)

preProc_test_set <- test_set
preProc_test_set$education <- factor(preProc_test_set$education, ordered=FALSE)

# hiperparameter tuning
control_rf <- trainControl(
  method="cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE)

grid_rf <- expand.grid(minNode = c(1,2) , predFixed = c(10, 14, 17))

train_rf <-  train(y ~ ., 
                   data = preProc_train_set[index,], 
                   method = "Rborist", 
                   nTree = 50,
                   trControl = control_rf,
                   tuneGrid = grid_rf,
                   nSamp = 5000,
                   metric="ROC")

## plot the model
plot(train_rf)


fit_rf <- Rborist(subset(preProc_train_set, select=-y),
                  train_set$y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed,
                  verbose=TRUE)


p_rf <- predict(fit_rf, subset(preProc_test_set, select=-y))$census  
p_rf<- p_rf / rowSums(p_rf)


ROC <- roc(test_set$y, p_rf[,"yes"])
plot(ROC, col = "red", print.auc=TRUE)
auc_results <- rbind(auc_results,
                         tibble(method="Rborist",  
                                    AUC = pROC::auc(ROC) ))


######################################
## svmRadial
set.seed(305)
preProc_svmRadial <- preProcess(subset(train_set, select=-y), method=c("center", "scale", "pca"))
preProc_train_set <- predict(preProc_svmRadial,train_set)
preProc_test_set <- predict(preProc_svmRadial,test_set)

tuned_parameters <- tune.svm(y~., 
                             data = preProc_train_set[index,], 
                             gamma = 10^(-5:-1), 
                             cost = c(0.01, 0.1, 0.5, 1, 3))

plot(tuned_parameters)


fit_svmRadial <- svm(y ~ ., 
                     data=preProc_train_set, 
                     kernel="radial", 
                     cost=tuned_parameters$best.parameters$cost,
                     gamma=tuned_parameters$best.parameters$gamma,
                     probability=TRUE)

pred <- predict(fit_svmRadial, preProc_test_set, probability=TRUE)

p_svmRadial <- attr(pred,"probabilities")

ROC <- roc(test_set$y, p_svmRadial[,"yes"])
plot(ROC, col = "red", print.auc=TRUE)
auc_results <- rbind(auc_results,
                         tibble(method="svmRadial",  
                                    AUC = pROC::auc(ROC) ))


######################################
## Ensemble of previous models

p_ensemble <- (p_glm + p_glmnet + p_knn + p_nb + p_rf + p_svmRadial) / 6

ROC <- roc(test_set$y, p_ensemble[,"yes"])
plot(ROC, col = "red", print.auc=TRUE)
auc_results <- rbind(auc_results,
                         tibble(method="ensemble",  
                                    AUC = pROC::auc(ROC) ))

auc_results

y_pred <- factor(ifelse(apply(p_ensemble, 1, which.max)-1==1, "yes", "no"))

cm_ensemble <- confusionMatrix(y_pred, test_set$y, positive="yes")

cm_ensemble

#################################################################################
# Selecting the 1000 customers with the best probability to subscribe

selection <- test_set[order(p_ensemble$yes, decreasing=TRUE)[1:1000],]
sel_subscriptors <- sum(selection$y=="yes")
tot_subscriptors <- sum(test_set$y=="yes")

#################################################################################
## Accuracy - sensitivity trade off
#################################################################################

## performance indicators function
performance_indicators <- function(cut, probs, y) {
  y_hat = factor(ifelse(probs > cut, "yes", "no"), levels=c("yes","no"))
  w = which(y=="yes")
  sensitivity = mean( y_hat[w] == "yes" ) 
  specificity = mean( y_hat[-w] == "no" ) 
  accuracy = mean( y==y_hat ) 
  selection_rate = length(which(y_hat == "yes")) / length(y_hat)
  out = t(as.matrix(c(cut, sensitivity, specificity, accuracy, selection_rate)))
  colnames(out) = c("cut","sensitivity", "specificity", "accuracy", "pred_yes")
  return(out)
  
}

cuts <- seq(.01,.99,length=1000)

list_cut <- lapply(cuts, function(x) {performance_indicators(x,p_ensemble[,"yes"],test_set$y)})
df_cut <- data.frame(do.call(rbind, list_cut))

df_cut %>%
  ggplot() +
  geom_line(aes(x=cut,y=sensitivity, color="Sensitivity")) +
  geom_line(aes(x=cut,y=specificity, color="Specificity")) +
  geom_line(aes(x=cut,y=accuracy, color="Accuracy")) +
  geom_line(aes(x=cut,y=pred_yes, color="Subscriptions predicted")) +
  labs(color = 'Performance indicators')
