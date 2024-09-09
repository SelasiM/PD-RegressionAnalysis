               
                   ### INSTALLING PACKAGES ###

# You might need to install packages with install.packages()
#install.packages("readr")
#install.packages("pysch")
#install.packages("pander")
#install.packages("MASS")
#install.packages("rpart")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("tidymodels")
#install.packages("tidyr")
#install.packages("rpart.plot")
#install.packages("vip")
#install.packages("car")
#install.packages("glmnet")

                      ### Load Libraries ###

library(readr) # read file into RStudio
library(psych) # for file upload without path name
library(pander) # creates tables
library(MASS) # AIC All Subsets Regression
library(rpart)# decision tree
library(caret) # claassification of decision tree/ cross validation
library(dplyr) # data manipulation 
library(tidymodels) # machine learning package/ cross validation
library(tidyr) # reshapeing data
library(rpart.plot) # plot tree model
library(vip) # decision tree model variable selection
library(car) # model visulization
library(glmnet) # cross validation and lasso, ridge, and elastic net


                      ### Load the Dataset ###

# Load with File Prompt (no direct path needed) 
parkinsonsTelemonitoring<- read.csv(file.choose(), header = T)

# Load with direct file path ( need to change path)
#parkinsonsTelemonitoring <- read.csv("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/parkinsons_telemonitoring.csv", 
                                 #    header = T)


                  ### Initial Data Examination ###

### make dummy df # 
pt <- parkinsonsTelemonitoring

# look at first few rows #
head(parkinsonsTelemonitoring)

# look at the dimensions of the data #
dim(pt)

# look at the column names of the data #
names(pt)


                          ### Data Cleaning/ Preprocessing ###
# standardize names in data set
names(pt) <- tolower(names(pt))
names(pt) <- gsub("\\.", "_", names(pt))
names(pt)[names(pt) == "subject_"] <- gsub("[_]", "", names(pt)[names(pt) == "subject_"])
names(pt)[names(pt) == "jitter___"] <- gsub("[_]", "", names(pt)[names(pt) == "jitter___"])
names(pt)[names(pt) == "jitter_abs_"] <- substr(names(pt)[names(pt) == "jitter_abs_"], 1, nchar(names(pt)[names(pt) == "jitter_abs_"]) - 1)
names(pt)[names(pt) == "shimmer_db_"] <- substr(names(pt)[names(pt) == "shimmer_db_"], 1, nchar(names(pt)[names(pt) == "shimmer_db_"]) - 1)
print(names(pt))

# looking for nulls #
sum(is.na(pt))

# check for duplicate rows
dups <- pt[duplicated(pt), ]
print(dups)

## look for outliers ##
# help('sapply')

# find outliers with iqr
fd_out <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  return(x < lower_bound | x > upper_bound)}

# use function on dataset
out <- lapply(pt[, sapply(pt, is.numeric)], fd_out)

out_count <- sapply(out, table)

# print the count of outliers
print(out_count)


                      #### Exploratory Analysis ###

                  # SUMMARY OF THE STATISTICS ANALYSIS #

# look at the statistics of the dataset #
sum_stats <- summary(pt)
View(sum_stats)

                        # BOX PLOT ANALYSIS #

# look at box plots of data #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/boxplot_matrix.png", 
    width = 800, height = 600) 
par(mfrow = c(4, 6))  # grid of 4 by 6 #
for (i in 1:22) {
  boxplot(pt[,i], 
          main = paste("Boxplot of", colnames(pt)[i]), 
          xlab = "Class", 
          ylab = colnames(pt)[i])}
dev.off()
graphics.off()

                         # HISTOGRAM ANALYSIS #

# 22 random colors for histogram and correlation plot
colors <- rainbow(22)

# create a series of histograms for each feature #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/histogram_matrix.png", 
    width = 800, height = 600) 
par(mfrow = c(4, 6))  # grid of 4 by 6 #
for (i in 1:22)   
{hist(pt[, i], 
      main = paste("Histogram of", 
                   colnames(pt)[i]), 
      xlab = colnames(pt)[i], 
      col = colors[i])}
dev.off()
graphics.off()

                    # CORRELATION ANALYSIS / HEAT MAPS OF CORRELATION #

# correlation of data #
cor(pt)

# remove low correlating variables in correlation # 
correlation_df <- pt[,-c(1,2,3,4)]

# make variable for correlation of the data #
correlation_matrix <- cor(correlation_df, method = "spearman")

# shows all correlations for all variables #
correlation_matrix_all <- cor(pt, method = "spearman")

# 22 random colors for correlation plot
colors <- rainbow(22) 

                            # HEAT MAPS OF CORRELATION #

# help("corPlot")
# correlation matrix of just significant values #

# save to PNG #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/correlation_heatmap.png", 
    width = 800, height = 600) 
corPlot(correlation_matrix, 
        symmetric = F, 
        diag = T, 
        colors = T,
        main = "Spearman Correlation Heat Map of Parkinson's Telemonitoring Data", 
        show.legend = T, 
        scale = T, 
        upper = F, 
        digits = 3, 
        zlim = c(min(correlation_matrix), max(correlation_matrix)),
        las = 2
        )
dev.off()
graphics.off()


                            # correlation of all values #

# save to PNG #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/correlation_heatmap_all.png", 
    width = 800, height = 600) 
corPlot(correlation_matrix_all, 
        symmetric = F, 
        diag = T, 
        colors = T,
        main = "Spearman Correlation Heat Map of Parkinson's Telemonitoring Data", 
        show.legend = T, 
        scale = T, 
        upper = F, 
        digits = 3, 
        zlim = c(min(correlation_matrix), max(correlation_matrix)),
        las = 2
)
dev.off()
graphics.off()

                  # SCATTER PLOTS of TOTAL UPDRS AND MOTOR UPDRS #
                       
                         # total UPDRS scatterplot array #

# PNG to get picture of scatterplot matrix #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/scatterplot_matrix_TOTAL.png", 
    width = 1000, height = 1000)
par(mfrow = c(5, 5)) # five by 5 grid of scatterplots #
# loop through all columns except total UPDRS #
for (i in which(names(pt) != "total_updrs")) {
  # create a scatterplot of the column against total UPDRS #
  plot(pt[, i], pt$total_updrs,
       xlab = colnames(pt)[i], 
       ylab = "Total UPDRS")
  # fit a linear regression line
  lm_fit <- lm(pt$total_updrs ~ pt[, i])
  # add a regression line
  abline(lm_fit, col = "red")}

# close the PNG #
dev.off()
graphics.off()


                              # motor UPDRS scatterplot array #

png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/scatterplot_matrix_MOTOR.png", 
    width = 1000, height = 1000)
par(mfrow = c(5, 5)) # five by 5 grid of scatterplots #
# loop through all columns except motor_UPDRS #
for (i in which(names(pt) != "motor_updrs")) {
  # create a scatterplot of the column against motor_UPDRS #
  plot(pt[, i], pt$motor_updrs,
       xlab = colnames(pt)[i], 
       ylab = "Motor UPDRS")
  # fit a linear regression line
  lm_fit <- lm(pt$motor_updrs ~ pt[, i])
  # add a regression line
  abline(lm_fit, col = "red")}

# close the PNG #
dev.off()
graphics.off()

            # NORMAILIZED SCATTER PLOT ARRAY FOR MOTOR UPDRS AND TOTAL UPDRS #

# function to perform min-max scaling #
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))}

# apply min-max scaling to numeric columns #
pt_scaled <- as.data.frame(lapply(pt, function(x) {
  if (is.numeric(x)) min_max_scaling(x) else x}))

                   # total UPDRS scatterplot array - normalized #

# save the plot to a PNG #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/scatterplot_matrix_normalized_TOTAL.png", 
    width = 1000, height = 1000)
par(mfrow = c(5, 5)) # grid of scatterplots 5 by 5

# loop through each column besides "total_updrs" #
for (i in which(names(pt_scaled) != "total_updrs")) 
 
   # create a scatterplot of the column against "total_updrs" #
  {plot(pt_scaled[, i], pt_scaled$total_updrs,
       xlab = colnames(pt_scaled)[i], 
       ylab = "Total UPDRS")
  
  # fit a linear regression line #
  lm_fit <- lm(pt_scaled$total_updrs ~ pt_scaled[, i])
  
  # add the regression line to the plot #
  abline(lm_fit, col = "red")}

# close the PNG #
dev.off()
graphics.off()


                  # motor UPDRS scatterplot array- normalized #

png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/scatterplot_matrix_normalized_MOTOR.png", 
    width = 1000, height = 1000)
par(mfrow = c(5, 5)) # grid of scatterplots 5 by 5

# loop through each column besides "motor_updrs" #
for (i in which(names(pt_scaled) != "motor_updrs")) 
  
  # create a scatterplot of the column against "motor_updrs" #
  {plot( pt_scaled[, i], pt_scaled$motor_updrs,
             xlab = colnames(pt)[i], 
             ylab = "Motor UPDRS")
  
  # fit a linear regression line #
  lm_fit <- lm(pt_scaled$motor_updrs ~ pt_scaled[, i])
  
  # add the regression line to the plot #
  abline(lm_fit, col = "red")}

# close the PNG #
dev.off()
graphics.off()

          ### REMOVAL OF UNNECCESSARY FEATURES FOR PREDICTION ###
              
# look at feature names
names(pt)

# remove feature based on unusefulness or we decided to concentrate on total
# updrs due to it encompassing motor updrs
pt_cleaned <- subset(pt, select = -c(subject, motor_updrs))

                     ### Removal of Outliers ####

#Convex hull method
# Define the predictors of interest
preds <- c("total_updrs", "jitter", "jitter_abs", "jitter_rap", 
                "jitter_ppq5", "jitter_ddp", "shimmer", "shimmer_db", 
                "shimmer_apq3", "shimmer_apq5", "shimmer_apq11", 
                "shimmer_dda", "nhr", "hnr", "rpde", "dfa", "ppe")

# for loop to remove outliers
for (p in preds) {
  # Calculate the convex hull
  h <- chull(pt_cleaned[, c("total_updrs", p)])
  
  # outliers removed b/c hull algorithm 
  pt_cleaned <- pt_cleaned[-h, ]
  
  # Print the dimensions after outlier removal
  cat("These are the new dimensions of ", p, ":", dim(pt_cleaned), "\n")}

# look at dimensions of old and new dataframe
dim(pt) 
#5875 rows 
dim(pt_cleaned) 
#5563 rows now

# looking for nulls #
sum(is.na(pt_cleaned))

names(pt_cleaned)

# make df with just non- normal data
df_nonnormal <- pt_cleaned[,-c(1,2,3,4,17,18,19,20)]
dim(df_nonnormal)
names(df_nonnormal)

# look at histogram again just non-normal variables
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/histogram_matrix_nonnormal.png", 
    width = 800, height = 600) 
par(mfrow = c(6, 2))  # grid of 6 by 2#
for (i in 1:12) {
  hist(df_nonnormal[, i], 
      main = paste(colnames(df_nonnormal)[i]), 
      xlab = colnames(df_nonnormal)[i], 
      col = colors[i])}
dev.off()
graphics.off()


                         # TRANSFORMATIONS #
                 ## log of all variables that are skewed ##
                 
# make another dummy variable
log_cleaned <- pt_cleaned

# make all variables log transformed
log_cleaned <- mutate_at(log_cleaned, c("jitter", "jitter_abs", "jitter_rap", 
                          "jitter_ppq5", "jitter_ddp", "shimmer", 
                          "shimmer_db", "shimmer_apq3", 
                          "shimmer_apq5", "shimmer_apq11", 
                          "shimmer_dda", "nhr"), log)

# make another df to make another histogram of cleaned nonnormal data                         
nonnormal_fixed <- log_cleaned[,-c(1,2,3,4,17,18,19,20)]
                          
# replot histograms
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/histogram_matrix_log.png", 
    width = 800, height = 600) 
par(mfrow = c(6, 2))  # grid of 6 by 2 #
for (i in 1:12)   
  {hist(nonnormal_fixed[, i], 
      main = paste(colnames(nonnormal_fixed)[i]), 
      xlab = colnames(nonnormal_fixed)[i], 
      col = colors[i])}

dev.off()
graphics.off()

                  # CREATE FORMULAS TO USE IN MODELING #

# specify the formulas for regression #
f_total <- total_updrs ~ .

                    ### MODEL MAKING ###
                  
                    ### FIRST ORDER MODELS NO Transformation###

model_total <- lm(f_total, data = pt_cleaned)
summary (model_total)

# Jitter.RAP, Jitter.DDP, Jitter..., Shimmer.db, Shimmer.APQ3, 
# Shimmer.DDA all seem to be insignificant in model. 
## Feature selection
m1 <- stepAIC(model_total, direction = "backward")
summary(m1)

names(pt_cleaned)

# look at multicollinearity
vif(m1)

# remove shimmer 
m2<- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
         jitter_ppq5 + jitter_ddp + shimmer_db + shimmer_apq3 + 
         shimmer_apq5 + shimmer_apq11 + nhr + hnr + rpde + dfa + ppe, 
         data = pt_cleaned)
summary(m2)

# look at multicollinearity
vif(m2)

# remove shimmer_apq5
m3<- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
               jitter_ppq5 + jitter_ddp + shimmer_db + shimmer_apq3 + 
               shimmer_apq11 + nhr + hnr + rpde + dfa + ppe, 
             data = pt_cleaned)
summary(m3)

# look at multicollinearity again
vif(m3)

# remove shimmer_db 
m4 <- lm(total_updrs ~ age + sex + test_time + jitter_abs + 
                 jitter_ddp +  
                 shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + dfa + ppe, 
               data = pt_cleaned)
summary(m4)

# looked at VIF and multicollinerarity looks good
vif(m4)

#residuals
plot(m4)
                   ## First order Log Transformed variables ##
# look at log first order model
model_total_log <- lm(f_total, data = log_cleaned)
summary (model_total_log)

## feature selection log model
m1_log <- stepAIC(model_total_log, direction = "backward")
summary(m1_log)

# remove jitter_ppq5
m2_log<- lm(total_updrs ~ age + sex + test_time + jitter + jitter_ddp + 
                 shimmer + shimmer_db + shimmer_apq5 + shimmer_apq11 + 
                 nhr + hnr + rpde + dfa + ppe, data = log_cleaned)
summary(m2_log)

# remove ppe
m3_log <- lm(formula = total_updrs ~ age + sex + test_time + jitter + jitter_ddp + 
                shimmer + shimmer_db + shimmer_apq5 + shimmer_apq11 + nhr + 
                hnr + rpde + dfa, data = log_cleaned)
summary(m3_log)

# looked at VIF 
vif(m3_log)

# remove shimmer_apq5
m4_log<- lm(formula = total_updrs ~ age + sex + test_time + jitter + jitter_ddp + 
                shimmer + shimmer_db + shimmer_apq11 + nhr + 
                hnr + rpde + dfa, data = log_cleaned)
summary(m4_log)

# remove jitter
m5_log<- lm(formula = total_updrs ~ age + sex + test_time + jitter_ddp + 
                shimmer + shimmer_db + shimmer_apq11 + nhr + 
                hnr + rpde + dfa, data = log_cleaned)
summary(m5_log)

# look at VIF again
vif(m5_log)

# remove shimmer
m6_log<- lm(formula = total_updrs ~ age + sex + test_time + jitter_ddp + 
                shimmer_db + shimmer_apq11 + nhr + 
                hnr + rpde + dfa, data = log_cleaned)
summary(m6_log)

# look at VIF again
vif(m6_log)

# remove shimmer_apq11
m7_log<- lm(formula = total_updrs ~ age + sex + test_time + jitter_ddp + 
                shimmer_db + nhr + 
                hnr + rpde + dfa, data = log_cleaned)
summary(m7_log)

# look at VIF again
vif(m7_log)

plot(m7_log)

# FINAL FIRST ORDER NO TRANSFORMATIONS
#formula = total_updrs ~ age + sex + test_time + jitter_abs + jitter_ddp + 
#          shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + dfa + ppe

# Multiple R-squared:  0.1789,	Adjusted R-squared:  0.1771 
# all variables significant, passes VIF

#  FINAL FIRST ORDER TRANSFORMATIONS
# Formula = total_updrs ~ age + sex + test_time + jitter_ddp + 
#           shimmer_db + nhr + hnr + rpde + dfa, data = log_cleaned)
# Multiple R-squared:  0.1677,	Adjusted R-squared:  0.1663 
# all variables significant, passes VIF



                              # RIDGE MODEL- NO TRANSFORMATIONS #

# grab everything besides total_updrs
all_x <- c("age", "sex", "test_time", "jitter", "jitter_abs", "jitter_rap", 
                "jitter_ppq5", "jitter_ddp", "shimmer", "shimmer_db", "shimmer_apq3", 
                "shimmer_apq5", "shimmer_apq11", "shimmer_dda", "nhr", "hnr", 
                "rpde", "dfa", "ppe")

names(pt_cleaned)
# all the rows and columns for predictors
x <- as.matrix(pt_cleaned[,all_x])
x

# all the rows and columns for total_updrs
y <- as.matrix(pt_cleaned[,4])
y

# ridge model
set.seed(123)
ridge <- cv.glmnet(x, y, family="gaussian", alpha=0)
plot(ridge)

# a=exact min lambda #
ridge_lambda_min <- ridge$lambda.min
ridge_lambda_min

# get coefficients of independent varaibles
co_ridge <- coef(ridge, s= ridge_lambda_min)
co_ridge

              
              #   make resdiduals for ridge- no transformation
# get predictors matrix from model and exclude the intcerpt column
ridge_matrix <- model.matrix(~., data = pt_cleaned[,-c(3)])[, -1]  
dim(ridge_matrix)

# predicted values
ridge_pred <- predict(ridge, newx = ridge_matrix, s = ridge_lambda_min)

# residuals
residuals_ridge <- y - ridge_pred

# head residuals
head(residuals_ridge)

# make ridge coefficents into lm coefficents so can use plot()
ridge_co <- as.vector(co_ridge[-1])
int_ridge <- co_ridge[1]  

# make linear model for ridge so can use plot()
ridge_LM <- lm(y ~ x)
ridge_LM$coefficients <- c(int_ridge, ridge_co)
summary(ridge_LM)

                    # RIDGE MODEL-TRANSFORMATIONS #

# all the rows and columns for predictors
x2 <- as.matrix(log_cleaned[,all_x])
x2

# all the rows and columns for total_updrs
y2 <- as.matrix(log_cleaned[,4])
y2

# ridge model
set.seed(123)
ridge2 <- cv.glmnet(x2, y2, family="gaussian", alpha=0)
plot(ridge2)

# a=exact min lambda #
ridge_lambda_min2 <- ridge2$lambda.min
ridge_lambda_min2

# get coefficients of independent varaibles
co_ridge2 <- coef(ridge2, s= ridge_lambda_min2)
co_ridge2

              # #   make resdiduals for ridge- transformation

# get predictors matrix from model and exclude the intcerpt column
ridge_matrix2 <- model.matrix(~., data = log_cleaned[,-c(3)])[, -1]  
dim(ridge_matrix2)

# predicted values
ridge_pred2 <- predict(ridge2, newx = ridge_matrix2, s = ridge_lambda_min2)

# residuals
residuals_ridge2 <- y2 - ridge_pred2

# head residuals
head(residuals_ridge2)

# make ridge coefficents into lm coefficents so can use plot()
ridge_co2 <- as.vector(co_ridge2[-1])
int_ridge2 <- co_ridge2[1]  

# make linear model for ridge so can use plot()
ridge_LM_log <- lm(y2 ~ x2)
ridge_LM_log$coefficients <- c(int_ridge2, ridge_co2)
summary(ridge_LM_log)


                        # LASSO MODEL- NO TRANSFORMATION #

# lasso model
set.seed(123)
lasso <- cv.glmnet(x,y,family="gaussian", alpha=1)
plot(lasso)

# a=exact min lambda #
lasso_lambda_min <- lasso$lambda.min
lasso_lambda_min 

# get coefficients of independent varaibles
co_lasso <- coef(lasso, s=ridge_lambda_min)
co_lasso

# make resdiduals for lasso

# get predictors matrix from model and exclude the intcerpt column
lasso_matrix <- model.matrix(~., data = pt_cleaned[,-c(3)])[, -1]  
dim(lasso_matrix)

# predicted values
lasso_pred <- predict(lasso, newx = lasso_matrix, s = lasso_lambda_min)

# residuals
residuals_lasso <- y - lasso_pred


# head residuals
head(residuals_lasso)

# make lasso coefficents into lm coefficents so can use plot()
lasso_co <- as.vector(co_lasso[-1])
int_lasso <- co_lasso[1]  

# make linear model for lasso so can use plot()
lasso_LM <- lm(y ~ x)
lasso_LM$coefficients <- c(int_lasso, lasso_co)
summary(lasso_LM)

                    # LASSO MODEL- TRANSFORMATION #

# lasso model
set.seed(123)
lasso2 <- cv.glmnet(x2,y2,family="gaussian", alpha=1)
plot(lasso2)

# a=exact min lambda #
lasso_lambda_min2 <- lasso2$lambda.min
lasso_lambda_min2

# get coefficients of independent varaibles
co_lasso2 <- coef(lasso2, s=ridge_lambda_min2)
co_lasso2

# make resdiduals for lasso

# get predictors matrix from model and exclude the intcerpt column
lasso_matrix2 <- model.matrix(~., data = log_cleaned[,-c(3)])[, -1]  
dim(lasso_matrix2)

# predicted values
lasso_pred2 <- predict(lasso2, newx = lasso_matrix2, s = lasso_lambda_min2)

# residuals
residuals_lasso2 <- y2 - lasso_pred2

# head residuals
head(residuals_lasso2)

# make lasso coefficents into lm coefficents so can use plot()
lasso_co2 <- as.vector(co_lasso2[-1])
int_lasso2 <- co_lasso2[1]  

# make linear model for lasso so can use plot()
lasso_LM_log <- lm(y2 ~ x2)
lasso_LM_log$coefficients <- c(int_lasso2, lasso_co2)
summary(lasso_LM_log)


                # ELASTIC NET MODEL- NO TRANSFORMATION #

# elastic net model
set.seed(123)
elasticnet <- cv.glmnet(x,y,family="gaussian", alpha=0.5)
plot(elasticnet)

# a=exact min lambda #
elastic_lambda_min <- elasticnet$lambda.min
elastic_lambda_min

# get coefficients of independent varaibles
co_elastic <- coef(elasticnet, s= elastic_lambda_min)
co_elastic
# make resdiduals for elasticnet

# get predictors matrix from model and exclude the intcerpt column
elastic_matrix <- model.matrix(~., data = pt_cleaned[,-c(3)])[, -1]  
dim(elastic_matrix)

# predicted values
elastic_pred <- predict(elasticnet, newx = elastic_matrix, s = elastic_lambda_min)

# residuals
residuals_elastic <- y - elastic_pred

# head residuals
head(residuals_elastic)

# make elastic net coefficents into lm coefficents so can use plot()
elastic_co <- as.vector(co_elastic[-1])
int_elastic <- co_elastic[1]  

# make linear model for elastic net so can use plot()
elastic_LM <- lm(y ~ x)
elastic_LM$coefficients <- c(int_elastic, elastic_co)
summary(elastic_LM)

                # ELASTIC NET MODEL- TRANSFORMATION #

# elastic net model
set.seed(123)
elasticnet2 <- cv.glmnet(x2,y2,family="gaussian", alpha=0.5)
plot(elasticnet2)

# a=exact min lambda #
elastic_lambda_min2 <- elasticnet2$lambda.min
elastic_lambda_min2

# get coefficients of independent varaibles
co_elastic2 <- coef(elasticnet2, s= elastic_lambda_min2)
co_elastic2

# make resdiduals for elasticnet

# get predictors matrix from model and exclude the intcerpt column
elastic_matrix2 <- model.matrix(~., data = log_cleaned[,-c(3)])[, -1]  
dim(elastic_matrix2)

# predicted values
elastic_pred2 <- predict(elasticnet2, newx = elastic_matrix2, s = elastic_lambda_min2)

# residuals
residuals_elastic2 <- y2 - elastic_pred2

# head residuals
head(residuals_elastic2)

# make elastic net coefficents into lm coefficents so can use plot()
elastic_co2 <- as.vector(co_elastic2[-1])
int_elastic2 <- co_elastic2[1]  

# make linear model for elastic net so can use plot()
elastic_LM_log <- lm(y2 ~ x2)
elastic_LM_log$coefficients <- c(int_elastic2, elastic_co2)
summary(elastic_LM_log)

                  # SECOND ORDER MODELS- NO TRANSFORMATIONS #
# second order with just age squared- age^2 is significant
m2_SQage <- lm(total_updrs ~ age + I(age^2), data = pt_cleaned)
summary(m2_SQage)

# second order with just sex squared- sex^2 is not significant
m2_SQsex <- lm(total_updrs ~ sex + I(sex^2), data = pt_cleaned)
summary(m2_SQsex)

# second order with just test_time squared- test_time^2 is not significant
m2_SQtest_time <- lm(total_updrs ~ test_time + I(test_time^2), data = pt_cleaned)
summary(m2_SQtest_time)

# second order with just jitter_abs squared- jitter_abs^2 - significant 
m2_SQjitter_abs <- lm(total_updrs ~ jitter_abs + I(jitter_abs^2), data = pt_cleaned)
summary(m2_SQjitter_abs)

# second order with just age squared- jitter_ddp^2 is significant
m2_SQjitter_ddp <- lm(total_updrs ~ jitter_ddp + I(jitter_ddp^2), data = pt_cleaned)
summary(m2_SQjitter_ddp)

# second order with just age squared- shimmer_apq3^2 is significant
m2_SQshimmer_apq3 <- lm(total_updrs ~ shimmer_apq3 + I(shimmer_apq3^2), data = pt_cleaned)
summary(m2_SQshimmer_apq3)

# second order with just shimmer_apq11 squared- shimmer_apq11^2 is significant
m2_SQshimmer_apq11 <- lm(total_updrs ~ shimmer_apq11 + I(shimmer_apq11^2), data = pt_cleaned)
summary(m2_SQshimmer_apq11)

# second order with just nhr squared- nhr^2 is significant
m2_SQnhr <- lm(total_updrs ~ nhr + I(nhr^2), data = pt_cleaned)
summary(m2_SQnhr)

# second order with just hnr squared- hnr^2 is significant
m2_SQhnr <- lm(total_updrs ~ hnr + I(hnr^2), data = pt_cleaned)
summary(m2_SQhnr)

# second order with just rpde squared- rpde^2 is significant
m2_SQrpde <- lm(total_updrs ~ rpde + I(rpde^2), data = pt_cleaned)
summary(m2_SQrpde)

# second order with just dfa squared- dfa^2 is significant
m2_SQdfa <- lm(total_updrs ~ dfa + I(dfa^2), data = pt_cleaned)
summary(m2_SQdfa)

# second order with just ppe squared- ppe^2 is significant
m2_SQppe <- lm(total_updrs ~ ppe + I(ppe^2), data = pt_cleaned)
summary(m2_SQppe)

# combination of those that were significant
m2_SQcombo <- lm(total_updrs ~ age + sex + test_time + jitter_abs + 
                jitter_ddp +  shimmer_apq3 + shimmer_apq11 + nhr + hnr + 
                rpde + dfa + ppe + I(age^2) + I(jitter_abs^2) + I(jitter_ddp^2)+ 
                I(shimmer_apq3^2) + I(shimmer_apq11^2) + I(nhr^2) + I(hnr^2)
                + I(rpde^2)+ I(dfa^2)+ I(ppe^2), data = pt_cleaned)
summary(m2_SQcombo)

# remove I(rpde^2)
m2_SQcombo <- lm(total_updrs ~ age + sex + test_time + jitter_abs + 
                   jitter_ddp +  shimmer_apq3 + shimmer_apq11 + nhr + hnr + 
                   rpde + dfa + ppe + I(jitter_abs^2) + I(jitter_ddp^2)+ 
                   I(shimmer_apq3^2) + I(shimmer_apq11^2) + I(nhr^2) + I(hnr^2)
                 + I(dfa^2)+ I(ppe^2), data = pt_cleaned)
summary(m2_SQcombo)

vif(m2_SQcombo)

# second order will be too similar due to the nature of terms
# will try complete second order

library(ggplot2)
ggplot(pt_cleaned, aes(x = test_time + I(test_time^2), y = total_updrs)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
      labs( x = "Test Time", y = "Total UPDRS Score", title = "Scatter Plot with Second-Order Model")+
      theme_minimal()


# Complete Second Order Model #
m_Fullsecond <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
                 jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
                 dfa + ppe + 
                 I(age^2) + I(sex^2) + I(test_time^2) + I(jitter_abs^2) + 
                 I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
                 I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
                 age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
                 age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
                 age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
                 sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:nhr + 
                 sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
                 test_time:jitter_ddp + test_time:shimmer_apq3 + test_time:shimmer_apq11 + 
                 test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
                 test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
                 jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
                 jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3 + 
                 jitter_ddp:shimmer_apq11 + jitter_ddp:nhr + jitter_ddp:hnr + 
                 jitter_ddp:rpde + jitter_ddp:dfa + jitter_ddp:ppe + shimmer_apq3:shimmer_apq11 + 
                 shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
                 shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
                 shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
                 shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
                 hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
               data = pt_cleaned)
summary(m_Fullsecond)

# removed sex^2
m_Fs2 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:nhr + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + test_time:shimmer_apq11 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3 + 
             jitter_ddp:shimmer_apq11 + jitter_ddp:nhr + jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + jitter_ddp:ppe + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
summary(m_Fs2)

# removed jitter_ddp:ppe
m_Fs3 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:nhr + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + test_time:shimmer_apq11 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3 + 
             jitter_ddp:shimmer_apq11 + jitter_ddp:nhr + jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
summary(m_Fs3)

# removed jitter_ddp:shimmer_apq11 
m_Fs4 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:nhr + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + test_time:shimmer_apq11 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
             jitter_ddp:nhr + jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
summary(m_Fs4)                       

# removed test_time:shimmer_apq11
m_Fs5 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:nhr + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
             jitter_ddp:nhr + jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
  summary(m_Fs5) 

# removed jitter_ddp:nhr
m_Fs6 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:nhr + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
             jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
  summary(m_Fs6) 

# removed sex:nhr  
m_Fs7 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:jitter_ddp + sex:shimmer_apq3 + sex:shimmer_apq11 + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
             jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
  summary(m_Fs7) 

# removed sex:jitter_ddp
m_Fs8 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
             jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
             dfa + ppe + 
             I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
             I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
             I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
             age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
             age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
             age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
             sex:shimmer_apq3 + sex:shimmer_apq11 + 
             sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
             test_time:jitter_ddp + test_time:shimmer_apq3 + 
             test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
             test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
             jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
             jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
             jitter_ddp:hnr + 
             jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
             shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
             shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
             shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
             shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
             hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
           data = pt_cleaned)
  summary(m_Fs8) 

# removed I(nhr^2) 
m_Fs9 <-  lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              shimmer_apq11:ppe + nhr:hnr + nhr:rpde + nhr:dfa + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs9) 


# removed hnr:rpde 
m_Fs10 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:jitter_ddp + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              shimmer_apq11:ppe + nhr:hnr + nhr:dfa + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs10) 

# removed jitter_abs:jitter_ddp
m_Fs11 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:nhr + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              shimmer_apq11:ppe + nhr:hnr + nhr:dfa + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs11) 

# removed jitter_abs:nhr
m_Fs12 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              shimmer_apq11:ppe + nhr:hnr + nhr:dfa + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs12) 

# removed nhr:dfa
m_Fs13 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              shimmer_apq11:ppe + nhr:hnr + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs13) 

# removed shimmer_apq11:ppe
m_Fs14 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:test_time + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs14) 

# removed sex:test_time
m_Fs15 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:hnr + 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs15) 

# removed jitter_ddp:hnr 
m_Fs16 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:rpde + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs16) 

# removed hnr:rpde
m_Fs17 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs17) 

# removed shimmer_apq11:hnr 
m_Fs18 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:hnr + shimmer_apq11:rpde + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
summary(m_Fs18) 

# removed shimmer_apq11:hnr  
m_Fs19 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:rpde + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs19) 

# removed shimmer_apq11:rpde
m_Fs20 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(jitter_ddp^2) + I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs20) 

# removed I(jitter_ddp^2) 
m_Fs21 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:dfa + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
summary(m_Fs21) 

# removed sex:dfa 
m_Fs22 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:test_time + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs22) 

# removed age:test_time 
m_Fs23 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs23) 

# removed
m_Fs24 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:nhr + test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs24) 

# removed test_time:hnr
m_Fs25 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:shimmer_apq3 + 
              test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs25) 

# removed test_time:shimmer_apq3  
m_Fs26 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) + I(jitter_abs^2) + 
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + 
              test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs26) 

# removed I(jitter_abs^2) 
m_Fs27 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) +
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr + age:hnr + 
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + 
              test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs27) 

# removed age:hnr                    
m_Fs28 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) +
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + I(ppe^2) +
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr +
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + 
              test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs28) 

# removed I(ppe^2)  
m_Fs29 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + 
              I(age^2) + I(test_time^2) +
              I(shimmer_apq3^2) + I(shimmer_apq11^2) + 
              I(hnr^2) + I(rpde^2) + I(dfa^2) + 
              age:sex + age:jitter_abs + age:jitter_ddp + 
              age:shimmer_apq3 + age:shimmer_apq11 + age:nhr +
              age:rpde + age:dfa + age:ppe + sex:jitter_abs + 
              sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + 
              test_time:hnr + test_time:rpde + test_time:dfa + 
              test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + 
              jitter_abs:rpde + jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:rpde + jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + 
              shimmer_apq3:nhr + shimmer_apq3:hnr + shimmer_apq3:rpde + 
              shimmer_apq3:dfa + shimmer_apq3:ppe + shimmer_apq11:nhr + 
              shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + 
              hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + dfa:ppe, 
            data = pt_cleaned)
  summary(m_Fs29) 

# removed jitter_abs:hnr
m_Fs30 <- lm(formula = total_updrs ~ age + sex + test_time + jitter_abs + 
    jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
    dfa + ppe + I(age^2) + I(test_time^2) + I(shimmer_apq3^2) + 
    I(shimmer_apq11^2) + I(hnr^2) + I(dfa^2) + age:sex + age:jitter_abs + 
    age:jitter_ddp + age:nhr + age:rpde + age:dfa + age:ppe + 
    sex:jitter_abs + sex:shimmer_apq3 + sex:shimmer_apq11 + sex:hnr + 
    sex:ppe + test_time:jitter_abs + test_time:jitter_ddp + test_time:hnr + 
    test_time:rpde + test_time:ppe + jitter_abs:shimmer_apq3 + 
    jitter_abs:shimmer_apq11 + jitter_abs:rpde + jitter_abs:dfa + 
    jitter_abs:ppe + jitter_ddp:shimmer_apq3 + jitter_ddp:dfa + 
    shimmer_apq3:shimmer_apq11 + shimmer_apq3:nhr + shimmer_apq3:hnr + 
    shimmer_apq3:rpde + shimmer_apq3:dfa + shimmer_apq3:ppe + 
    shimmer_apq11:dfa + nhr:hnr + nhr:ppe + hnr:dfa + hnr:ppe + 
    rpde:dfa + dfa:ppe, data = pt_cleaned)

  summary(m_Fs30)
  
vif(m_Fs30)
# same issue will not work with second order terms to similar
# also will overfit

                    # SECOND ORDER W/ TRANSFORMATIONS #
# complete second order with transformations
m_fullSecond_log <- lm(formula = total_updrs ~ age + sex + test_time + jitter_ddp + 
                    shimmer_db + nhr + hnr + rpde + dfa + age:sex + age:test_time + 
                    age:jitter_ddp + age:shimmer_db + age:nhr + age:hnr + age:rpde + 
                    age:dfa + sex:test_time + sex:jitter_ddp + sex:shimmer_db + 
                    sex:nhr + sex:hnr + sex:rpde + sex:dfa + test_time:jitter_ddp +
                    test_time:shimmer_db + test_time:nhr + test_time:hnr + test_time:rpde +
                    test_time:dfa +  jitter_ddp:shimmer_db + jitter_ddp:nhr + 
                    jitter_ddp:hnr + jitter_ddp:rpde + jitter_ddp:dfa + shimmer_db:nhr + 
                    shimmer_db:hnr + shimmer_db:rpde + shimmer_db:dfa + nhr:hnr + 
                    nhr:rpde + nhr:dfa + hnr:rpde + hnr:dfa + rpde:dfa + I(age^2) + 
                    I(sex^2) + I(test_time^2) + I(jitter_ddp^2) + I(shimmer_db^2) + 
                    I(nhr^2) + I(hnr^2) + I(rpde^2) + I(dfa^2), data = log_cleaned)
summary(m_fullSecond_log)

# removed sex2, sex:test_time, test_time:nhr, jitter_ddp:hnr, jitter_ddp:hnr, 
# age:hnr, I(dfa^2), I(test_time^2), I(shimmer_db^2), I(rpde^2), sex:jitter_ddp,
# sex:rpde, age:test_time, nhr:dfa, age:shimmer_db, test_time:hnr, age:jitter_ddp,
# age:dfa, I(jitter_ddp^2), shimmer_db:nhr, jitter_ddp:rpde, nhr:rpde, test_time:jitter_ddp,
# test_time:dfa
m_fullSecond_log <- lm(formula = total_updrs ~ age + sex + test_time + jitter_ddp + 
                    shimmer_db + nhr + hnr + rpde + dfa + age:sex + age:rpde + 
                    sex:shimmer_db + sex:nhr + sex:hnr + sex:dfa + 
                    test_time:shimmer_db + test_time:rpde +  
                    jitter_ddp:shimmer_db +  jitter_ddp:dfa +
                    shimmer_db:hnr + shimmer_db:rpde + shimmer_db:dfa + nhr:hnr + 
                    hnr:rpde + hnr:dfa + rpde:dfa + I(age^2) + 
                    I(nhr^2) + I(hnr^2), data = log_cleaned)
summary(m_fullSecond_log)

vif(m_fullSecond_log)
# multicollinearity is again an issue even with log transformations


# code block 21
                # MAKE A TEST/ TRAIN SPLIT's #
# make another dummy df
pt_cleaned_tree <- pt_cleaned

names(pt_cleaned_tree)


# make training and testing subsets
set.seed(12)  # for reproducibility
partition <- sample(2, nrow(pt_cleaned_tree), replace = TRUE, prob = c(0.70, 0.30))
train <- pt_cleaned_tree[partition==1 ,]
test <- pt_cleaned_tree[partition==2 ,]

                   # Decision Tree MODELs #

                        # Decision Tree - no transformations #

# make specs for decision tree model
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# fit the model to the training data
tree_fit <- tree_spec %>%
  fit(f_total, data = train)
  
# create predictions for testing data
predictions <- tree_fit %>%
  predict(test) %>%
  pull(.pred)

# make specs for metrics to look at RMSE and R-squared
metrics <- metric_set(rmse, mae, rsq)

# model performace motor_updrs
model_performance <- test %>%
  mutate(predictions = predictions) %>%
  metrics(truth = total_updrs, estimate = predictions)

print(model_performance)

# code block 22

# PNG to get picture of decision tree #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/DECISIONTREE_TOTAL.png", 
    width = 1500, height = 950)
# plot the decision tree- total_updrs
rpart.plot(tree_fit$fit, 
           type = 4,
           roundint = FALSE,
           extra = 101, 
           under = TRUE, 
           cex = 0.8, 
           box.palette = "auto")

# close the PNG #
dev.off()
graphics.off()

# create a variable importance plot
var_importance <- vip::vip(tree_fit, num_features = 20)
print(var_importance)


# removed items based on insignificance to model

# run 1 with all variables
# Run 2 without shimmer_apq11
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -shimmer_apq11)
# run 3 without test_time
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -test_time)

# run 4 without shimmer_db
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -shimmer_db)

# run 5 without shimmer_dda
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -shimmer_dda)

# run 6 without shimmer
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -shimmer)

# run 7 without shimmer_apq5
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -shimmer_apq5)

# run 8 without shimmer_apq3
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -shimmer_apq3)

# run 9 without nhr
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -nhr)

# run 10 without ppe
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -ppe)

# run 11 without jitter_ddp
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -jitter_ddp)

# run 12 without rpde
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -rpde)

# run 13 without jitter_rap
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -jitter_rap)

# run 14 without jitter
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -jitter)

# run 15 without jitter_ppq5
pt_cleaned_tree <- subset(pt_cleaned_tree, select = -jitter_ppq5)

# # run 16 without sex 
#pt_cleaned_tree <- subset(pt_cleaned_tree, select = -sex)


# First run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687, 
# Second run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
# Third run  #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
# fourth run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
# fifth run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
# sixth run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
# seven run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
# 8th run #1 rmse 5.57,  2 mae 4.13 , 3 rsq  0.687
#9th run #1 rmse 5.60,  2 mae 4.16, 3 rsq  0.684, 
# 10th run #1 rmse 5.60,  2 mae 4.16, 3 rsq  0.684
# 11th run #1 rmse 5.60,  2 mae 4.16, 3 rsq  0.684
# 12th run #1 rmse 5.60,  2 mae 4.16, 3 rsq  0.684
# 13th run #1 rmse 4.78,  2 mae 3.45, 3 rsq 0.734
# 14th run #1 rmse 4.78,  2 mae 3.45, 3 rsq  0.770
# 15th run  #1 rmse 4.78,  2 mae 3.45, 3 rsq  0.770
# 16th run  #1 rmse 5.64,  2 mae 4.24, 3 rsq  0.679


# make chart of data
deletion_run1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
rmse_tree1 <- c(5.57, 5.57, 5.57, 5.57, 5.57, 5.57, 5.57, 5.57, 5.60, 5.60, 5.60, 5.60, 4.78, 4.78, 4.78, 5.64)
mae_tree1 <- c(4.13, 4.13, 4.13, 4.13, 4.13, 4.13, 4.13, 4.13, 4.16, 4.16, 4.16, 4.16, 3.45, 3.45, 3.45, 4.24)
rsq_tree1 <- c(0.687, 0.687, 0.687, 0.687, 0.687, 0.687, 0.687, 0.687, 0.684, 0.684, 0.684, 0.684, 0.734, 0.770, 0.770, 0.679)
variables = c("All Variables", "Without shimmer_apq11", "Without test_time", "Without shimmer_db", 
                "Without shimmer_dda", "Without shimmer", "Without shimmer_apq5", "Without shimmer_apq3", 
                "Without nhr", "Without ppe", "Without jitter_ddp", "Without rpde", "Without jitter_rap", "Without jitter",
                "Without jitter_ppq5", "Without sex")
                
# Create a data frame
RESULTS_NT <- data.frame(variables, deletion_run1, rmse_tree1, mae_tree1, rsq_tree1)
print(RESULTS_NT)

# make df to see predicted vs actual plot for decision tree
predict_df <- tibble(actual = test$total_updrs, predicted = predictions)

# make plot with ggplot
ggplot(predict_df, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  
  labs(x = "Actual", y = "Predicted", title = "Predicted vs Actual Plot for Decision Tree Model")


# code block 24

                      # Decision Tree - transformations #

# make another dummy df
log_cleaned_tree <- log_cleaned

# make training and testing subsets
set.seed(123)  # for reproducibility
partition2 <- sample(2, nrow(log_cleaned_tree), replace = TRUE, prob = c(0.70, 0.30))
train2 <- log_cleaned_tree[partition2==1 ,]
test2 <- log_cleaned_tree[partition2==2 ,]

# make specs for decision tree model
tree_spec2 <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# fit the model to the training data
tree_fit2 <- tree_spec2 %>%
  fit(f_total, data = train2)

# create predictions for testing data
predictions2 <- tree_fit2 %>%
  predict(test2) %>%
  pull(.pred)


# make specs for metrics to look at RMSE and R-squared
metrics <- metric_set(rmse, mae, rsq)

# model performace motor_updrs
model_performance2 <- test2 %>%
  mutate(predictions2 = predictions2) %>%
  metrics(truth = total_updrs, estimate = predictions2)

print(model_performance2)

# code block 25

# PNG to get picture of scatterplot matrix #
png("/Users/kylekriho/Documents/DATA ANALYSIS AND REGRESSION - 182024 /FInal Project/PICTURES/DECISIONTREE_TOTAL_log.png", 
    width = 1500, height = 950)
# plot the decision tree- total_updrs 
rpart.plot(tree_fit2$fit, 
           type = 4,
           roundint = FALSE,
           extra = 101, 
           under = TRUE, 
           cex = 0.8, 
           box.palette = "auto")

# close the PNG #
dev.off()
graphics.off()

# create a variable importance plot
var_importance2 <- vip::vip(tree_fit2, num_features = 20)
print(var_importance2)

# code block 26

# removed items based on insignificance to model

# age          dfa          hnr          rpde         sex          shimmer_apq3

# run 1 with all variables
# run 2 without test_time
log_cleaned_tree <- subset(log_cleaned_tree, select = -test_time)
# run 3 without nhr
log_cleaned_tree <- subset(log_cleaned_tree, select = -nhr)
# run 4 without jitter_ddp
log_cleaned_tree <- subset(log_cleaned_tree, select = -jitter_ddp)
# run 5 without shimmer_apq11
log_cleaned_tree <- subset(log_cleaned_tree, select = -shimmer_apq11)
# run 6 without shimmer_dda
log_cleaned_tree <- subset(log_cleaned_tree, select = -shimmer_dda)
# run 7 without jitter_rap
log_cleaned_tree <- subset(log_cleaned_tree, select = -jitter_rap)
# run 8 without shimmer_db
log_cleaned_tree <- subset(log_cleaned_tree, select = -shimmer_db)
# run 9 without shimmer_apq3
log_cleaned_tree <- subset(log_cleaned_tree, select = -shimmer_apq3)
# run 10 without shimmer_apq5
log_cleaned_tree <- subset(log_cleaned_tree, select = -shimmer_apq5)
# run 11 without shimmer
log_cleaned_tree <- subset(log_cleaned_tree, select = -shimmer)
# run 12 without ppe
log_cleaned_tree <- subset(log_cleaned_tree, select = -ppe)
# run 13 without jitter
log_cleaned_tree <- subset(log_cleaned_tree, select = -jitter)
# run 14 without jitter_ppq5
log_cleaned_tree <- subset(log_cleaned_tree, select = -jitter_ppq5)
# run 15 without hnr
log_cleaned_tree <- subset(log_cleaned_tree, select = -hnr)
# run 16 without rpde
log_cleaned_tree <- subset(log_cleaned_tree, select = -rpde)
# run 17 without sex
#log_cleaned_tree <- subset(log_cleaned_tree, select = -sex)


# First run #1 rmse 5.44,  2 mae 3.86, 3 rsq  0.691
# Second run #1 rmse 5.44,  2 mae 3.86, 3 rsq  0.691
# Third run  #1 rmse 5.23,  2 mae 3.73, 3 rsq  0.714
# fourth run #1 rmse 5.23,  2 mae 3.73, 3 rsq  0.714
# fifth run #1 rmse 5.23,  2 mae 3.73, 3 rsq  0.714 - 
# sixth run #1 rmse 5.23,  2 mae 3.73, 3 rsq  0.714
# seven run #1 rmse 5.23,  2 mae 3.73, 3 rsq  0.714
# 8th run #1 rmse 5.23,  2 mae 3.73, 3 rsq  0.714
# 9th run #1 rmse 4.84,  2 mae 3.42 3 rsq  0.756
# 10th run #1 rmse 4.84,  2 mae 3.42 3 rsq  0.756
# 11th run #1 rmse 4.84,  2 mae 3.42 3 rsq  0.756 
# 12th run #1 rmse 4.84,  2 mae 3.42 3 rsq  0.756 
# 13th run #1 rmse 4.84,  2 mae 3.42 3 rsq  0.756 
# 14th run #1 rmse 4.84,  2 mae 3.42 3 rsq  0.756 
# 15th run #1 rmse 4.55,  2 mae 3.21, 3 rsq  0.784
# 16th run #1 rmse 4.41,  2 mae 3.14, 3 rsq  0.797
# 17th run #1 rmse 5.64,  2 mae 4.14, 3 rsq  0.668

# make chart of data
deletion_run2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
rmse_tree2 <- c(5.44, 5.44, 5.23, 5.23, 5.23, 5.23, 5.23, 5.23, 4.84, 4.84, 4.84, 4.84, 4.84, 4.84, 4.55, 4.41, 5.64)
mae_tree2 <- c(3.86, 3.86, 3.73, 3.73, 3.73, 3.73, 3.73, 3.73, 3.42, 3.42, 3.42, 3.42, 3.42, 3.42, 3.21, 3.14, 4.14)
rsq_tree2 <- c(0.691, 0.691, 0.714, 0.714, 0.714, 0.714, 0.714, 0.714, 0.756, 0.756, 0.756, 0.756, 0.756, 0.756, 0.784, 0.797, 0.668)
variables2 <- c("All Variables", "Without test_time", "Without nhr", "Without jitter_ddp", 
               "Without shimmer_apq11", "Without shimmer_dda", "Without jitter_rap", 
               "Without shimmer_db", "Without shimmer_apq3", "Without shimmer_apq5", 
               "Without shimmer", "Without ppe", "Without jitter", "Without jitter_ppq5", 
               "Without hnr", "Without rpde", "Without sex")
# Create a data frame
RESULTS_Log <- data.frame(variables2, deletion_run2, rmse_tree2, mae_tree2, rsq_tree2)
print(RESULTS_Log)

# code block 27
View(pt_cleaned$total_updrs)
# make df to see predicted vs actual plot for decision tree
predict_df2 <- tibble(actual = test2$total_updrs, predicted = predictions2)

# make plot with ggplot
ggplot(predict_df2, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  
  labs(x = "Actual", y = "Predicted", title = "Predicted vs Actual Plot for Decision Tree Model_log")
  
# code block 28

                      ##### RESIDUALS ANALYSIS #####
                      
                        # First Order Final Model- NO TRANSFORMATIONS #

# plot residuals for 1st
residuals_first = m4$residuals

plot(residuals_first,
     main = "First Order Residuals Plot")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(m4)

                      # First Order Final Model- TRANSFORMATIONS #

# plot residuals for 1st
residuals_first_log = m7_log$residuals

plot(residuals_first_log,
     main = "First Order Residuals Plot_log")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(m7_log)

# code block 29

                      # SECOND ORDER FINAL MODEL_ NO TRANSFORMATIONS #

# plot residuals for 2nd
residuals_second = m_Fs30$residuals

plot(residuals_second,
     main = "Second Order Residuals Plot")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(m_Fs30)

                   # SECOND ORDER FINAL MODEL_ TRANSFORMATIONS #

# plot residuals for 2nd
residuals_second_log = m_fullSecond_log$residuals

plot(residuals_second_log,
     main = "Second Order Residuals Plot_log")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(m_fullSecond_log)

# code block 30

                                # RIDGE- NO TRANSFORMATIONS #

# plot residuals for ridge
plot(residuals_ridge,
     main = "Ridge Residuals Plot")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(ridge_LM)

# histogram of residuals - normality plot
hist(residuals_ridge, 
    breaks = 20, 
    xlab = "Residuals", 
    main = "Histogram of Residuals of Ridge Model")

                          # RIDGE-TRANSFORMATIONS #

# plot residuals for ridge
plot(residuals_ridge2,
     main = "Ridge Residuals Plot_log")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(ridge_LM_log)

# histogram of residuals - normality plot
hist(residuals_ridge2, 
    breaks = 20, 
    xlab = "Residuals", 
    main = "Histogram of Residuals of Ridge Model_log")

# code block 31

                                # LASSO- NO TRANSFORMATIONS #
# plot residuals for lasso
plot(residuals_lasso,
     main = "Lasso Residuals Plot")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(lasso_LM)

# histogram of residuals - normality plot
hist(residuals_lasso, 
    breaks = 20, 
    xlab = "Residuals", 
    main = "Histogram of Residuals of Lasso Model")

                        # LASSO- TRANSFORMATIONS #
# plot residuals for lasso
plot(residuals_lasso2,
     main = "Lasso Residuals Plot_log")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(lasso_LM_log)


# histogram of residuals - normality plot
hist(residuals_lasso2, 
    breaks = 20, 
    xlab = "Residuals", 
    main = "Histogram of Residuals of Lasso Model_log")
    
# code block 32

                              # ELASTIC NET- NO TRANSFORMATIONS #

# plot residuals for elastic net
plot(residuals_elastic,
     main = "Elastic Net Residuals Plot")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(elastic_LM)

# histogram of residuals - normality plot
hist(residuals_elastic, 
    breaks = 20, 
    xlab = "Residuals", 
    main = "Histogram of Residuals of Elastic Net Model")

                        # ELASTIC NET-TRANSFORMATIONS #

# plot residuals for elastic net
plot(residuals_elastic2,
     main = "Elastic Net Residuals Plot_log")
abline(h = 0, col = "red", lty = 3)

# look at assumptions of line
plot(elastic_LM_log)

# histogram of residuals - normality plot
hist(residuals_elastic2, 
    breaks = 20, 
    xlab = "Residuals", 
    main = "Histogram of Residuals of Elastic Net Model_log")



                     #### CROSS VALIDATION ####

                        # First Order Model- NO TRANSFORMATIONS #

# look at summary of final model 1 and pulled out info
summary(m4)

# formula
first_order_formula <- total_updrs ~ age + sex + test_time + jitter_abs + jitter_ddp + 
                       shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + dfa + ppe

# reproducibility
set.seed(123)

#  make training variable with 10 folds for cross validation
train_control <- trainControl(method = "cv", number = 10)

#  train model
first_nonTran <- train(first_order_formula, data = pt_cleaned, method = "lm", trControl = train_control)
first_nonTran$results

# find rmse & mae in model
rmse_1st <- sqrt(mean(m4$residuals^2))
rmse_1st
mae_1st <- mean(abs(m4$residuals))
mae_1st

# CV- RMSE 8.980171, Rsquared 0.1765, MAE 7.508677
# model- RMSE 8.961153, Rsquared 0.1789, MAE 7.491989

# make a dataframe of cross validation vs Model results
results_1st_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(8.980171, 0.1765, 7.508677),
  Model = c(8.961153, 0.1789, 7.491989))
  
results_1st_valid


                    # First Order Model- TRANSFORMATIONS #
# summary of formula
summary(m7_log)

# formula
first_order_formula2 <- total_updrs ~ age + sex + test_time + jitter_ddp + 
                      shimmer_db + nhr + hnr + rpde + dfa
                      
# reproducibility
set.seed(123)
#  make training variable with 10 folds for cross validation
train_control <- trainControl(method = "cv", number = 10)

#  train model
first_Trainlog <- train(first_order_formula2, data = log_cleaned, method = "lm", trControl = train_control)
first_Trainlog$results

# find rmse & mae in model
rmse_1st_log <- sqrt(mean(m7_log$residuals^2))
rmse_1st_log
mae_1st_log <- mean(abs(m7_log$residuals))
mae_1st_log
#       RMSE      Rsquared    MAE 
# CV-  9.031614, 0.1677282, 7.498887
# model- 9.022158, 0.1677, 7.488174

# make a dataframe of cross validation vs Model results
results_1stLOG_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.031614, 0.1677, 7.498887),
  Model = c(9.022158, 0.1677, 7.488174))
results_1stLOG_valid

                        # Second Order Model - NO TRANSFORMATIONS#

# summary of formula
summary(m_Fs30)

# formula
second_order_formula <- total_updrs ~ age + sex + test_time + jitter_abs + 
              jitter_ddp + shimmer_apq3 + shimmer_apq11 + nhr + hnr + rpde + 
              dfa + ppe + I(age^2) + I(test_time^2) + I(shimmer_apq3^2) + 
              I(shimmer_apq11^2) + I(hnr^2) + I(rpde^2) + I(dfa^2) + 
              age:sex + age:jitter_abs + age:jitter_ddp + age:shimmer_apq3 + 
              age:shimmer_apq11 + age:nhr + age:rpde + age:dfa + age:ppe + 
              sex:jitter_abs + sex:shimmer_apq3 + sex:shimmer_apq11 + 
              sex:hnr + sex:rpde + sex:ppe + test_time:jitter_abs + 
              test_time:jitter_ddp + test_time:hnr + test_time:rpde + 
              test_time:dfa + test_time:ppe + jitter_abs:shimmer_apq3 + 
              jitter_abs:shimmer_apq11 + jitter_abs:hnr + jitter_abs:rpde + 
              jitter_abs:dfa + jitter_abs:ppe + jitter_ddp:shimmer_apq3+ 
              jitter_ddp:dfa + shimmer_apq3:shimmer_apq11 + shimmer_apq3:nhr + 
              shimmer_apq3:hnr + shimmer_apq3:rpde + shimmer_apq3:dfa + 
              shimmer_apq3:ppe + shimmer_apq11:nhr + shimmer_apq11:dfa + 
              nhr:hnr + nhr:ppe + hnr:dfa + hnr:ppe + rpde:dfa + rpde:ppe + 
              dfa:ppe
              
# reproducibility
set.seed(123)
#  make training variable with 10 folds for cross validation
train_control <- trainControl(method = "cv", number = 10)

#  train model
second_Train <- train(second_order_formula, data = pt_cleaned, method = "lm", trControl = train_control)
second_Train$results

# find rmse & mae in model
rmse_2nd <- sqrt(mean(m_Fs30$residuals^2))
rmse_2nd
mae_2nd <- mean(abs(m_Fs30$residuals))
mae_2nd
#       RMSE      Rsquared    MAE 
# CV-  8.31507, 0.2945068, 6.785001
# model- 8.23493, 0.3066, 6.71064

# make a dataframe of cross validation vs Model results
results_2nd_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(8.31507, 0.2945068, 6.785001),
  Model = c(8.23493, 0.3066, 6.71064))
results_2nd_valid

                    # Second Order Model - TRANSFORMATIONS#
                    
# summary of model                    
summary(m_fullSecond_log)   

# formula
second_order_formula2 <- total_updrs ~ age + sex + test_time + jitter_ddp + 
                    shimmer_db + nhr + hnr + rpde + dfa + age:sex + age:rpde + 
                    sex:shimmer_db + sex:nhr + sex:hnr + sex:dfa + 
                    test_time:shimmer_db + test_time:rpde +  
                    jitter_ddp:shimmer_db +  jitter_ddp:dfa +
                    shimmer_db:hnr + shimmer_db:rpde + shimmer_db:dfa + nhr:hnr + 
                    hnr:rpde + hnr:dfa + rpde:dfa + I(age^2) + 
                    I(nhr^2) + I(hnr^2)
                    
# reproducibility
set.seed(123)

#  make training variable with 10 folds for cross validation
train_control <- trainControl(method = "cv", number = 10)

#  train model
second_Trainlog <- train(second_order_formula2, data = log_cleaned, method = "lm", trControl = train_control)

# look at model results
print(second_Trainlog$results)

# find rmse & mae in model
rmse_2nd_log <- sqrt(mean(m_fullSecond_log$residuals^2))
rmse_2nd_log
mae_2nd_log <- mean(abs(m_fullSecond_log$residuals))
mae_2nd_log
#       RMSE      Rsquared    MAE 
# CV-  8.565469, 0.2504, 6.973048
# model- 8.523265, 0.2572, 6.933304

# make a dataframe of cross validation vs Model results
results_2ndLOG_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(8.565469, 0.2504437, 6.973048),
  Model = c(8.523265, 0.2572, 6.933304))
results_2ndLOG_valid

                              # Ridge- NO TRANSFORMATIONS #

# look at model
summary(ridge_LM)

# find rmse & mae in model
rmse_ridge <- sqrt(mean(ridge_LM$residuals^2))
rmse_ridge
mae_ridge <- mean(abs(ridge_LM$residuals))
mae_ridge

# reproducibility
set.seed(123)

# cross-validation fit for ridge model
ridge_cv <- cv.glmnet(x, y, family = "gaussian", alpha = 0, nfolds = 10)

# find best lambda 
lambda_best_ridge <- ridge_cv$lambda.min
lambda_best_ridge

# predicted values
predictedValues_ridge <- predict(ridge, newx = x)

#  rmse cross validation result
ridge_rmse <- sqrt(mean((y - predictedValues_ridge)^2))
ridge_rmse

# r2 cross validation result
ridge_r_squared <- 1 - (sum((y - predictedValues_ridge)^2) / sum((y - mean(y))^2))
ridge_r_squared

# mae cross validation result
ridge_mae <- mean(abs(y - predictedValues_ridge))
ridge_mae
#       RMSE      Rsquared    MAE 
# CV-  9.074624, 0.1580, 7.560337
# model- 8.942655, 0.1823, 7.471322

# make a dataframe of cross validation vs Model results
results_ridge_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.074624, 0.1580, 7.560337),
  Model = c(8.942655, 0.1823, 7.471322))
results_ridge_valid


                      # Ridge- TRANSFORMATIONS #

# look at model
summary(ridge_LM_log)

# find rmse & mae in model
rmse_ridge_log <- sqrt(mean(ridge_LM_log$residuals^2))
rmse_ridge_log
mae_ridge_log <- mean(abs(ridge_LM_log$residuals))
mae_ridge_log
                              
# reproducibility
set.seed(123)

# cross-validation fit for ridge model
ridge_cv2 <- cv.glmnet(x2, y2, family = "gaussian", alpha = 0, nfolds = 10)

# find best lambda 
lambda_best_ridge2 <- ridge_cv2$lambda.min
lambda_best_ridge2

# predicted values
predictedValues_ridge2 <- predict(ridge2, newx = x2)

# rmse cross validation result
ridge_rmse2 <- sqrt(mean((y2 - predictedValues_ridge2)^2))
ridge_rmse2

# r2 cross validation result
ridge_r_squared2 <- 1 - (sum((y2 - predictedValues_ridge2)^2) / sum((y2 - mean(y2))^2))
ridge_r_squared2

# mae cross validation result
ridge_mae2 <- mean(abs(y2 - predictedValues_ridge2))
ridge_mae2

#       RMSE      Rsquared    MAE 
# CV-  9.074183, 0.1580, 7.563103
# model- 8.937728, 0.1823, 7.398654

# make a dataframe of cross validation vs Model results
results_ridgeLOG_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.074183, 0.1580, 7.563103),
  Model = c(8.523265, 0.2572, 6.933304))
results_ridgeLOG_valid

                              # Lasso- NO TRANSFORMATIONS #

# look at model
summary(lasso_LM)

# find rmse & mae in model
rmse_lasso <- sqrt(mean(lasso_LM$residuals^2))
rmse_lasso
mae_lasso <- mean(abs(lasso_LM$residuals))
mae_lasso

# reproducibility
set.seed(123)

# cross-validation fit for lasso model
lasso_cv <- cv.glmnet(x, y, family = "gaussian", alpha = 1, nfolds = 10)

# find best lambda 
lambda_best_lasso <- lasso_cv$lambda.min
lambda_best_lasso

# predicted values
predictedValues_lasso <- predict(lasso, newx = x)

#  rmse cross validation result
lasso_rmse <- sqrt(mean((y - predictedValues_lasso)^2))
lasso_rmse

# r2 cross validation result
lasso_r_squared <- 1 - (sum((y - predictedValues_lasso)^2) / sum((y - mean(y))^2))
lasso_r_squared

# mae cross validation result
lasso_mae <- mean(abs(y - predictedValues_lasso))
lasso_mae
#       RMSE      Rsquared    MAE 
# CV-  9.074624, 0.1613556, 7.555519
# model- 8.942655, 0.1823, 7.471322

# make a dataframe of cross validation vs Model results
results_lasso_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.074624, 0.1613556, 7.555519),
  Model = c(8.942655, 0.1823, 7.471322))
results_lasso_valid

                             # Lasso- TRANSFORMATIONS #

# look at model
summary(lasso_LM_log)

rmse_lasso_log <- sqrt(mean(lasso_LM_log$residuals^2))
rmse_lasso_log
mae_lasso_log <- mean(abs(lasso_LM_log$residuals))
mae_lasso_log

# reproducibility
set.seed(123)

# cross-validation fit for lasso model
lasso_cv2 <- cv.glmnet(x2, y2, family = "gaussian", alpha = 1, nfolds = 10)

# find best lambda 
lambda_best_lasso2 <- lasso_cv2$lambda.min
lambda_best_lasso2

# predicted values
predictedValues_lasso2 <- predict(lasso2, newx = x2)

# rmse cross validation result
lasso_rmse2 <- sqrt(mean((y2 - predictedValues_lasso2)^2))
lasso_rmse2

# r2 cross validation result
lasso_r_squared2 <- 1 - (sum((y2 - predictedValues_lasso2)^2) / sum((y2 - mean(y2))^2))
lasso_r_squared2

# mae cross validation result
lasso_mae2 <- mean(abs(y2 - predictedValues_lasso2))
lasso_mae2

#       RMSE      Rsquared    MAE 
# CV-  9.058481, .1609607, 7.554899
# model- 8.937728, 0.1823, 7.398654

# make a dataframe of cross validation vs Model results
results_lassoLOG_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.058481, .1609607, 7.554899),
  Model = c(8.937728, 0.1823, 7.398654))
results_lassoLOG_valid
                           # Elastic Net- NO TRANSFORMATIONS #

# look at model
summary(elastic_LM)

# find rmse & mae in model
rmse_elastic <- sqrt(mean(elastic_LM$residuals^2))
rmse_elastic
mae_elastic <- mean(abs(elastic_LM$residuals))
mae_elastic

# reproducibility
set.seed(123)

# cross-validation fit for elastic model
elastic_cv <- cv.glmnet(x, y, family = "gaussian", alpha = 0.5, nfolds = 10)

# find best lambda 
lambda_best_elastic <- elastic_cv$lambda.min
lambda_best_elastic

# predicted values
predictedValues_elastic <- predict(elasticnet, newx = x)

#  rmse cross validation result
elastic_rmse <- sqrt(mean((y - predictedValues_elastic)^2))
elastic_rmse

# r2 cross validation result
elastic_r_squared <- 1 - (sum((y - predictedValues_elastic)^2) / sum((y - mean(y))^2))
elastic_r_squared

# mae cross validation result
elastic_mae <- mean(abs(y - predictedValues_elastic))
elastic_mae

#       RMSE      Rsquared    MAE 
# CV-   9.063011, 0.1601212, 7.560475
# model- 8.942655, 0.1823, 7.471322

# make a dataframe of cross validation vs Model results
results_elastic_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.063011, 0.1601212, 7.560475),
  Model = c(8.942655, 0.1823, 7.471322))
results_elastic_valid

                          # Elastic Net- TRANSFORMATIONS #

# look at model
summary(elastic_LM_log)

# find rmse & mae in model
rmse_elastic_log <- sqrt(mean(elastic_LM_log$residuals^2))
rmse_elastic_log
mae_elastic_log <- mean(abs(elastic_LM_log$residuals))
mae_elastic_log

# reproducibility
set.seed(123)

# cross-validation fit for elastic model
elastic_cv2 <- cv.glmnet(x2, y2, family = "gaussian", alpha = 0.5, nfolds = 10)

# find best lambda 
lambda_best_elastic2 <- elastic_cv2$lambda.min
lambda_best_elastic2

# predicted values
predictedValues_elastic2 <- predict(elasticnet2, newx = x2)


# rmse cross validation result
elastic_rmse2 <- sqrt(mean((y2 - predictedValues_elastic2)^2))
elastic_rmse2

# r2 cross validation result
elastic_r_squared2 <- 1 - (sum((y2 - predictedValues_elastic2)^2) / sum((y2 - mean(y2))^2))
elastic_r_squared2

# mae cross validation result
elastic_mae2 <- mean(abs(y2 - predictedValues_elastic2))
elastic_mae2

#       RMSE      Rsquared    MAE 
# CV-  9.056776, 0.1612764, 7.55294
# model- 8.937728, 0.1823, 7.398654

# make a dataframe of cross validation vs Model results
results_elasticLOG_valid <- data.frame(
  Measure = c("RMSE", "Rsquared", "MAE"),
  Cross_Validation = c(9.056776, 0.1612764, 7.55294),
  Model = c(8.937728, 0.1823, 7.398654))
results_elasticLOG_valid








                  


  









