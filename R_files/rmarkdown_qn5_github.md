Use the available variables to construct a model that predicts delays
================
J Sudharsaan
3/6/2022

**Setting up working directory**

``` r
setwd("C:/Users/Sudharsaan/OneDrive/SIM/Year 2/ST2195 Programming for Data Science/Coursework/Essential Harvard Dataverse Files")
getwd()
```

    ## [1] "C:/Users/Sudharsaan/OneDrive/SIM/Year 2/ST2195 Programming for Data Science/Coursework/Essential Harvard Dataverse Files"

**Installing & Loading packages in R**

``` r
#install.packages("dplyr")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#install.packages("DBI")
library(DBI)

#install.packages("skimr")
library(skimr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("ggthemes")
library(ggthemes)

#install.packages("mlr3")
library(mlr3)
```

    ## 
    ## Attaching package: 'mlr3'

    ## The following object is masked from 'package:skimr':
    ## 
    ##     partition

``` r
#install.packages("mlr3learners")
library(mlr3learners)

#install.packages("mlr3pipelines")
library(mlr3pipelines)

#install.packages("mlr3tuning")
library(mlr3tuning)
```

    ## Loading required package: paradox

``` r
#install.packages("paradox")
library(paradox)

#install.packages("mlr3viz")
library(mlr3viz)

#install.packages("glmnet")
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 4.1-3

``` r
#install.packages("future")
library(future)
```

**Creating connection with airlinemain\_r.db (already created)**

``` r
conn <- dbConnect(RSQLite::SQLite(), "airlinemain_r.db")
```

**Performing Query on question 5 of coursework**

Question 5 asks to use the available variables construct a model that
predicts delays. Delays in the context of our model refers to the
arrival delay. Thus, we will be building a model that predicts **arrival
delay (target/response variable)** of flights that is based on data
collected in 2006 & 2007. The **features** that will be chosen for
bulding the model are: - CarrierDelay, WeatherDelay, NASDelay,
SecurityDelay, LateAircraftDelay, DepDelay, & AgeofPlane.

Take note that we remove cancelled & diverted flights from the dataset
as rows of cancelled & diverted flights contain many null values in its
features. This is due to the nature of cancelled & diverted flights.
Furthermore, we aim to use age of plane as one of the features. In
question 2, we queried for planes between ages 0 & 61 given the years
the planes were manufactured & the latest year of flight in our dataset.
Thus, when querying for age of planes as one of the features in our
model, we once again restrict planes whose ages are below 0 & above 61
as they are considered erroneous data given the context of our analysis.

``` r
q5_dataset <- dbSendQuery(conn,
                          "SELECT CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay, DepDelay, (ontime.Year - planes.year) AS AgeofPlane, ArrDelay
                          FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
                          WHERE Cancelled = 0 AND Diverted = 0 AND (ontime.Year - planes.year) > -1 AND (ontime.Year - planes.year) < 62")

q5_dataset <- dbFetch(q5_dataset)
q5_dataset <- as.data.frame(q5_dataset)
head(q5_dataset)
```

    ##   CarrierDelay WeatherDelay NASDelay SecurityDelay LateAircraftDelay DepDelay
    ## 1            0            0        0             0                 0       -2
    ## 2            0            0        0             0                 0        0
    ## 3            0            0        0             0                 0       -8
    ## 4            0            0        0             0                 0       -3
    ## 5            0            0        0             0                 0       -3
    ## 6            0            0        0             0                 0       -3
    ##   AgeofPlane ArrDelay
    ## 1          7        6
    ## 2          9      -23
    ## 3         16        0
    ## 4          6        2
    ## 5         17        7
    ## 6          7       -4

**Creating Models to predict Arrival Delay**

We will be creating **3 different models** to predict arrival delay &
assessing which model is the best using various **metrics**. The models
to be created are: 1. Linear Regression Model 2. Lasso Regression Model
3. Ridge Regression Model

``` r
# TaskRegr is used to perform regression since our target variable, "ArrDelay", is a continuous variable. 
task <- TaskRegr$new(id = 'arrival_delay', backend = q5_dataset, target = 'ArrDelay')

task$nrow # Shows number of rows in our dataset. 
```

    ## [1] 11564875

``` r
task$ncol # Shows number of columns in our dataset.
```

    ## [1] 8

``` r
print(task)
```

    ## <TaskRegr:arrival_delay> (11564875 x 8)
    ## * Target: ArrDelay
    ## * Properties: -
    ## * Features (7):
    ##   - int (7): AgeofPlane, CarrierDelay, DepDelay, LateAircraftDelay,
    ##     NASDelay, SecurityDelay, WeatherDelay

``` r
print(task$feature_names) # Shows the name of the features in our dataset. 
```

    ## [1] "AgeofPlane"        "CarrierDelay"      "DepDelay"         
    ## [4] "LateAircraftDelay" "NASDelay"          "SecurityDelay"    
    ## [7] "WeatherDelay"

``` r
print(task$target_names) # Shows the name of the target/response variable in our dataset. 
```

    ## [1] "ArrDelay"

``` r
print(task$missings()) # Shows that we have no missing values in our dataset. 
```

    ##          ArrDelay        AgeofPlane      CarrierDelay          DepDelay 
    ##                 0                 0                 0                 0 
    ## LateAircraftDelay          NASDelay     SecurityDelay      WeatherDelay 
    ##                 0                 0                 0                 0

``` r
# print(task$data()) # Shows the dataset that the task is working with for the regression. 
```

``` r
measure <- msr("regr.mse")
```

**Linear Regression Model**

``` r
# Choosing linear regression as one of the learners for this regression task.
learner_lm <- lrn("regr.lm")

# Performing scaling & handling missing values for this linear regression machine learning model. 
gr_lm <- po("scale") %>>% po("imputemean") %>>%
                          po(learner_lm)

# Combined learner (Combines the pre-processing stage & machine learning model stage)
glrn_lm <- GraphLearner$new(gr_lm)

# For standard linear regression, there isn't any hyperparameters. Thus, we will not be tuning hyperparameters for our linear regression model.  
```

``` r
future::plan()
```

    ## sequential:
    ## - args: function (..., envir = parent.frame())
    ## - tweaked: FALSE
    ## - call: NULL

``` r
# Ensures reproducibility of result
set.seed(1)

# Splitting dataset into train & test sets.
train_set <- sample(task$nrow, 0.7 * task$nrow)
test_set <- setdiff(seq_len(task$nrow), train_set)

# Training of linear regression model
glrn_lm$train(task, row_ids = train_set)

# Prediction of arrival delay values using linear regression model & attaining Mean Squared Error value to evaluate performance of model.
prediction_lm <- glrn_lm$predict(task, row_ids = test_set)$score()
head(prediction_lm)
```

    ## regr.mse 
    ##   72.305

``` r
predict_values_lm <- glrn_lm$predict(task, row_ids = test_set)
predict_values_lm <- as.list(predict_values_lm)

predict_values_lm_response <- as.data.frame(predict_values_lm$response)
head(predict_values_lm_response)
```

    ##   predict_values_lm$response
    ## 1                  -4.026388
    ## 2                  -4.210303
    ## 3                  -5.526772
    ## 4                 160.356274
    ## 5                  -5.104842
    ## 6                  -4.870247

``` r
predict_values_lm_truth <- as.data.frame(predict_values_lm$truth)
head(predict_values_lm_truth)
```

    ##   predict_values_lm$truth
    ## 1                     -23
    ## 2                       7
    ## 3                      -5
    ## 4                     149
    ## 5                     -15
    ## 6                      12

``` r
predict_values_lm <- cbind(predict_values_lm_truth, predict_values_lm_response)
names(predict_values_lm) <- c("Actual_ArrDelay", "Predicted_ArrDelay") 

min(predict_values_lm$Actual_ArrDelay)
```

    ## [1] -562

``` r
min(predict_values_lm$Predicted_ArrDelay)
```

    ## [1] -117.1781

``` r
max(predict_values_lm$Actual_ArrDelay)
```

    ## [1] 2598

``` r
max(predict_values_lm$Predicted_ArrDelay)
```

    ## [1] 2678.484

``` r
q5_lm_scatterplot <- ggplot(predict_values_lm) + geom_point(aes(x = predict_values_lm[,2], y = predict_values_lm[,1]), colour = "darkred") + geom_smooth(aes(x = predict_values_lm[,2], y = predict_values_lm[,1]), se = FALSE, method = lm, colour = "red") + labs(title = "Figure 5.1: Linear Regression Model on Arrival Delay", x = "Predicted Arrival Delays", y = "Actual Arrival Delays") + theme_economist() + xlim(-600,2800) + ylim(-600, 2800)

q5_lm_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn5_github_files/figure-gfm/Plotting%20Linear%20Regression%20Model-1.png)<!-- -->

**Lasso Regression Model**

``` r
# Ensuring reproducible results
set.seed(1)

# Setting up tune environment for tuning of hyperparameters. We specify the range that the parameter "lambda" can take for the lasso regression is between 0.001 & 2. 
tune_lambda <- ParamSet$new(list(
  ParamDbl$new("regr.glmnet.lambda", lower = 0.001, upper = 2)
))

# Specifying how the search will be performed (grid search refers to a systematic way of searching). There exist other forms of searching such as "random search".
tuner <- tnr("grid_search")

# Specifying the number of times the procedure will be repeated to be 3 times. Evaluation is defined as one resampling of a parameter value. 
terminator <- trm("evals", n_evals = 3)
```

``` r
# Choosing lasso regression as one of the learners for this regression task.
learner_lasso <- lrn("regr.glmnet")

# We set the parameter alpha's value to 1 to specify the use of lasso regression. 
learner_lasso$param_set$values <- list(alpha = 1)

# Performing scaling & handling missing values for this lasso regression machine learning model. 
gr_lasso <- po("scale") %>>% po("imputemean") %>>% 
                             po(learner_lasso)

# Combined learner (Combines the pre-processing stage & machine learning model stage)
glrn_lasso <- GraphLearner$new(gr_lasso)

# Putting the tuning of hyperparameter with the combined learner to create a new learner
at_lasso <- AutoTuner$new(
  learner = glrn_lasso,
  resampling = rsmp("cv", folds = 3),
  measure = measure,
  search_space = tune_lambda,
  terminator = terminator,
  tuner = tuner
)
```

``` r
future::plan()
```

    ## sequential:
    ## - args: function (..., envir = parent.frame())
    ## - tweaked: FALSE
    ## - call: NULL

``` r
# Ensures reproducibility of result
set.seed(1)

# Training of lasso regression model
at_lasso$train(task, row_ids = train_set)
```

    ## INFO  [21:35:59.560] [bbotk] Starting to optimize 1 parameter(s) with '<TunerGridSearch>' and '<TerminatorEvals> [n_evals=3, k=0]' 
    ## INFO  [21:35:59.586] [bbotk] Evaluating 1 configuration(s) 
    ## INFO  [21:35:59.631] [mlr3] Running benchmark with 3 resampling iterations 
    ## INFO  [21:35:59.725] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 3/3) 
    ## INFO  [21:36:20.148] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 1/3) 
    ## INFO  [21:36:40.775] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 2/3) 
    ## INFO  [21:37:00.598] [mlr3] Finished benchmark 
    ## INFO  [21:37:01.407] [bbotk] Result of batch 1: 
    ## INFO  [21:37:01.409] [bbotk]  regr.glmnet.lambda regr.mse warnings errors runtime_learners 
    ## INFO  [21:37:01.409] [bbotk]               0.001 72.39615        0      0            60.53 
    ## INFO  [21:37:01.409] [bbotk]                                 uhash 
    ## INFO  [21:37:01.409] [bbotk]  4eb8533c-31cd-4485-ada8-44d8e0149608 
    ## INFO  [21:37:01.410] [bbotk] Evaluating 1 configuration(s) 
    ## INFO  [21:37:01.444] [mlr3] Running benchmark with 3 resampling iterations 
    ## INFO  [21:37:01.448] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 3/3) 
    ## INFO  [21:37:21.852] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 2/3) 
    ## INFO  [21:37:41.679] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 1/3) 
    ## INFO  [21:38:02.036] [mlr3] Finished benchmark 
    ## INFO  [21:38:02.773] [bbotk] Result of batch 2: 
    ## INFO  [21:38:02.774] [bbotk]  regr.glmnet.lambda regr.mse warnings errors runtime_learners 
    ## INFO  [21:38:02.774] [bbotk]            1.555778 95.50004        0      0            60.25 
    ## INFO  [21:38:02.774] [bbotk]                                 uhash 
    ## INFO  [21:38:02.774] [bbotk]  35bc0d42-8368-45c6-b017-ae37f1fa56a9 
    ## INFO  [21:38:02.775] [bbotk] Evaluating 1 configuration(s) 
    ## INFO  [21:38:02.808] [mlr3] Running benchmark with 3 resampling iterations 
    ## INFO  [21:38:02.813] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 3/3) 
    ## INFO  [21:38:23.037] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 2/3) 
    ## INFO  [21:38:42.843] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 1/3) 
    ## INFO  [21:39:03.148] [mlr3] Finished benchmark 
    ## INFO  [21:39:04.010] [bbotk] Result of batch 3: 
    ## INFO  [21:39:04.011] [bbotk]  regr.glmnet.lambda regr.mse warnings errors runtime_learners 
    ## INFO  [21:39:04.011] [bbotk]            1.333667 89.72771        0      0            59.89 
    ## INFO  [21:39:04.011] [bbotk]                                 uhash 
    ## INFO  [21:39:04.011] [bbotk]  8179d65e-cf62-4319-bcfa-bc247efcd228 
    ## INFO  [21:39:04.016] [bbotk] Finished optimizing after 3 evaluation(s) 
    ## INFO  [21:39:04.017] [bbotk] Result: 
    ## INFO  [21:39:04.017] [bbotk]  regr.glmnet.lambda learner_param_vals  x_domain regr.mse 
    ## INFO  [21:39:04.017] [bbotk]               0.001          <list[3]> <list[1]> 72.39615

``` r
# Prediction of arrival delay values using lasso regression model & attaining Mean Squared Error (MSE) value to evaluate performance of model.
prediction_lasso <- at_lasso$predict(task, row_ids = test_set)$score()
head(prediction_lasso)
```

    ## regr.mse 
    ## 72.30732

``` r
# Converting object of "environment" type to a data frame by changing it into a list before changing parts of the list into a dataframe. Take note that using as.data.frame() results in errors. 
predict_values_lasso <- at_lasso$predict(task, row_ids = test_set)
predict_values_lasso <- as.list(predict_values_lasso)

predict_values_lasso_response <- as.data.frame(predict_values_lasso$response)
head(predict_values_lasso_response)
```

    ##   predict_values_lasso$response
    ## 1                     -4.021165
    ## 2                     -4.214669
    ## 3                     -5.541804
    ## 4                    160.484478
    ## 5                     -5.114074
    ## 6                     -4.876625

``` r
predict_values_lasso_truth <- as.data.frame(predict_values_lasso$truth)
head(predict_values_lasso_truth)
```

    ##   predict_values_lasso$truth
    ## 1                        -23
    ## 2                          7
    ## 3                         -5
    ## 4                        149
    ## 5                        -15
    ## 6                         12

``` r
predict_values_lasso <- cbind(predict_values_lasso_truth, predict_values_lasso_response)
names(predict_values_lasso) <- c("Actual_ArrDelay", "Predicted_ArrDelay") 

min(predict_values_lasso$Predicted_ArrDelay)
```

    ## [1] -118.9495

``` r
max(predict_values_lasso$Predicted_ArrDelay)
```

    ## [1] 2677.5

``` r
q5_lasso_scatterplot <- ggplot(predict_values_lasso) + geom_point(aes(x = predict_values_lasso[,2], y = predict_values_lasso[,1]), colour = "darkgreen") + geom_smooth(aes(x = predict_values_lasso[,2], y = predict_values_lasso[,1]), se = FALSE, method = lm, colour = "green") + labs(title = "Figure 5.2: Lasso Regression Model on Arrival Delay", x = "Predicted Arrival Delays", y = "Actual Arrival Delays") + theme_economist() + xlim(-600,2800) + ylim(-600, 2800)

q5_lasso_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn5_github_files/figure-gfm/Plotting%20Lasso%20Regression%20Model-1.png)<!-- -->

**Ridge Regression Model**

``` r
# Tuning of hyperparameters for the ridge regression model is identical to the tuning of hyperparameters for lasso regression model. You may choose to skip this part as the declaration of variables is unchanged. We redeclare the variables to show that hyperparameter tuning is taking place for the ridge regression model.  

# Ensuring reproducible results
set.seed(1)

# Setting up tune environment for tuning of hyperparameters. We specify the range that the parameter "lambda" can take for the ridge regression is between 0.001 & 2. 
tune_lambda <- ParamSet$new(list(
  ParamDbl$new("regr.glmnet.lambda", lower = 0.001, upper = 2)
))

# Specifying how the search will be performed (grid search refers to a systematic way of searching). There exist other forms of searching such as "random search".
tuner <- tnr("grid_search")

# Specifying the number of times the procedure will be repeated to be 3 times. Evaluation is defined as one resampling of a parameter value. 
terminator <- trm("evals", n_evals = 3)
```

``` r
# Choosing ridge regression as one of the learners for this regression task.
learner_ridge <- lrn("regr.glmnet")

# We set the parameter alpha's value to 0 to specify the use of ridge regression. 
learner_ridge$param_set$values <- list(alpha = 0)

# Performing scaling & handling missing values for this ridge regression machine learning model. 
gr_ridge <- po("scale") %>>% po("imputemean") %>>% 
                             po(learner_ridge)

# Combined learner (Combines the pre-processing stage & machine learning model stage)
glrn_ridge <- GraphLearner$new(gr_ridge)

# Putting the tuning of hyperparameter with the combined learner to create a new learner
at_ridge <- AutoTuner$new(
  learner = glrn_ridge,
  resampling = rsmp("cv", folds = 3),
  measure = measure,
  search_space = tune_lambda,
  terminator = terminator,
  tuner = tuner
)
```

``` r
future::plan()
```

    ## sequential:
    ## - args: function (..., envir = parent.frame())
    ## - tweaked: FALSE
    ## - call: NULL

``` r
# Ensures reproducibility of result
set.seed(1)

# Training of ridge regression model
at_ridge$train(task, row_ids = train_set)
```

    ## INFO  [21:44:29.993] [bbotk] Starting to optimize 1 parameter(s) with '<TunerGridSearch>' and '<TerminatorEvals> [n_evals=3, k=0]' 
    ## INFO  [21:44:29.996] [bbotk] Evaluating 1 configuration(s) 
    ## INFO  [21:44:30.029] [mlr3] Running benchmark with 3 resampling iterations 
    ## INFO  [21:44:30.034] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 3/3) 
    ## INFO  [21:44:49.951] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 1/3) 
    ## INFO  [21:45:10.119] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 2/3) 
    ## INFO  [21:45:30.223] [mlr3] Finished benchmark 
    ## INFO  [21:45:31.176] [bbotk] Result of batch 1: 
    ## INFO  [21:45:31.177] [bbotk]  regr.glmnet.lambda regr.mse warnings errors runtime_learners 
    ## INFO  [21:45:31.177] [bbotk]               0.001 72.39609        0      0            59.87 
    ## INFO  [21:45:31.177] [bbotk]                                 uhash 
    ## INFO  [21:45:31.177] [bbotk]  e756151f-5444-415e-9ec0-b95af6a40a90 
    ## INFO  [21:45:31.178] [bbotk] Evaluating 1 configuration(s) 
    ## INFO  [21:45:31.211] [mlr3] Running benchmark with 3 resampling iterations 
    ## INFO  [21:45:31.216] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 3/3) 
    ## INFO  [21:45:51.509] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 2/3) 
    ## INFO  [21:46:11.745] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 1/3) 
    ## INFO  [21:46:32.155] [mlr3] Finished benchmark 
    ## INFO  [21:46:32.897] [bbotk] Result of batch 2: 
    ## INFO  [21:46:32.899] [bbotk]  regr.glmnet.lambda regr.mse warnings errors runtime_learners 
    ## INFO  [21:46:32.899] [bbotk]            1.555778 75.46619        0      0            60.37 
    ## INFO  [21:46:32.899] [bbotk]                                 uhash 
    ## INFO  [21:46:32.899] [bbotk]  44ee644d-a522-41bf-99e1-d9895cd17215 
    ## INFO  [21:46:32.900] [bbotk] Evaluating 1 configuration(s) 
    ## INFO  [21:46:32.932] [mlr3] Running benchmark with 3 resampling iterations 
    ## INFO  [21:46:32.937] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 3/3) 
    ## INFO  [21:46:53.828] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 2/3) 
    ## INFO  [21:47:15.509] [mlr3] Applying learner 'scale.imputemean.regr.glmnet' on task 'arrival_delay' (iter 1/3) 
    ## INFO  [21:47:36.142] [mlr3] Finished benchmark 
    ## INFO  [21:47:36.892] [bbotk] Result of batch 3: 
    ## INFO  [21:47:36.893] [bbotk]  regr.glmnet.lambda regr.mse warnings errors runtime_learners 
    ## INFO  [21:47:36.893] [bbotk]            1.333667 74.91496        0      0            62.87 
    ## INFO  [21:47:36.893] [bbotk]                                 uhash 
    ## INFO  [21:47:36.893] [bbotk]  f5ffe7b5-5467-4d91-8d40-79d7a3e16a6b 
    ## INFO  [21:47:36.898] [bbotk] Finished optimizing after 3 evaluation(s) 
    ## INFO  [21:47:36.898] [bbotk] Result: 
    ## INFO  [21:47:36.899] [bbotk]  regr.glmnet.lambda learner_param_vals  x_domain regr.mse 
    ## INFO  [21:47:36.899] [bbotk]               0.001          <list[3]> <list[1]> 72.39609

``` r
# Prediction of arrival delay values using ridge regression model & attaining Mean Squared Error (MSE) value to evaluate performance of model.
prediction_ridge <- at_ridge$predict(task, row_ids = test_set)$score()
head(prediction_ridge)
```

    ## regr.mse 
    ## 72.30718

``` r
# Converting object of "environment" type to a data frame by changing it into a list before changing parts of the list into a dataframe. Take note that using as.data.frame() results in errors. 
predict_values_ridge <- at_ridge$predict(task, row_ids = test_set)
predict_values_ridge <- as.list(predict_values_ridge)

predict_values_ridge_response <- as.data.frame(predict_values_ridge$response)
head(predict_values_ridge_response)
```

    ##   predict_values_ridge$response
    ## 1                     -4.021760
    ## 2                     -4.213969
    ## 3                     -5.542450
    ## 4                    160.485243
    ## 5                     -5.114725
    ## 6                     -4.877211

``` r
predict_values_ridge_truth <- as.data.frame(predict_values_ridge$truth)
head(predict_values_ridge_truth)
```

    ##   predict_values_ridge$truth
    ## 1                        -23
    ## 2                          7
    ## 3                         -5
    ## 4                        149
    ## 5                        -15
    ## 6                         12

``` r
predict_values_ridge <- cbind(predict_values_ridge_truth, predict_values_ridge_response)
names(predict_values_ridge) <- c("Actual_ArrDelay", "Predicted_ArrDelay") 

min(predict_values_ridge$Predicted_ArrDelay)
```

    ## [1] -118.9083

``` r
max(predict_values_ridge$Predicted_ArrDelay)
```

    ## [1] 2677.575

``` r
q5_ridge_scatterplot <- ggplot(predict_values_ridge) + geom_point(aes(x = predict_values_ridge[,2], y = predict_values_ridge[,1]), colour = "darkblue") + geom_smooth(aes(x = predict_values_ridge[,2], y = predict_values_ridge[,1]), se = FALSE, method = lm, colour = "blue") + labs(title = "Figure 5.3: Ridge Regression Model on Arrival Delay", x = "Predicted Arrival Delays", y = "Actual Arrival Delays") + theme_economist() + xlim(-600,2800) + ylim(-600, 2800)

q5_ridge_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn5_github_files/figure-gfm/Plotting%20Ridge%20Regression%20Model-1.png)<!-- -->

``` r
metrics <- list(prediction_lm, prediction_lasso, prediction_ridge)
metrics <- as.data.frame(metrics)
names(metrics) <- c("Linear", "Lasso", "Ridge")
rownames(metrics) <- c("Mean Squared Error")
metrics
```

    ##                    Linear    Lasso    Ridge
    ## Mean Squared Error 72.305 72.30732 72.30718

**Observations from table above**

From the table above, we can observe that the Linear Regression model
has the lowest Mean Squared Error (MSE) compared to the other models,
Lasso & Ridge Regression.

**Conclusion**

Thus, **Linear Regression model** is the **best model** out of the 3
models created in predicting arrival delays.

``` r
dbDisconnect(conn)
```

    ## Warning in connection_release(conn@ptr): There are 1 result in use. The
    ## connection will be released when they are closed
