# Data Wrangle_EDA_Prep
## Libraries
library(tidyverse)
library(caret)
library(readr)
library(matrixStats)
library(utils)
options(scipen = 999)


skim_glim <- function(df) {
  tibble::glimpse(df)
  skimr::skim(df)
}

#######################################################################################
# Data Import & Wrangle(1)

# First Problem: Set Data Variable Ids (column names) deriveed from wdbc.names.txt
name_cols <- c("id","diagnosis","radius_mean","texture_mean",
               "perimeter_mean","area_mean","smoothness_mean",
               "compactness_mean","concavity_mean","concave_points_mean",
               "symmetry_mean","fractal_dimension_mean","radius_se","texture_se",
               "perimeter_se","area_se","smoothness_se","compactness_se",
               "concavity_se","concave_points_se","symmetry_se","fractal_dimension_se",
               "radius_worst","texture_worst","perimeter_worst",
               "area_worst","smoothness_worst","compactness_worst",
               "concavity_worst","concave_points_worst","symmetry_worst",
               "fractal_dimension_worst")

# Read in UIC data with column names
wdbc_data <- read_csv("wdbc.data.csv", col_names = name_cols )

str(wdbc_data) # Check import


## NOTE: for RMD
# Read in UIC data with column names
#csv_url <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

#wdbc_data <- read_csv(csv_url, col_names = name_cols)

# as.factor and set levels
wdbc_data <- mutate_if(wdbc_data, is.character, as.factor) %>% 
  mutate_at("diagnosis", factor, levels = c("M", "B")) # Set malignant as POSITIVE

str(wdbc_data)  # Check result 

skim_glim(wdbc_data) # Quick EDA overview
#
# All good on data import: note that skim does not display properly in RMD
#



###############################################################################
# ML  EDA, using old school and Tidyverse methods
#

table(wdbc_data$diagnosis) # check distribution

# Convert wdbc_data  to matrix
wdbc_mx <- as.matrix(wdbc_data[, 3:32]) # remove id & diagnosis
# Set the row names 
row.names(wdbc_mx) <- wdbc_data$id

#
#  Ways of checking data varability
# 

colMeans2(wdbc_mx) # old school
colSds(wdbc_mx)  # old school
apply(wdbc_mx, 2, mean) # old school
apply(wdbc_mx, 2, sd) # old school

# Recapture as df using Tidyverse
tidy_2 <- bind_cols(enframe(names(wdbc_data[, 3:32]), 
                            name = NULL, value = "Variable"),
                    enframe(colMeans2(wdbc_mx), 
                            name = NULL, value = "Avg") ,
                    enframe(colSds(wdbc_mx), 
                            name = NULL, value = "SD"),
                    enframe(colMins(wdbc_mx), 
                            name = NULL, value = "Min"),
                    enframe(colMaxs(wdbc_mx), 
                            name = NULL, value = "Max"),
                    enframe(colMedians(wdbc_mx), 
                            name = NULL, value = "Median"))

tidy_2 %>% head() %>%  
  knitr::kable(caption = "wdbc: Summary Stats [first 6 variables shown] ") 

# Summarise the summary table -- another Tidyverse advantage
tidy_2 %>% 
  summarize(avg_mean = mean(Avg), sd_mean = sd(Avg),
            min_mean = min(Avg), max_mean = max(Avg),
            avg_sd = mean(SD), sd_sd = sd(SD), 
            min_sd = min(SD), max_sd = max(SD)) %>% 
  knitr::kable(caption = "wdbc: Range of mean & sd values") 

# Strong case for centering and scaling the data

####################
# Explore centering and scaling data

table(wdbc_data$diagnosis) # We know that we have   B   M  357 212.

true_values <-  as.factor(wdbc_data$diagnosis) %>% 
  relevel("M") %>% set_names(wdbc_data$id) # Set malignant as POSITIVE

#check for `id` match
head(true_values)
t(wdbc_mx[1:6, 1:2])


##
## Unsupervised Learning for EDA
##
library(cluster)
library(fpc)
## We know that we have   B   M  357 212.  
# The task of kmeans here, unsupervised learning, is just to identify which belong to which centers.

# Unscaled
set.seed(2019)
unscaled_K <- kmeans(wdbc_mx, centers = 2, nstart = 20)
table(unscaled_K$cluster)
# Viz
plotcluster(wdbc_mx, unscaled_K$cluster, main = "Unscaled K Results",
            ylab = "", xlab = "Cluster 1: 131 assigned; Cluster 2: 438")

# Center and Scale
wdbc_mx_sc <- scale(sweep(wdbc_mx, 2, colMeans(wdbc_mx))) # center & scale
set.seed(2019)
scaled_K <- kmeans(wdbc_mx_sc, centers = 2, nstart = 20)
table(scaled_K$cluster)
# Viz
plotcluster(wdbc_mx, scaled_K$cluster, 
            main = "Centered & Scaled K Results",
            ylab = "", xlab = "Cluster1: 380 assigned; Cluster 2: 189")



## Test Assumptions about Clusters per model
# unscaled
unscaled_k_pred <- if_else(unscaled_K$cluster == 1, "M", "B") %>% 
  as.factor() %>% relevel("M") %>% set_names(wdbc_data$id)

rbind(unscaled_k_pred[60:64], true_values[60:64]) # check   
mean(unscaled_k_pred[60:79] == true_values[60:79]) # check   

# scaled
scaled_k_pred <- if_else(scaled_K$cluster == 1, "B", "M") %>% 
  as.factor() %>% relevel("M") %>% set_names(wdbc_data$id)

rbind(scaled_k_pred[60:64], true_values[60:64]) # check  
mean(scaled_k_pred[60:79] == true_values[60:79]) # check  

ID_check <- rbind(
  rbind("True_Values" = true_values[90:97]), 
  rbind("Unscaled_K" = unscaled_k_pred[90:97]),
  rbind("Scaled_K" = scaled_k_pred[90:97])  
  ) 

#rbind(true_values[90:97]) %>% as.data.frame() %>% names()
#rbind(unscaled_k_pred[90:97]) %>% as.data.frame() %>% names()
#rbind(scaled_k_pred[90:97]) %>% as.data.frame() %>% names() 

#rbind(true_values[90:97]) %>% as.data.frame() %>% 
 # knitr::kable(caption = "true_values[90:97]" )

ID_check %>% 
  knitr::kable(caption = "IDs and Indexs Match: sample[90:97]" )

## Confusion Matrix Test
cfm_unscaled_k <- confusionMatrix(unscaled_k_pred, true_values)
cfm_unscaled_k
cfm_scaled_k  <- confusionMatrix(scaled_k_pred, true_values)
cfm_scaled_k 

# key values as table output
key_values_K_cluster <- bind_cols( 
  enframe(cfm_unscaled_k$overall["Accuracy"], name = NULL, value = "unK_Acc" ),
  enframe(cfm_unscaled_k$byClass["F1"], name = NULL, value = "unK_F1" ) ,
  enframe(cfm_scaled_k$overall["Accuracy"], name = NULL, value = "scalK_Acc " ),
  enframe(cfm_scaled_k$byClass["F1"], name = NULL, value = "scalK_F1" ) ) %>%  
  knitr::kable(caption = "Unscaled and Scaled K: Accuracy and F Measure results")

key_values_K_cluster 

# quick and dirty comp table
dirty_comp_table <- cbind(cbind(TrV = table(true_values)), 
      cfm_unscaled_k$table, 
      cfm_scaled_k$table) %>% 
  knitr::kable(caption = "L-R: True Values, Unscaled K, Scaled K" )

dirty_comp_table

# Raw vs. C-S
t(wdbc_mx)[1:8, 1:5] %>% 
  as.data.frame() %>% 
  rownames_to_column("Variable") %>%  
  knitr::kable(caption = "Raw Data: wdbc_mx: First 8 Vars. for 5 Cases")

t(wdbc_mx_sc)[1:8, 1:5] %>% 
  as.data.frame() %>% 
  rownames_to_column("Variable") %>%  
  knitr::kable(caption = "C-S Prep: wdbc_mx_sc: First 8 Vars. for 5 Cases")

# Centering and scaling the data shows 
# a meaningful improvement in Accuaracy and F Measure. 
#The improvement in predicting malignant cancer cells 
#is particularly important for this ML task.  
#We know both from the kmeans model test above, and from the professional 
#literature, that many ML alogorithms will perform better with centered & scaled data.  
#So our first preprocessing prepartion will be to center and scale the data.


# Next question. Would the data set benefit from PCAA/

diagno <- as.numeric(wdbc_data$diagnosis == "M") # for plotting
wdbc_PCA <- prcomp(wdbc_mx, center = TRUE, scale = TRUE) 

importance_df <- data.frame(Sum_Exp = summary(wdbc_PCA)$importance[3,]) %>%
  rownames_to_column("PCA") # Cumulative Proportion

PCA_sum <- summary(wdbc_PCA) # PCA list: SD, Prop Var., Cum Prop.
# PCA_sum$importance[3,] == summary(wdbc_PCA)$importance[3,]


biplot(wdbc_PCA, cex = 0.45)


plot_PCA1_2 <- data.frame(PC1 = wdbc_PCA$x[,1], PC2 = wdbc_PCA$x[,2],
                          label = factor(wdbc_data$diagnosis )) %>%
  ggplot(aes(PC1, PC2, fill = label)) +
  geom_point(cex = 3, pch = 21) +
  labs(fill = "Class", title = "True Value Groupings: PC1 / PC2",
       subtitle = "63% of variance explained") + 
  theme(legend.position = c(0.88, 0.14))


plot_PCA4_5 <- data.frame(PC4 = wdbc_PCA$x[,4], PC5 = wdbc_PCA$x[,5],
                          label = factor(wdbc_data$diagnosis )) %>%
  ggplot(aes(PC4, PC5, fill = label)) +
  geom_point(cex = 3, pch = 21) +
  labs(fill = "Class", title = "True Value Groupings: PC4 / PC5",
       subtitle = "12% of variance explained") + 
  theme(legend.position = c(0.88, 0.14))

#########
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Winston Chang
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(plot_PCA1_2,plot_PCA4_5 , cols = 2) 
#########


PCA_sum[["importance"]][, 1:5] %>% 
  as.data.frame() %>% 
  rownames_to_column("Results") %>%
  knitr::kable(caption = "PCA Summary: First 5 Components")

wdbc_PCA$rotation[1:8, 1:5] %>% 
  as.data.frame() %>% 
  rownames_to_column("Variable") %>%  
  knitr::kable(caption = "PCA Matrix: First 8 Variables for First 5 PC")



graph_PCA <- importance_df[1:12, ] %>% 
  ggplot( aes(reorder(PCA, Sum_Exp), Sum_Exp)) + 
  geom_point() +
  labs(title = "PCA Results: 10 components explain over 95% variance", 
       x = "Principal Component Analysis: 1-12 of 30" , 
       y = "Variance Explained", subtitle = "WDBC (Wisconsin Diagnostic Breast Cancer) data set") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_hline(aes(yintercept = 0.95), color = "orange", linetype = 2)

graph_PCA 
## Strong case for PCA; none for nzv, but include as formality
nearZeroVar(wdbc_data[, 3:32])

#
#  END EDA  on to modelling
#



#  Create first stage train and test sets  50% for model training; 50% for testing
#  Second stage: 82% train; 18 % test

#############################   50/50 train/test split
Y <- wdbc_data$diagnosis

set.seed(2019)
test_index <- createDataPartition(Y, times = 1, p = 0.5, list = FALSE)

# Apply index
test_set_id <- wdbc_data[test_index, ] # will use ID later
train_set_id <- wdbc_data[-test_index, ]

# Remove id for testing
test_set <- test_set_id %>% select(-id)
train_set <- train_set_id %>% select(-id)

#############################   82/18 train/test split
(test_ratio <- 1/sqrt(30)) # round down to 18%
#  Which models have an ML advantage?

set.seed(2019)
test_index2 <- createDataPartition(Y, times = 1, p = 0.18, list = FALSE)
# Apply index
test2_id <- wdbc_data[test_index2, ]
train2_id <- wdbc_data[-test_index2, ]
# Remove id variable
test_2 <- test2_id %>% select(-id)
train_2 <- train2_id %>% select(-id)

########  trainControl and Data Preparations for all models
## NOTE: To have reproducible results for ensemble modeling, must use seeds argument in trainControl
set.seed(2019)
seeds <- vector(mode = "list", length =  1000)
for (i in 1:1000) seeds[[i]] <- sample.int(1000, 800)

###  Re-usable trainControl for consistent comparison across models
#
###  Does NOT change: same for all models
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  seeds = seeds
)   

##
# Experiement with preprocessing: one NULL, two typical
##
prep_0 <- NULL
prep_1 <- c("center", "scale")
prep_2 <- c("nzv", "center", "scale", "pca")

###
## Select Models for Ensemble: 21
###
models <- c("adaboost", "avNNet", "gamboost", "gamLoess", "glm", 
            "gbm", "knn", "kknn", "lda", "mlp", "monmlp", "naive_bayes", 
            "qda", "ranger", "Rborist", "rf", "rpart", "svmLinear", "svmRadial", 
            "svmRadialCost", "svmRadialSigma")

mod_names <- enframe(models, value = "Model", name = NULL) 

###############################################################################
## SAVE POINT: 
## NEXT: Modelling and Results
###############################################################################


#
#
######################################## First Run : 50/50 ############################

set.seed(2019)
garbage_0 <- capture.output(  
  fits_0 <- lapply(models, function(model){
  print(model)
  train(diagnosis ~ ., data = train_set,  method = model,
        trControl = myControl, preProcess = prep_0)
}) 
)

names(fits_0) <- models

#  Predictions 0
predictions_0 <- sapply(fits_0, function(object) 
  predict(object, newdata = test_set))

# Predictions for CFM & F Measure
pred_ft_0 <- predictions_0 %>% as.data.frame() %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_all(., factor, levels = c("M", "B")) # Set malignant as POSITIVE

# Confusion Matrix for Prep_0
CFM_Prep_0 <- sapply(pred_ft_0 , function(object) {
  CFM <- confusionMatrix(data = object,  reference = test_set$diagnosis)
  list(CFM)
}) 

########### Quick and Dirty extract for all CFM Lists!
ACC_dex <- c(6, 30, 54, 78, 102, 126, 150,174 ,198 ,222 ,246, 
             270 ,294 ,318 ,342, 366, 390, 414, 438, 462, 486) # Accuracy score

F1_dex <- c(19, 43, 67, 91, 115, 139, 163, 187, 211, 235, 259, 283, 
            307, 331, 355, 379, 403, 427, 451, 475, 499) # F1 score
############

CFM_mess_0 <- CFM_Prep_0 %>% unlist() %>% as.data.frame() # create an ordered mess

CFM_0_Keys <- bind_cols(mod_names, 
                        Accuracy = round(as.numeric(as.character(CFM_mess_0[ACC_dex,])),4) , 
                        F_Measure = round(as.numeric(as.character(CFM_mess_0[F1_dex,])),4) 
) %>%
  mutate(Total = Accuracy + F_Measure) # grab values: convert from factor to numeric; round

CFM_0_Keys %>% arrange(desc(Total)) %>% head(n = 7) %>% 
  knitr::kable(caption = "Run One: NULL prep: Top Seven Models")

#
#
## Prep_1 center, scale
set.seed(2019)
garbage_1 <- capture.output(
  fits_1 <- lapply(models, function(model){
    print(model)
    train(diagnosis ~ ., data = train_set,  method = model,
          trControl = myControl, preProcess = prep_1)
  }) 
)
names(fits_1) <- models

##
#  Predictions
predictions_1 <- sapply(fits_1, function(object) 
  predict(object, newdata = test_set))

# Predictions for CFM & F Measure
pred_ft_1 <- predictions_1 %>% as.data.frame() %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_all(., factor, levels = c("M" , "B"))

# Confusion Matrix List for Prep_1
CFM_Prep_1 <- sapply(pred_ft_1 , function(object) {
  CFM <- confusionMatrix(data = object,  reference = test_set$diagnosis)
  list(CFM)
}) 


CFM_mess_1 <- CFM_Prep_1 %>% unlist() %>% as.data.frame()  # mess!

CFM_1_Keys <- bind_cols(mod_names, 
                        Accuracy = round(as.numeric(as.character(CFM_mess_1[ACC_dex,])), 4 ) , 
                        F_Measure = round(as.numeric(as.character(CFM_mess_1[F1_dex,])), 4 ) 
) %>%
  mutate(Total = Accuracy + F_Measure)

CFM_1_Keys %>% arrange(desc(Total)) %>% head(n = 7) %>% 
  knitr::kable(caption = "Run One: Prep_1: Top Seven Models")

#
#
## Prep 2: nzv, center, scale, pca
set.seed(2019)
garbage_2 <- capture.output(
fits_2 <- lapply(models, function(model){ 
  print(model)
  train(diagnosis ~ ., data = train_set,  method = model,
        trControl = myControl, preProcess = prep_2)
}) 
)

names(fits_2) <- models

# Predictions
predictions_2 <- sapply(fits_2, function(object) 
  predict(object, newdata = test_set))

pred_ft_2 <- predictions_2 %>% as_tibble() %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_all(., factor, levels = c("M" , "B"))


# Confusion Matrix for Prep_2
CFM_Prep_2 <- sapply(pred_ft_2 , function(object) {
  CFM <- confusionMatrix(data = object,  reference = test_set$diagnosis)
  list(CFM)
}) 

CFM_mess_2 <- CFM_Prep_2 %>% unlist() %>% as.data.frame() 

CFM_2_Keys <- bind_cols(mod_names, 
                        Accuracy = round(as.numeric(as.character(CFM_mess_2[ACC_dex,])), 4), 
                        F_Measure = round(as.numeric(as.character(CFM_mess_2[F1_dex,])), 4)
) %>%
  mutate(Total = Accuracy + F_Measure) 

CFM_2_Keys %>% arrange(desc(Total)) %>% head(n = 7) %>% 
  knitr::kable(caption = "Run One: Prep_2: Top Seven Models")
#
#
######################################## Second Run : 82/18  ############################
set.seed(2019)
garbage_3 <- capture.output(
fits_3.0 <- lapply(models, function(model){ 
  print(model)
  train(diagnosis ~ ., data = train_2,  method = model,
        trControl = myControl, preProcess = prep_0)
}) 
)
names(fits_3.0) <- models

# Predictions
predictions_3.0 <- sapply(fits_3.0, function(object) 
  predict(object, newdata = test_2))

pred_ft_3.0 <- predictions_3.0 %>% as_tibble() %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_all(., factor, levels = c("M", "B"))

# Confusion Matrix for Prep_0
CFM_Prep_3.0  <- sapply(pred_ft_3.0 , function(object) {
  CFM <- confusionMatrix(data = object,  reference = test_2$diagnosis)
  list(CFM)
}) 

CFM_mess_3.0 <- CFM_Prep_3.0 %>% unlist() %>% as.data.frame() 

CFM_3.0_Keys <- bind_cols(mod_names, 
                          Accuracy = round(as.numeric(as.character(CFM_mess_3.0[ACC_dex,])), 4), 
                          F_Measure = round(as.numeric(as.character(CFM_mess_3.0[F1_dex,])), 4) 
) %>%
  mutate(Total = Accuracy + F_Measure) 


CFM_3.0_Keys %>% arrange(desc(Total)) %>% head(n = 7) %>% 
  knitr::kable(caption = "Run Two: NULL prep: Top Seven Models")
#  
##  Prep_1 model center, scale
#
set.seed(2019)
garbage_3.1 <- capture.output(
fits_3.1 <- lapply(models, function(model){ 
  print(model)
  train(diagnosis ~ ., data = train_2,  method = model,
        trControl = myControl, preProcess = prep_1)
}) 
)

names(fits_3.1) <- models

# Predictions Prep_1
predictions_3.1 <- sapply(fits_3.1, function(object) 
  predict(object, newdata = test_2))

pred_ft_3.1 <- predictions_3.1 %>% as_tibble() %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_all(., factor, levels = c("M", "B"))

# Confusion Matrix for Prep_1
CFM_Prep_3.1  <- sapply(pred_ft_3.1 , function(object) {
  CFM <- confusionMatrix(data = object,  reference = test_2$diagnosis)
  list(CFM)
}) 

CFM_mess_3.1 <- CFM_Prep_3.1 %>% unlist() %>% as.data.frame() 

CFM_3.1_Keys <- bind_cols(mod_names, 
                          Accuracy = round(as.numeric(as.character(CFM_mess_3.1[ACC_dex,])), 4) , 
                          F_Measure = round(as.numeric(as.character(CFM_mess_3.1[F1_dex,])), 4) 
) %>%
  mutate(Total = Accuracy + F_Measure) 

CFM_3.1_Keys %>% arrange(desc(Total)) %>% head(n = 7) %>% 
  knitr::kable(caption = "Run Two: Prep_1: Top Seven Models" )

#rm(CFM_mess_3.1)  ## Clean up the mess
#rm(predictions_3.1)
##

#  
##  Prep_2 model nzv, center, scale, pca
#
set.seed(2019)
garbage_3.2 <- capture.output(
fits_3.2 <- lapply(models, function(model){ 
  print(model)
  train(diagnosis ~ ., data = train_2,  method = model,
        trControl = myControl, preProcess = prep_2)
}) 
)

names(fits_3.2) <- models

# Predictions Prep_2
predictions_3.2 <- sapply(fits_3.2, function(object) 
  predict(object, newdata = test_2))

pred_ft_3.2 <- predictions_3.2 %>% as_tibble() %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_all(., factor, levels = c("M", "B"))

# Confusion Matrix for Prep_2
CFM_Prep_3.2  <- sapply(pred_ft_3.2 , function(object) {
  CFM <- confusionMatrix(data = object,  reference = test_2$diagnosis)
  list(CFM)
}) 

CFM_mess_3.2 <- CFM_Prep_3.2 %>% unlist() %>% as.data.frame() 

CFM_3.2_Keys <- bind_cols(mod_names, 
                          Accuracy = round(as.numeric(as.character(CFM_mess_3.2[ACC_dex,])), 4) , 
                          F_Measure = round(as.numeric(as.character(CFM_mess_3.2[F1_dex,])), 4) 
) %>%
  mutate(Total = Accuracy + F_Measure) 

#
CFM_3.2_Keys %>% arrange(desc(Total)) %>% head(n = 7) %>% 
  knitr::kable(caption = "Run Two: Prep_2: Top Seven Models")
#



rm(garbage_0, garbage_1, garbage_2, garbage_3, garbage_3.1, garbage_3.2)
################ END Modelling ####################################################

###############################  Results  #########################################

#
# Run One: 50/50

Accuracy_Table_1 <- bind_cols(Model = mod_names, 
                              Acc_0 = CFM_0_Keys$Accuracy, 
                              F1_0 = CFM_0_Keys$F_Measure, 
                              Acc_1 = CFM_1_Keys$Accuracy, 
                              F1_1 = CFM_1_Keys$F_Measure, 
                              Acc_2 = CFM_2_Keys$Accuracy, 
                              F1_2 = CFM_2_Keys$F_Measure) %>% 
  mutate(Top_PreProcess = (Acc_1  + Acc_2) / 2, 
         Top_Overall = (Acc_0  + Acc_1  + Acc_2) / 3) 


## Averages
h_line_Acc_0 <- mean(Accuracy_Table_1$Acc_0)
h_line1_Acc_1 <- mean(Accuracy_Table_1$Acc_1)
h_line2_Acc_2 <- mean(Accuracy_Table_1$Acc_2)

Accuracy_Run_One_Viz <- Accuracy_Table_1 %>% 
  ggplot(aes(Model, Acc_0)) +
  geom_jitter(color = "red", alpha = 0.6, width = 0.44, height = -0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_jitter(aes(y = Acc_1),  color = "blue",  
              alpha = 0.6, width = 0.5, height = 0) + 
  geom_jitter(aes(y = Acc_2), color = "green" , alpha = 0.6, width = 0.44, height = 0) +
  geom_hline(yintercept = h_line1_Acc_1, linetype = 2, color = "blue", alpha = 0.3) +
  geom_hline(yintercept = h_line_Acc_0, linetype = 2, color = "red", alpha = 0.3) +
  geom_hline(yintercept = h_line2_Acc_2, linetype = 2, color = "green", alpha = 0.5) +
  labs(title = "All Models: Accuracy Scores: 50/50 Split", 
       subtitle = "Prep by color: Red 0; Blue 1; Green 2",
       y = "Accuracy Rate", caption = "H-lines = Prep avg.")

Accuracy_Run_One_Viz # includes outlier mlp prep_0

## Replot
Accuracy_Table_1a <- Accuracy_Table_1 
Accuracy_Table_1a$Acc_0[10] <- NA # induce NA to remove mlp outlier
h_line_Acc_0_check <- mean(Accuracy_Table_1a$Acc_0, na.rm = TRUE) # without MLP_0

Accuracy_Run_One_reViz <- Accuracy_Table_1a  %>% 
  ggplot(aes(Model, Acc_0)) +
  geom_jitter(color = "red", alpha = 0.6, width = 0.4, height = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_jitter(aes(y = Acc_1),  color = "blue",  
              alpha = 0.6, width = 0.5, height = 0) + 
  geom_jitter(aes(y = Acc_2), color = "green", alpha = 0.6, width = 0.4, height = 0) +
  geom_hline(yintercept = h_line_Acc_0, linetype = 2, color = "red", alpha = 0.3) +
  geom_hline(yintercept = h_line1_Acc_1, linetype = 2, color = "blue", alpha = 0.3) +
  geom_hline(yintercept = h_line2_Acc_2, linetype = 2, color = "green", alpha = 0.5) +
  geom_hline(yintercept = h_line_Acc_0_check , linetype = 2, color = "orange", alpha = 0.5) +
  labs(title = "All Models: Accuracy Scores: 50/50 Split", 
       subtitle = "Prep by color: Red 0; Blue 1; Green 2",
       y = "Accuracy Rate", 
       caption = "H-lines = Prep avg.; Prep_0 for MLP not plotted: 0.6281")

Accuracy_Run_One_reViz # removes outlier mlp prep_0


## Top Seven Models Per Prep, Run One
Top_Seven_0 <- Accuracy_Table_1 %>% arrange(desc(Acc_0)) %>% 
  select(Model_0 = Model, Prep_Null = Acc_0) %>% slice(1:7)
Top_Seven_1 <- Accuracy_Table_1 %>% arrange(desc(Acc_1)) %>% 
  select(Model_1 = Model, Prep_1 = Acc_1) %>% slice(1:7)
Top_Seven_2 <- Accuracy_Table_1 %>% arrange(desc(Acc_2)) %>% 
  select(Model_2 = Model, Prep_2 = Acc_2) %>% slice(1:7) 
Top_Overall <- Accuracy_Table_1 %>% arrange(desc(Top_Overall)) %>% 
  select(Model_Overall = Model, Avg_Acc = Top_Overall) %>% slice(1:7) 
Top_Seven_50_Split <- bind_cols(Top_Seven_0, Top_Seven_1, 
                                Top_Seven_2, Top_Overall)

Top_Seven_50_Split %>% 
  knitr::kable(caption = "Run One: Top Seven Models by Accuracy per Prep") 

Top_Seven_50_Split %>% 
  summarize(Avg_Prep_0 = mean(Prep_Null), 
            Avg_Prep_1 = mean(Prep_1), 
            Avg_Prep_2 = mean(Prep_2)) %>% 
  knitr::kable(caption = "Run One: Mean Accuracy by Prep for Top Seven Models")

###################### END Run One

#
# Run Two: 82/18

Accuracy_Table_2 <- bind_cols(Model = mod_names, 
                              Acc_3.0 = CFM_3.0_Keys$Accuracy, 
                              F1_3.0 = CFM_3.0_Keys$F_Measure, 
                              Acc_3.1 = CFM_3.1_Keys$Accuracy,  
                              F1_3.1 = CFM_3.1_Keys$F_Measure, 
                              Acc_3.2 = CFM_3.2_Keys$Accuracy, 
                              F1_3.2 = CFM_3.2_Keys$F_Measure) %>% 
  mutate(Top_PreProcess = (Acc_3.1  + Acc_3.2) / 2, 
         Top_Overall = (Acc_3.0  + Acc_3.1  + Acc_3.2) / 3) 

# Remove mlp NULL for chart
Accuracy_Table_2a <- Accuracy_Table_2 
Accuracy_Table_2a$Acc_3.0[10] <- NA # induce NA

h_line_Acc_3.0 <- mean(Accuracy_Table_2$Acc_3.0) 
h_line_Acc_3.1 <- mean(Accuracy_Table_2$Acc_3.1)
h_line_Acc_3.2 <- mean(Accuracy_Table_2$Acc_3.2)
h_line_check_3.0 <- mean(Accuracy_Table_2a$Acc_3.0, 
                     na.rm = TRUE ) # remove mlp 


Accuracy_Run_Two_reViz <- Accuracy_Table_2a  %>% 
  ggplot(aes(Model, Acc_3.0)) +
  geom_jitter(color = "red", alpha = 0.6, width = 0.4, height = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_jitter(aes(y = Acc_3.1),  color = "blue",  
              alpha = 0.6, width = 0.5, height = 0) + 
  geom_jitter(aes(y = Acc_3.2), color = "green", alpha = 0.6, width = 0.4, height = 0) +
  geom_hline(yintercept = h_line_Acc_3.0, linetype = 2, color = "red", alpha = 0.3) +
  geom_hline(yintercept = h_line_Acc_3.1, linetype = 2, color = "blue", alpha = 0.3) +
  geom_hline(yintercept = h_line_Acc_3.2, linetype = 2, color = "green", alpha = 0.5) +
  geom_hline(yintercept = h_line_check_3.0, linetype = 2, color = "orange", alpha = 0.5) +
  labs(title = "All Models: Accuracy Scores: 82/18 Split", 
       subtitle = "Prep by color: Red 3.0; Blue 3.1; Green 3.2",
       y = "Accuracy Rate", 
       caption = "H-lines = Prep avg.; Prep_3.0 for MLP not plotted: 0.6281")

Accuracy_Run_Two_reViz  # removes outlier mlp prep_0

# Top Seven Models Run Two
Top_Seven_3.0 <- Accuracy_Table_2 %>% arrange(desc(Acc_3.0)) %>% 
  select(Model_3.0 = Model, Prep_Null = Acc_3.0) %>% slice(1:7)
Top_Seven_3.1 <- Accuracy_Table_2 %>% arrange(desc(Acc_3.1)) %>% 
  select(Model_3.1 = Model, Prep_1 = Acc_3.1) %>% slice(1:7)
Top_Seven_3.2 <- Accuracy_Table_2 %>% arrange(desc(Acc_3.2)) %>% 
  select(Model_3.2 = Model, Prep_2 = Acc_3.2) %>% slice(1:7) 
Top_Overall_82 <- Accuracy_Table_2 %>% arrange(desc(Top_Overall)) %>% 
  select(Model_Overall = Model, Avg_Acc = Top_Overall) %>% slice(1:7)  
Top_Seven_82_Split <- bind_cols(Top_Seven_3.0, Top_Seven_3.1, 
                                Top_Seven_3.2, Top_Overall_82 )

Top_Seven_82_Split %>% 
  knitr::kable(caption = "Run Two: Top Seven Models by Accuracy per Prep") 

Top_Seven_82_Split %>% 
  summarize(Avg_3.0 = mean(Prep_Null), Avg_3.1 = mean(Prep_1), Avg_3.2 = mean(Prep_2)) %>% 
  knitr::kable(caption = "Run Two: Mean Accuracy by Prep for Top Seven Models")


###################### END Run Two


#
#
#  Comparing Run Results

Overall_Accuracy_Table <- bind_cols(
  Accuracy_Table_1 %>% 
    select(Model, starts_with("Acc")),
  Accuracy_Table_2 %>% 
    select(starts_with("Acc")) 
) %>% 
  mutate(Top_PreProcess = (Acc_1 + Acc_2 + Acc_3.1  + Acc_3.2) / 4, 
         Top_Overall = (Acc_0 + Acc_1 + Acc_2 + Acc_3.0  + Acc_3.1  + Acc_3.2) / 6) %>%
  arrange(desc(Top_Overall))

Overall_Accuracy_Table %>% slice(1:10) %>% rename("CS_to_PCA" = Top_PreProcess, "All_Preps" = Top_Overall) %>% 
  knitr::kable(caption = "All Runs/Preps: Top 10 Models for Accuracy")


#
###############################################################################
## SAVE POINT: 
## 
## NEXT: Failure Tables
###############################################################################




# Create Tables Holding ALL Prediction for each RUN.
# This will allow drill down per model for all conditions
###

T_Set_ID <- test_set_id %>% select(id, diagnosis) # grab id and true values
T2_ID <- test2_id %>% select(id, diagnosis) # ditto

All_Predictions_Run_One <- bind_cols(T_Set_ID, pred_ft_0, pred_ft_1, pred_ft_2)
All_Predictions_Run_One <- All_Predictions_Run_One %>%
  mutate(Percent = rowMeans2(as.matrix(All_Predictions_Run_One[3:65]) ==  test_set_id$diagnosis)) %>%
  select(id, diagnosis, Percent, everything() ) # observations ordered as in the test_set: 50/50

All_Predictions_Run_Two <- bind_cols(T2_ID , pred_ft_3.0, pred_ft_3.1, pred_ft_3.2)
All_Predictions_Run_Two <- All_Predictions_Run_Two %>%
  mutate(Percent = rowMeans2(as.matrix(All_Predictions_Run_Two[3:65]) ==  T2_ID$diagnosis)) %>%
  select(id, diagnosis, Percent, everything() ) # observations ordered as in test2: 82/18 


names_vector_one <- c("id", "diagnosis", "Percent", "adaboost_0", "avNNet_0", "gamboost_0", "gamLoess_0", "glm_0", 
                      "gbm_0", "knn_0", "kknn_0", "lda_0", "mlp_0",  "monmlp_0", "naive_bayes_0", "qda_0", "ranger_0", 
                      "Rborist_0", "rf_0", "rpart_0",  "svmLinear_0", "svmRadial_0",  "svmRadialCost_0", "svmRadialSigma_0",   
                      "adaboost_1", "avNNet_1", "gamboost_1", "gamLoess_1", "glm_1", "gbm_1", "knn_1",  "kknn_1", "lda_1", 
                      "mlp_1",  "monmlp_1", "naive_bayes_1", "qda_1",  "ranger_1", "Rborist_1", "rf_1", "rpart_1" ,"svmLinear_1",
                      "svmRadial_1", "svmRadialCost_1", "svmRadialSigma_1", "adaboost_2", "avNNet_2", "gamboost_2", "gamLoess_2",
                      "glm_2", "gbm_2", "knn_2", "kknn_2", "lda_2", "mlp_2",  "monmlp_2", "naive_bayes_2", "qda_2", "ranger_2" , 
                      "Rborist_2", "rf_2", "rpart_2", "svmLinear_2", "svmRadial_2",  "svmRadialCost_2", "svmRadialSigma_2")


names(All_Predictions_Run_One)[1:66] <- names_vector_one


mean(All_Predictions_Run_One$Percent == 1)
mean(All_Predictions_Run_One$Percent > 0.625)


names_vector_two <- c("id", "diagnosis", "Percent","adaboost_3.0", "avNNet_3.0", "gamboost_3.0", "gamLoess_3.0", "glm_3.0", "gbm_3.0", "knn_3.0", "kknn_3.0", 
                      "lda_3.0", "mlp_3.0", "monmlp_3.0", "naive_bayes_3.0", "qda_3.0", "ranger_3.0", "Rborist_3.0", "rf_3.0", 
                      "rpart_3.0", "svmLinear_3.0", "svmRadial_3.0", "svmRadialCost_3.0", "svmRadialSigma_3.0", "adaboost_3.1", 
                      "avNNet_3.1", "gamboost_3.1", "gamLoess_3.1", "glm_3.1", "gbm_3.1", "knn_3.1", "kknn_3.1", "lda_3.1",
                      "mlp_3.1", "monmlp_3.1", "naive_bayes_3.1", "qda_3.1", "ranger_3.1", "Rborist_3.1", "rf_3.1", "rpart_3.1", 
                      "svmLinear_3.1", "svmRadial_3.1", "svmRadialCost_3.1", "svmRadialSigma_3.1", "adaboost_3.2", "avNNet_3.2", 
                      "gamboost_3.2", "gamLoess_3.2", "glm_3.2", "gbm_3.2", "knn_3.2", "kknn_3.2", "lda_3.2", "mlp_3.2", 
                      "monmlp_3.2", "naive_bayes_3.2", "qda_3.2", "ranger_3.2", "Rborist_3.2", "rf_3.2", "rpart_3.2", 
                      "svmLinear_3.2", "svmRadial_3.2", "svmRadialCost_3.2", "svmRadialSigma_3.2") 

names(All_Predictions_Run_Two)[1:66] <- names_vector_two

mean(All_Predictions_Run_Two$Percent == 1)
mean(All_Predictions_Run_Two$Percent > 0.625)


Obvious_Cases <- full_join(All_Predictions_Run_One %>% 
                             filter(Percent == 1) %>% select(id), 
                           All_Predictions_Run_Two %>% 
                             filter(Percent == 1) %>% select(id), by = "id") %>% 
  left_join(wdbc_data, by = "id")

table(Obvious_Cases$diagnosis) # 168 uncontroversial: all benign


Obvious_Cases2 <- full_join(All_Predictions_Run_One %>% select(-mlp_0) %>%
                              filter(Percent == 1) %>% select(id), 
                            All_Predictions_Run_Two %>%  select(-mlp_3.0) %>%
                              filter(Percent == 1) %>% select(id), by = "id") %>% 
  left_join(wdbc_data, by = "id")

table(Obvious_Cases2$diagnosis) # 168 uncontroversial: all benign




#############    First Run 50/50 Split  ##################################
#
## Do the individual Preps step by step

################################


###  Prep = NULL
# identify failure points
Fail_0 <- rowMeans2(predictions_0 == test_set_id$diagnosis) 
# create index and success percentage
Fail_Dex_0 <- bind_cols(dex = which(Fail_0 < 1), 
                        percent = Fail_0[Fail_0  < 1] )
# create table
Fail_Table_0 <- bind_cols(Fail_Dex_0, pred_ft_0[Fail_Dex_0$dex, ], 
                          T_Set_ID[Fail_Dex_0$dex, ] ) %>% 
  select(id, diagnosis, percent, everything() , -dex) %>% 
  arrange(percent)

names_fail_0 <- c("id", "diagnosis", "Percent", "adaboost_0", "avNNet_0", "gamboost_0", "gamLoess_0", "glm_0", 
                  "gbm_0", "knn_0", "kknn_0", "lda_0", "mlp_0",  "monmlp_0", "naive_bayes_0", "qda_0", "ranger_0", 
                  "Rborist_0", "rf_0", "rpart_0",  "svmLinear_0", "svmRadial_0",  "svmRadialCost_0", "svmRadialSigma_0")

names(Fail_Table_0)[1:24] <- names_fail_0 

str(Fail_Table_0) 

Fail_Table_0[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Fail Table: 50/50 split: Null Prep: First 8 rows/cols" )


# Prep_1 = center, scale
Fail_1 <- rowMeans2(predictions_1 == test_set_id$diagnosis) 
# create index and success percentage
Fail_Dex_1 <- bind_cols(dex = which(Fail_1 < 1), 
                        percent = Fail_1[Fail_1  < 1] )
# create table
Fail_Table_1 <- bind_cols(Fail_Dex_1, pred_ft_1[Fail_Dex_1$dex, ], 
                          T_Set_ID[Fail_Dex_1$dex, ] ) %>% 
  select(id, diagnosis, percent, everything() , -dex) %>% 
  arrange(percent)

names_fail_1 <- c("id", "diagnosis", "Percent", "adaboost_1", "avNNet_1", "gamboost_1", "gamLoess_1", "glm_1", 
                  "gbm_1", "knn_1", "kknn_1", "lda_1", "mlp_1",  "monmlp_1", "naive_bayes_1", "qda_1", "ranger_1", 
                  "Rborist_1", "rf_1", "rpart_1",  "svmLinear_1", "svmRadial_1",  "svmRadialCost_1", "svmRadialSigma_1")


names(Fail_Table_1)[1:24] <- names_fail_1
str(Fail_Table_1) 

Fail_Table_1[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Fail Table: 50/50 split: Prep_1: First 8 rows/cols" )



# Prep_2 = nzv, center, scale, pca
Fail_2 <- rowMeans2(predictions_2 == test_set_id$diagnosis) 
# create index and success percentage
Fail_Dex_2 <- bind_cols(dex = which(Fail_2 < 1), 
                        percent = Fail_2[Fail_2  < 1] )
# create table
Fail_Table_2 <- bind_cols(Fail_Dex_2, pred_ft_2[Fail_Dex_2$dex, ], 
                          T_Set_ID[Fail_Dex_2$dex, ] ) %>% 
  select(id, diagnosis, percent, everything() , -dex) %>% 
  arrange(percent)

names_fail_2 <- c("id", "diagnosis", "Percent", "adaboost_2", "avNNet_2", "gamboost_2", "gamLoess_2", "glm_2", 
                  "gbm_2", "knn_2", "kknn_2", "lda_2", "mlp_2",  "monmlp_2", "naive_bayes_2", "qda_2", "ranger_2", 
                  "Rborist_2", "rf_2", "rpart_2",  "svmLinear_2", "svmRadial_2",  "svmRadialCost_2", "svmRadialSigma_2")

names(Fail_Table_2)[1:24] <- names_fail_2
str(Fail_Table_2) 

Fail_Table_2[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Fail Table: 50/50 split: Prep_2: First 8 rows/cols" )

#
###################  82 / 18 Split #####################

# Prep = NULL
Fail_3.0 <- rowMeans2(predictions_3.0 == test2_id$diagnosis) 

Fail_Dex_3.0 <- bind_cols(dex = which(Fail_3.0 < 1), 
                          percent = Fail_3.0[Fail_3.0  < 1] )

Fail_Table_3.0 <- bind_cols(Fail_Dex_3.0, pred_ft_3.0[Fail_Dex_3.0$dex, ], 
                            T2_ID[Fail_Dex_3.0$dex, ] ) %>% 
  select(id, diagnosis, percent, everything() , -dex) %>% 
  arrange(percent)

names_fail_3.0 <- c("id", "diagnosis", "Percent", "adaboost_3.0", "avNNet_3.0", "gamboost_3.0", "gamLoess_3.0", "glm_3.0", 
                    "gbm_3.0", "knn_3.0", "kknn_3.0", "lda_3.0", "mlp_3.0",  "monmlp_3.0", "naive_bayes_3.0", "qda_3.0", "ranger_3.0", 
                    "Rborist_3.0", "rf_3.0", "rpart_3.0",  "svmLinear_3.0", "svmRadial_3.0",  "svmRadialCost_3.0", "svmRadialSigma_3.0") 


names(Fail_Table_3.0)[1:24] <- names_fail_3.0
str(Fail_Table_3.0) 

Fail_Table_3.0[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Fail Table: 82/18 split: Prep_3.0: First 8 rows/cols" )


# Prep_1 = center, scale
Fail_3.1 <- rowMeans2(predictions_3.1 == test2_id$diagnosis) 

Fail_Dex_3.1 <- bind_cols(dex = which(Fail_3.1 < 1), 
                          percent = Fail_3.1[Fail_3.1  < 1] )

Fail_Table_3.1 <- bind_cols(Fail_Dex_3.1, pred_ft_3.1[Fail_Dex_3.1$dex, ], 
                            T2_ID[Fail_Dex_3.1$dex, ] ) %>% 
  select(id, diagnosis, percent, everything() , -dex) %>% 
  arrange(percent)


names_fail_3.1 <- c("id", "diagnosis", "Percent", "adaboost_3.1", "avNNet_3.1", "gamboost_3.1", "gamLoess_3.1", "glm_3.1", 
                    "gbm_3.1", "knn_3.1", "kknn_3.1", "lda_3.1", "mlp_3.1",  "monmlp_3.1", "naive_bayes_3.1", "qda_3.1", "ranger_3.1", 
                    "Rborist_3.1", "rf_3.1", "rpart_3.1",  "svmLinear_3.1", "svmRadial_3.1",  "svmRadialCost_3.1", "svmRadialSigma_3.1") 


names(Fail_Table_3.1)[1:24] <- names_fail_3.1
str(Fail_Table_3.1) 

Fail_Table_3.1[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Fail Table: 82/18 split: Prep_3.1: First 8 rows/cols" )




# Prep_2 = nzv, center, scale, pca
Fail_3.2 <- rowMeans2(predictions_3.2 == test2_id$diagnosis) 

Fail_Dex_3.2 <- bind_cols(dex = which(Fail_3.2 < 1), 
                          percent = Fail_3.2[Fail_3.2  < 1] )

Fail_Table_3.2 <- bind_cols(Fail_Dex_3.2, pred_ft_3.2[Fail_Dex_3.2$dex, ], 
                            T2_ID[Fail_Dex_3.2$dex, ] ) %>% 
  select(id, diagnosis, percent, everything() , -dex) %>% 
  arrange(percent)


names_fail_3.2 <- c("id", "diagnosis", "Percent", "adaboost_3.2", "avNNet_3.2", "gamboost_3.2", "gamLoess_3.2", "glm_3.2", 
                    "gbm_3.2", "knn_3.2", "kknn_3.2", "lda_3.2", "mlp_3.2",  "monmlp_3.2", "naive_bayes_3.2", "qda_3.2", "ranger_3.2", 
                    "Rborist_3.2", "rf_3.2", "rpart_3.2",  "svmLinear_3.2", "svmRadial_3.2",  "svmRadialCost_3.2", "svmRadialSigma_3.2") 


names(Fail_Table_3.2)[1:24] <- names_fail_3.2
str(Fail_Table_3.2) 

Fail_Table_3.2[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Fail Table: 82/18 split: Prep_3.2: First 8 rows/cols" )


## Clean up environment
rm(Fail_0, Fail_Dex_0, Fail_1, Fail_Dex_1, Fail_2, Fail_Dex_2,
   Fail_3.0, Fail_Dex_3.0, Fail_3.1, Fail_Dex_3.1, Fail_3.2, Fail_Dex_3.2, 
   names_fail_0, names_fail_1, names_fail_2, names_fail_3.0, names_fail_3.1, names_fail_3.2)

#
# SAVE POINT
# 
#

###########  Fails Tables Per Entire Run, and Preps Across Runs  ####
#### 50/50 Split Run : all preps 
Common_Run_One <- intersect(Fail_Table_0$id, Fail_Table_1$id) %>% intersect(Fail_Table_2$id) 

Common_One_Fail_Table <- bind_cols(Fail_Table_0 %>% filter(id %in% Common_Run_One),
                                   Fail_Table_1 %>% filter(id %in% Common_Run_One),
                                   Fail_Table_2 %>% filter(id %in% Common_Run_One)) %>%
  select(everything(), -c(Percent, id1, diagnosis1, Percent1,id2, diagnosis2, Percent2) ) 

Common_One_Fail_Table <- Common_One_Fail_Table %>% 
  mutate(Percent = rowMeans(as.matrix(Common_One_Fail_Table[3:65]) == Common_One_Fail_Table$diagnosis)) %>%
  select(id, diagnosis, Percent, everything() ) %>% arrange(Percent)


Common_One_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "50/50 Split: Failures Common to All Preps: First 8 rows/cols" )

#### 82/18 Split: all preps
Common_Run_Two <- intersect(Fail_Table_3.0$id, Fail_Table_3.1$id) %>% intersect(Fail_Table_3.2$id) 

Common_Two_Fail_Table <- bind_cols(Fail_Table_3.0 %>% filter(id %in% Common_Run_Two),
                                   Fail_Table_3.1 %>% filter(id %in% Common_Run_Two),
                                   Fail_Table_3.2 %>% filter(id %in% Common_Run_Two)) %>%
  select(everything(), -c(Percent, id1, diagnosis1, Percent1,
                          id2, diagnosis2, Percent2) ) 

Common_Two_Fail_Table <- Common_Two_Fail_Table %>% 
  mutate(Percent = rowMeans(as.matrix(Common_Two_Fail_Table[3:65]) == Common_Two_Fail_Table$diagnosis)) %>%
  select(id, diagnosis, Percent, everything()  ) %>% arrange(Percent)


Common_Two_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "82/18 Split: Failures Common to All Preps: First 8 rows/cols" )

table(Common_One_Fail_Table$diagnosis)
table(Common_Two_Fail_Table$diagnosis)

##### Tables per prep across splits
# Prep = NULL
Null_Run_Fails <- intersect(Fail_Table_0$id, Fail_Table_3.0$id)
Prep_Null_Fail_Table <- bind_cols(Fail_Table_0 %>% filter(id %in% Null_Run_Fails  ),
                                  Fail_Table_3.0 %>% filter(id %in% Null_Run_Fails ) ) %>%
  select(everything(), -c(id1, diagnosis1) ) %>% 
  mutate(Null_percent = (Percent + Percent1) / 2 ) %>%
  select(id, diagnosis, Null_percent, everything(), -c(Percent, Percent1)  ) %>% 
  arrange(Null_percent)

Prep_Null_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Null Prep Fails: Both Splits: First 8 rows/cols" )


# Prep_1 = center, scale
Prep1_Run_Fails <- intersect(Fail_Table_1$id, Fail_Table_3.1$id)

Prep1_Fail_Table <- bind_cols(Fail_Table_1 %>% filter(id %in% Prep1_Run_Fails),
                              Fail_Table_3.1 %>% filter(id %in% Prep1_Run_Fails) ) %>%
  select(everything(), -c(id1, diagnosis1) ) %>% 
  mutate(Prep1_percent = (Percent + Percent1) / 2 ) %>%
  select(id, diagnosis, Prep1_percent, everything(), -c(Percent, Percent1)  ) %>% 
  arrange(Prep1_percent)

Prep1_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Prep_1 Fails: Both Splits: First 8 rows/cols" )


# Prep_2 = nzv, center, scale, pca
Prep2_Run_Fails <- intersect(Fail_Table_2$id, Fail_Table_3.2$id)

Prep2_Fail_Table <- bind_cols(Fail_Table_2 %>% filter(id %in% Prep2_Run_Fails  ),
                              Fail_Table_3.2 %>% filter(id %in% Prep2_Run_Fails  ) ) %>%
  select(everything(), -c(id1, diagnosis1) ) %>% 
  mutate(Prep2_percent = (Percent + Percent1) / 2 ) %>%
  select(id, diagnosis, Prep2_percent, everything(), -c(Percent, Percent1)  ) %>% 
  arrange(Prep2_percent)

Prep2_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Prep_2 Fails: Both Splits: First 8 rows/cols" )

############  Relist results

Prep_Null_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Null Prep Fails: Both Splits: First 8 rows/cols" )

Prep1_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Prep_1 Fails: Both Splits: First 8 rows/cols" )

Prep2_Fail_Table[,1:8] %>% head(8) %>%  
  knitr::kable(caption = "Prep_2 Fails: Both Splits: First 8 rows/cols" )

######
###      Failures common to all runs, all preps (but not all models!): the intersection
######

Overall_Run_Fails <- intersect(Common_Run_One , Common_Run_Two )

Overall_Fail_Table_Long <- bind_cols(Common_One_Fail_Table %>% filter(id %in% Overall_Run_Fails ),
                                     Common_Two_Fail_Table %>% filter(id %in% Overall_Run_Fails ))  %>% 
  select(everything(), -c(Percent, id1,diagnosis1, Percent1)) 

Overall_Fail_Table_Long <- Overall_Fail_Table_Long %>% 
  mutate(Percent = rowMeans2(as.matrix(Overall_Fail_Table_Long[3:128]) == Overall_Fail_Table_Long$diagnosis)) %>% 
  select(id, diagnosis, Percent, everything())

names_vector_long <- c("id", "diagnosis", "Percent", "adaboost_0", "avNNet_0", "gamboost_0", "gamLoess_0", "glm_0", 
                       "gbm_0", "knn_0", "kknn_0", "lda_0", "mlp_0",  "monmlp_0", "naive_bayes_0", "qda_0", "ranger_0", 
                       "Rborist_0", "rf_0", "rpart_0",  "svmLinear_0", "svmRadial_0",  "svmRadialCost_0", "svmRadialSigma_0",   
                       "adaboost_1", "avNNet_1", "gamboost_1", "gamLoess_1", "glm_1", "gbm_1", "knn_1",  "kknn_1", "lda_1", 
                       "mlp_1",  "monmlp_1", "naive_bayes_1", "qda_1",  "ranger_1", "Rborist_1", "rf_1", "rpart_1" ,"svmLinear_1",
                       "svmRadial_1", "svmRadialCost_1", "svmRadialSigma_1", "adaboost_2", "avNNet_2", "gamboost_2", "gamLoess_2",
                       "glm_2", "gbm_2", "knn_2", "kknn_2", "lda_2", "mlp_2",  "monmlp_2", "naive_bayes_2", "qda_2", "ranger_2" , 
                       "Rborist_2", "rf_2", "rpart_2", "svmLinear_2", "svmRadial_2",  "svmRadialCost_2", "svmRadialSigma_2", 
                       "adaboost_3.0", "avNNet_3.0", "gamboost_3.0", "gamLoess_3.0", "glm_3.0", "gbm_3.0", "knn_3.0", "kknn_3.0", 
                       "lda_3.0", "mlp_3.0", "monmlp_3.0", "naive_bayes_3.0", "qda_3.0", "ranger_3.0", "Rborist_3.0", "rf_3.0", 
                       "rpart_3.0", "svmLinear_3.0", "svmRadial_3.0", "svmRadialCost_3.0", "svmRadialSigma_3.0", "adaboost_3.1", 
                       "avNNet_3.1", "gamboost_3.1", "gamLoess_3.1", "glm_3.1", "gbm_3.1", "knn_3.1", "kknn_3.1", "lda_3.1",
                       "mlp_3.1", "monmlp_3.1", "naive_bayes_3.1", "qda_3.1", "ranger_3.1", "Rborist_3.1", "rf_3.1", "rpart_3.1", 
                       "svmLinear_3.1", "svmRadial_3.1", "svmRadialCost_3.1", "svmRadialSigma_3.1", "adaboost_3.2", "avNNet_3.2", 
                       "gamboost_3.2", "gamLoess_3.2", "glm_3.2", "gbm_3.2", "knn_3.2", "kknn_3.2", "lda_3.2", "mlp_3.2", 
                       "monmlp_3.2", "naive_bayes_3.2", "qda_3.2", "ranger_3.2", "Rborist_3.2", "rf_3.2", "rpart_3.2", 
                       "svmLinear_3.2", "svmRadial_3.2", "svmRadialCost_3.2", "svmRadialSigma_3.2" ) 

names(Overall_Fail_Table_Long)[1:129] <- names_vector_long


# Example
Overall_Fail_Table_Long[1:8 ,1:8 ] %>% 
  knitr::kable(caption = "Failures Common to All Ensemble Runs [but not all models!]")

# Data information
Overall_Fail_Table_Short <- bind_cols(Common_One_Fail_Table %>% 
                                        filter(id %in% Overall_Run_Fails ),
                                      Common_Two_Fail_Table %>% 
                                        filter(id %in% Overall_Run_Fails ) ) %>%
  mutate(Percent = Overall_Fail_Table_Long$Percent ) %>%
  select(id, Percent)  %>% left_join(wdbc_data, by = "id" ) %>% 
  select(everything()) %>% 
  arrange(Percent)

Overall_Fail_Table_Short[1:8, 1:8] %>% 
  knitr::kable(caption = "Common Failures by ID: The Data in Detail")

#
#
#
############################ Individual Models #########################################

## case Study GamLoes

GamLoess_Run_One <-  All_Predictions_Run_One %>%
  select(id,  diagnosis, starts_with("gamLoess"))

GamLoess_Run_Two <-  All_Predictions_Run_Two %>%
  select(id,  diagnosis, starts_with("gamLoess"))

GamLoess_Fails_One <- GamLoess_Run_One %>% 
  mutate(percent = rowMeans(as.matrix(GamLoess_Run_One[3:5]) == GamLoess_Run_One$diagnosis)) %>%
  select(id, diagnosis, percent, everything() ) %>%
  filter(percent < 1) %>%
  arrange(percent)

GamLoess_Fails_Two <- GamLoess_Run_Two %>% 
  mutate(percent = rowMeans(as.matrix(GamLoess_Run_Two[3:5]) == GamLoess_Run_Two$diagnosis)) %>%
  select(id, diagnosis, percent, everything() ) %>%
  filter(percent < 1) %>%
  arrange(percent)


GamLoess_Fails_One %>% head(8) %>% 
  knitr::kable(caption = "gamLoess Fails: Run One: [First 8]")

GamLoess_Fails_Two %>% head(8) %>% 
  knitr::kable(caption = "gamLoess Fails: Run Two: [3 total]")

gamLoess_Accuracy_Table <- bind_cols(Accuracy_Table_1 %>% 
                                      filter(Model == "gamLoess") %>% 
                                      select(Model, starts_with("Acc")), 
                                     Accuracy_Table_2 %>% 
                                      filter(Model == "gamLoess") %>% 
                                      select(starts_with("Acc")) )


gamLoess_Accuracy_Table %>% rename( "Model/Prep" = "Model") %>%
  knitr::kable(caption = "gamLoess Accuracy Scores by Prep & ID") 

gamLoess_names <- names(gamLoess_Accuracy_Table)[2:7]

by_Prep <- c("Fail Count") # create rowname_Colum
gamLoess_failures_by_prep <- bind_cols(enframe(by_Prep, name = NULL, value = "Model"),
                                       abs(round((gamLoess_Accuracy_Table[,2:4] * 285) - 285)), 
                                       abs(round((gamLoess_Accuracy_Table[,5:7] * 104) - 104)) ) # do not re-run unless restarting

gamLoess_failures_by_prep %>% 
  knitr::kable(caption = "gamLoess Failure Count by Prep")

gamLoess_Accuracy_Table[1,1] <- "Accuracy"

bind_rows(gamLoess_Accuracy_Table, gamLoess_failures_by_prep) %>% 
  rename( "gamLoess" = "Model") %>%
  knitr::kable(caption = "gamLoess Accuracy  & Fail Counts by Prep")


