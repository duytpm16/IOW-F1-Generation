###################################################################################################################
#  This script contains the aft_prediction function which uses the survival package for prediction using the
#       AFT model.
#
#  Parameters: 
#     1.) var: name of the column for the response variable
#     2.) y: n x 1 matrix containing the values for the response variable. Must contain response variable and 'status' columns
#     3.) x: n x m matrix containing predictor variables
#     4.) split: percent to split data into training set. 
#
#
#  Author: Duy Pham
#  E-mail: dtpham@memphis.edu
###################################################################################################################
options(stringsAsFactors = FALSE)
library(survival)
library(dplyr)


aft_prediction <- function(var, y, x, split){
  
    # Empty MSE vector
    MSE <- numeric(length = 100)
    
    
    # Index vector to sample from
    index <- c(1:nrow(y))
    index_length <- length(index)
  
    
    # Changing response name
    surv_mat <- cbind(y,x)
    wh <- which(colnames(surv_mat) == var)
    colnames(surv_mat)[wh] <- 'resp'
    
    
    
    for(i in 1:100){
      
        # Randomly select split percentage from index
        trainInd <- sample(index, index_length * split)
      
        # Split data into training/testing based on split
        train_surv_mat <- surv_mat[trainInd,]
        test_surv_mat <- surv_mat[index[-trainInd],]
    
        
        
        # Fit AFT model
        aft_fit <- survreg(Surv(resp, status) ~., data = train_surv_mat, dist = 'weibull')

        
        # Predict using AFT model and testing set
        aft_predict <- predict(aft_fit, test_surv_mat %>% select(-resp, -status))
    
        
        
        MSE[i] <- mean((aft_predict-test_surv_mat[,'resp'])^2)
    }
    
    return(MSE)


} # aft_prediction()




### Load in filtered data for age at menarche in female subjects:
#
#      period_dnam: 359 x 229
#      period_cellType: 359 x 8
#      period_data: 359 x 1
#      iow_female: 359 x 9
load('~/Menarache_Prediction_Data.RData')




### Creating the y matrix
#
#      period_data: 359 x 2
period_data <- period_data %>% mutate(status = rep(1,nrow(.)))


### Creating a new x matrix
#
#    celltype_dnam: 359 x 235
celltype_dnam <- cbind(period_cellType[,c('CD4T','NK','Bcell','Mono','Neu','Eos')], period_dnam)





### AFT prediction
aft_dnam <- aft_prediction('AGEPERIODFEMALE_18', period_data, period_dnam, split = .8)
aft_celltype_dnam <- aft_prediction('AGEPERIODFEMALE_18', period_data, celltype_dnam, split = .8)


