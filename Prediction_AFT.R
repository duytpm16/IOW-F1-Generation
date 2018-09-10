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
      
        # Use the randomly selected 80% row indices to get data for the training set
        train_surv_mat <- surv_mat[trainInd,]
        test_surv_mat <- surv_mat[index[-trainInd],]
    
        
        
        # Fit AFT model
        aft_fit <- survreg(Surv(resp, status) ~., data = surv_mat, dist = 'weibull')

        
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

