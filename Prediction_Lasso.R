###################################################################################################################
#  This script contains the glmnet_function which uses the glmnet package for prediction
#
#  Parameters: 
#     1.) y: n x 1 matrix containing the values for the response variable
#     2.) x: n x m matrix containing predictor variables
#     3.) split: percent to split data into training set. 
#     4.) alpha: 0, 0.5, 1 for ridge regression, enet, or lasso
#     5.) penalty.factor (optional): vector of 0 and 1 to tell cv.glmnet which variables to apply shrinkage on
#                                     0 = no shrinkage and 1 = apply shrinkage
#
#
#  Author: Duy Pham
#  E-mail: dtpham@memphis.edu
###################################################################################################################
options(stringsAsFactors = FALSE)

library(glmnet)
library(dplyr)

glmnet_function <- function(y, x, split, alpha, penalty.factor = NULL){
   
      # Create index from 1:nrow(y). Used to split data into training and testing
      index <- 1:nrow(y)
  
      
      # If no penalty.factor is given, allow shrinkage on all variables
      if(is.null(penalty.factor)){
         penalty.factor = rep.int(1, ncol(x))
      }
  
  
      # Vector to store MSE and dataframe to store beta coefficients
      MSE <- numeric(length = 100)
      reg.coeff <- as.data.frame(matrix(NA, nrow = 100, ncol = ncol(x) + 1))
          
      
      
      
      # For-loop to iterate lasso regression 100 times
      for(i in 1:100){

          # Randomly select indices based on split percentage
          trainInd <- sample(index, length(index)*split)
            
          
          # Splitting data into training and testing
          train_y <- y[trainInd,, drop = FALSE]
          train_x <- x[trainInd,, drop = FALSE]
          test_y <-  y[index[-trainInd],,drop = FALSE]
          test_x <-  x[index[-trainInd],,drop = FALSE]
            
          
          
          # Fit a lasso regression. Use default cross-validation to find optimal lambda
          lasso.fit <- cv.glmnet(train_x,train_y, alpha = alpha, penalty.factor = penalty.factor)
            
          
          # Predict the test set
          lm.predict <- predict(lasso.fit,test_x, s=lasso.fit$lambda.1se)
            
            
          reg.coeff[i,] <- as.vector(coef(lasso.fit))
          
                
          # Compute the MSE and store into MSE
          MSE[i] <- mean((test_y - lm.predict)^2)
      
      }
      
      
      
      colnames(reg.coeff) <- dimnames(coef(lasso.fit))[[1]]
      cg_freqency <- colSums(reg.coeff != 0 )
      cg_freqency <- cg_freqency[order(cg_freqency, decreasing = TRUE)]
      
      
      return(list("MSE" = MSE, "coeffs" = reg.coeff, "best.cpg" = cg_freqency))

} #glmnet_function()





### Load in filtered data for age at menarche in female subjects:
#
#      period_dnam: 359 x 299
#      period_cellType: 359 x 8
#      period_data: 359 x 1
load('~/Menarache_Prediction_Data.RData')



### Creating x dataframes
#       period_dnam: 359 x 229 (already logit transformed)
#       bw: 354 x 1
#       bw_dnam: 354 x 230
#       bw_celltype_dnam: 354 x 236
#       celltype_dnam: 359 x 235
bw <- iow_female[complete.cases(iow_female$BIRTHWT),'BIRTHWT',drop = FALSE]

bw_dnam <- cbind(bw = iow_female$BIRTHWT, period_dnam)
bw_dnam <- bw_dnam[complete.cases(bw_dnam),]

bw_celltype_dnam <- cbind(bw = iow_female$BIRTHWT, period_cellType[,c('CD4T','NK','Bcell','Mono','Neu','Eos')], period_dnam)
bw_celltype_dnam <- bw_celltype_dnam[complete.cases(bw_celltype_dnam),]

celltype_dnam <- cbind(period_cellType[,c('CD4T','NK','Bcell','Mono','Neu','Eos')], period_dnam)




### Creating y dataframes
#     period_data: 359 x 1 
#     period_data_sub: 354 x 1, filtered for missing NAs in birth weight data
period_data_sub <- period_data[rownames(period_data) %in% rownames(bw), ,drop = FALSE]







### Lasso (L)
L_bw_dnam <- glmnet_function(as.matrix(period_data_sub), as.matrix(bw_dnam), 
                             split = .8, 
                             alpha = 1, 
                             penalty.factor = c(0,rep(1,ncol(bw_dnam) - 1)))

L_bw_celltype_dnam <- glmnet_function(as.matrix(period_data_sub), as.matrix(bw_celltype_dnam), 
                                      split = .8, 
                                      alpha = 1, 
                                      penalty.factor = c(0,0,0,0,0,0,0,rep(1,ncol(bw_celltype_dnam) - 7)))

L_dnam <- glmnet_function(as.matrix(period_data), as.matrix(period_dnam), 
                          split = .8, 
                          alpha = 1)


L_celltype_dnam <- glmnet_function(as.matrix(period_data), as.matrix(celltype_dnam), 
                                   split = .8, 
                                   alpha = 1, 
                                   penalty.factor = c(0,0,0,0,0,0,rep(1,ncol(celltype_dnam) - 6)))







### Elastic Net (E)
E_bw_dnam <- glmnet_function(as.matrix(period_data_sub), as.matrix(bw_dnam), 
                             split = .8, 
                             alpha = .5, 
                             penalty.factor = c(0,rep(1,ncol(bw_dnam) - 1)))


E_bw_celltype_dnam <- glmnet_function(as.matrix(period_data_sub), as.matrix(bw_celltype_dnam), 
                                      split = .8, 
                                      alpha = .5, 
                                      penalty.factor = c(0,0,0,0,0,0,0,rep(1,ncol(bw_celltype_dnam) - 7)))


E_dnam <- glmnet_function(as.matrix(period_data), as.matrix(period_dnam), 
                          split = .8, 
                          alpha = .5)


E_celltype_dnam <- glmnet_function(as.matrix(period_data), as.matrix(celltype_dnam), 
                                   split = .8, 
                                   alpha = .5, 
                                   penalty.factor = c(0,0,0,0,0,0,rep(1,ncol(celltype_dnam) - 6)))
