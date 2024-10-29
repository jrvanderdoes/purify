#####
##    Libraries
#####
# Add for model or plotting

#####
##    Functions
#####
resampling <- function(X, iters, fn, alpha=0.05,
                      strat = F, replace=T, 
                      seed.value=NULL, silence=F, siz=NaN, ...){
  ## This is a basic resampling code
  #       Since I was the main user, not much error handling for bad inputs
  #     Inputs:
  #       X: Data
  #         dataframe, n rows of observations, p columns of observations
  #       iters: Number of bootstrap iterations
  #         numeric positive integer
  #       fn: Function to bootstrap
  #         function returning coefficients of model
  #       alpha: significance level for CI
  #         numeric, [0,1]
  #       strat: Indicates if the data should be stratified. Will need to code
  #         boolean
  #       replace: Indicates if bootstrapping or permutation. 
  #                Note, permutation with strat requires using minimum group size
  #         boolean
  #       silence: Indicates if code should output updates
  #         boolean
  #       seed.value: If is numeric, set seed value
  #         NULL or numeric
  #       siz: size of bootstrapping from each strata, if unspecified, it will compute itself
  #         NaN (default) or numeric
  #       ...: Additional arguments for fn
  #         ...
  
  ## Set Seed
  if(is.numeric(seed.value))
    set.seed(seed.value)
  ## Get Baseline value
  #   Also record and use time as a rough estimate
  st<-Sys.time()
  fullVal <- fn(X, ...)
  en<-Sys.time()
  
  ## Display (rough) estimated time for reference
  if(!silence){
    cat(paste0('Estimated time: ',
               round(difftime(en,st,'units'='mins')[[1]]*iters,2),
               ' mins\n'))
  }
  
  ## Get number of observations
  n <- nrow(X)
  
  ## Baseline sampling (May be overwritten if stratifying)
  #     Each column is a sample
  #     replace=T for bootstrap, replace=F for permutatation test
  #     Let me know if you need to account for dependence
  #       I have code with customizable blockSize
  #       However, often that is not applicable for stratification
  idxs <- as.data.frame(sapply(1:iters,function(i,n,replace=replace){
    sample(1:n,replace = replace)
  },n=n,replace = replace))
  
  ## Stratify data as needed
  #     I generalized this for you, but may be a few bugs
  #     Let me know if ou can't get through anything
  if(strat){
    # For ease, I reorganize that data for stratification
    # Get the individual strata
    strata <- unique(X$Strata)
    stratsSizes <- c()
    tmpX <- data.frame()
    for(i in 1:length(strata)){
      tmpX <- rbind(tmpX, X[X$Strata==strata[i],])
      stratsSizes[i] <- nrow(X[X$Strata==strata[i],])
    }
    
    X <- tmpX
    # cat(paste0("tmpX nrows:", nrow(X), "\n"))
    # cat(paste0("tmpX ncols:", ncol(X), "\n"))
    
    # Determine how much to sample
    #   siz indicates how much sampled from each strat
    #   Common choices: n/NumStrat, sizeMinStrat
    if(is.na(siz)){
      siz <- floor(nrow(X)/4) # floor(min(stratsSizes))
    }
    
    ## Display group sizes for reference
    if(!silence){
      outputString <- paste0('Groups (sel ',siz,')')
      for(i in 1:length(stratsSizes)){
        outputString <- paste0(outputString,' - ',stratsSizes[i])
      }
      cat(paste0(outputString,'\n'))
    }
    
    
    ## Stratified sampling
    #     Each column is a sample
    #     replace=T for bootstrap, replace=F for permutatation test
    idxs <- as.data.frame(sapply(1:iters,function(i,n,siz=siz,
                                                  replace=replace,
                                                  stratsSizes=stratsSizes){
      return_idxs <-c()
      stratsSizes_tmp <- c(0,cumsum(stratsSizes))
      for(i in 2:length(stratsSizes_tmp)){
        return_idxs <- c(return_idxs, 
                         sample((stratsSizes_tmp[i-1]+1):stratsSizes_tmp[i],
                                size=siz, replace = replace))
      }
      
      return_idxs
    },n=n,siz=siz,replace=replace, stratsSizes=stratsSizes))
    
  }
  
  ## Generate all BS results
  bssamples <- sapply(idxs,
                      function(loop_iter,fn,X1,...){ fn(X=X1[loop_iter,], ...) },
                      X1=X, fn=fn, simplify = F, ...)
  bssamples <- sapply(idxs,
                      function(loop_iter,fn,X1){ fn(X=X1[loop_iter,]) },
                      X1=X, fn=fn, simplify = F)
  ## Generate all BS samples
  #   This could be done in last step, but I forgot to add it
  #   This essentially takes no time, so just haven't optimized
  bsdata <- sapply(idxs,
                   function(loop_iter,X1){ X1[loop_iter,]},
                   X1=X,simplify = F)
  
  ## Organize individual bootstrapped estimates
  # bssamples
  estims <- data.frame('V1'=bssamples[[1]][1])
  estimsP <- data.frame('V1'=bssamples[[1]][2])
  estimsConfInt1 <- data.frame('V1'=bssamples[[1]][3])
  estimsConfInt2 <- data.frame('V1'=bssamples[[1]][4])
  
  for(i in 1:iters){
    #avgSample <- avgSample + bssamples[[i]]
    estims[,i] <- bssamples[[i]][1]
    estimsP[,i] <- bssamples[[i]][2]
    estimsConfInt1[,i] <- bssamples[[i]][3]
    estimsConfInt2[,i] <- bssamples[[i]][4]
  }

  ## Organize overall bootstrapped estimates
  meanW <- rowMeans(estims,na.rm = T)
  meanPvalue <- rowMeans(estimsP, na.rm = T)
  meanConfInt1 <- rowMeans(estimsConfInt1,na.rm = T)
  meanConfInt2 <- rowMeans(estimsConfInt2,na.rm = T)
  
  # ## I removed plotting code here
  # 
  ## Return the following list
  list(
    "BSresults"=list("meanW"=meanW, "meanPvalue"=meanPvalue,
                     "meanConfInt1"=meanConfInt1, "meanConfInt2"=meanConfInt2),
    'BSSamples'=bssamples,
    'BSData'=bsdata,
    'SummaryInfo'=list('estims'=estims, 'pvals'=estimsP,
                       'ConfInt1s'=estimsConfInt1, 'ConfInt2s'=estimsConfInt2))
}


# ================== Example Code Below =================== # 

#####
##    Example Code
#       Note, example data may not show results perfectly
#         It is difficult to generate good stratified data quickly
#         Instead my goal is to show input
#####

# modelFuction <- function(X){
#   reg <- lm(Y~Age+Gender+Noise1+Noise2, data=X)
#   ctable <- coef(summary(reg))
#   
#   # Return Coefficients, Std error, and p-value
#   ctable[,c(1:2,4)]
# }

# n <- 600
# fakeData <- data.frame('Y'=rep(NA,n),
#                          'Age'=rnorm(n, mean=30, sd=5),
#                          'Gender'=rbinom(n,1,0.9),
#                          'UnknownStrat'=rbinom(n,1,0.7),
#                          'Noise1'= rnorm(n),
#                          'Noise2'= rpois(n,5),
#                          'Strata'=NA)
# fakeData$Age <- ifelse(fakeData$Age<18,18, fakeData$Age)
# fakeData$Strata <- ifelse(fakeData$Gender & fakeData$UnknownStrat, 1,
#                           ifelse(fakeData$Gender & !fakeData$UnknownStrat, 2,
#                                  ifelse(!fakeData$Gender & fakeData$UnknownStrat, 3, 4)))
# table(fakeData$Strata) # Show groups sizes
# fakeData$Y <- 10+
#               0.1*fakeData$Age +
#               2*fakeData$Gender +
#               0.1*fakeData$Age*fakeData$Gender+
#               3*fakeData$UnknownStrat +
#               rnorm(n,sd=10)
# # See current fit (Model: Y~Int + Age + Gender + Noise1 + Noise2)
# modelFuction(fakeData)
# 
# # Bootstrap
# boot_results <- resampling(X=fakeData,iters=1000, fn=modelFuction, strat = T,
#                            seed.value=123)
# boot_results$BSResult
