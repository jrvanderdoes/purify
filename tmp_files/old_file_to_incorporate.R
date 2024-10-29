#####
##    Libraries
#####
library(MASS)
library(ggplot2)
#library(ordinal)

#####
##    Functions
#####

bootstrap <- function(X, iters, fn, alpha=0.05,
                      strat = F, strat.full=T, stat.amt=NA,
                      returnPlots=T, silence=F, ...){
  ## Data
  st<-Sys.time()
  fullVal <- fn(X, ...)
  en<-Sys.time()

  if(!silence){
    cat(paste0('Estimated time: ',
               round(difftime(en,st,'units'='mins')[[1]]*iters,2),
               ' mins\n'))
  }

  n <- nrow(X)
  idxs <- as.data.frame(sapply(1:iters,function(i,n){
    sample(1:n,replace = T)
  },n=n))

  if(strat){
    # 4 groups, V1S1, V1S2,V2S1, V2S2
    data1 <- X[X$Vaccine==0 & X$Survey==1,]
    data2 <- X[X$Vaccine==0 & X$Survey==2,]
    data3 <- X[X$Vaccine==1 & X$Survey==1,]
    data4 <- X[X$Vaccine==1 & X$Survey==2,]

    len1 <- nrow(data1)
    len2 <- nrow(data2)
    len3 <- nrow(data3)
    len4 <- nrow(data4)

    X <-rbind(data1,data2,data3,data4)

    if(strat.full){
      siz <- floor(nrow(X)/4)
    }else{
      if(!is.na(stat.amt)){
        siz <- floor(stat.amt)
      }else{
        siz <- floor(min(len1,len2,len3,len4))
      }
    }

    if(!silence){
      cat(paste0('Groups (sel ',siz,') : ',len1,' - ',
                 len2,' - ',len3,' - ',len4,'\n'))
    }

    idxs <- as.data.frame(sapply(1:iters,function(i,n,siz=siz){
      c(sample(1:len1,size=siz,replace = T),
        sample(len1+1:len2,size=siz,replace = T),
        sample(len2+1:len3,size=siz,replace = T),
        sample(len3+1:len4,size=siz,replace = T))
    },n=n,siz=siz))

  }

  bssamples <- sapply(idxs,
                      function(loop_iter,fn,X1,...){ fn(X=X1[loop_iter,], ...) },
                      X1=X, fn=fn, simplify = F, ...)
  bsdata <- sapply(idxs,
                   function(loop_iter,X1){ X1[loop_iter,]},
                   X1=X,simplify = F)
  #avgSample <- bssamples[[1]]
  estims <- data.frame('V1'=bssamples[[1]][,1])
  estimsSD <- data.frame('V1'=bssamples[[1]][,2])
  estimsP <- data.frame('V1'=bssamples[[1]][,4])
  for(i in 1:iters){
    #avgSample <- avgSample + bssamples[[i]]
    estims[,i] <- bssamples[[i]][,1]
    estimsSD[,i] <- bssamples[[i]][,2]
    estimsP[,i] <- bssamples[[i]][,4]
  }
  #avgSample <- avgSample/iters
  means <- rowMeans(estims,na.rm = T)
  sds <- apply(estims,1,function(x){sd(x)})
  estimSDs <- rowMeans(estimsSD,na.rm = T)

  if(returnPlots){
    surveyPlot <- ggplot(mapping=aes(x=as.numeric(estims[1,]))) +
      geom_histogram(mapping=aes(y=..density..),
                     binwidth=0.025,
                     alpha=.5, position="identity") +
      geom_density(alpha=.1) +
      geom_vline(aes(xintercept=mean(as.numeric(estims[1,])))) +
      ylab(NULL) +
      xlab(NULL)+
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))

    vaccinePlot <- ggplot(mapping=aes(x=as.numeric(estims[6,]))) +
      geom_histogram(mapping=aes(y=..density..),
                     binwidth=0.025,
                     alpha=.5, position="identity") +
      geom_density(alpha=.1) +
      geom_vline(aes(xintercept=mean(as.numeric(estims[6,])))) +
      ylab(NULL) +
      xlab(NULL)+
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))

    plots=list(surveyPlot,vaccinePlot)
  }else{
    plots=NA
  }


  list('BSResult'=data.frame('Ests'=exp(means),
                             'alpha'=alpha,
                             #'95Pval'=apply(estimsP, 1, function(x){quantile(x,c(0.95))}),
                             #'50Pval'=apply(estimsP, 1, function(x){quantile(x,c(0.5))}),
                             #'5Pval'=apply(estimsP, 1, function(x){quantile(x,c(0.05))}),
                             #'Low95'=exp(means - qnorm(1-alpha/2) * sds/iters),
                             #'Up95'=exp(means + qnorm(1-alpha/2) * sds/iters),
                             'Low95_s'=exp(means - qnorm(1-alpha/2) * estimSDs),
                             'Up95_s'=exp(means + qnorm(1-alpha/2) * estimSDs)#,
                             #'Low95_e'=apply(exp(estims),1,function(x){quantile(x,c(0.025))}),
                             #'Up95_e'=apply(exp(estims),1,function(x){quantile(x,c(0.975))})
  ),
  'BSSamples'=bssamples,
  'BSData'=bsdata,
  'SummaryInfo'=list('estims'=estims,
                     'pvals'=estimsP),
  'plots' = plots)
}


computeModel_NH <- function(X){
  #ordReg <- ordinal::clm(NeedleHurt ~ Survey + Age + SuggestionSel +
  #               LessPainSel + GenderOneSelections + Vaccine,
  #             data = X, link = "logit")
  ordReg <- ordinal::clm(NeedleHurt ~ Survey + Age + Male + #Other +
                           #Vaccine + #Q5R1 +
                           #Q5R1 +
                           #Q5R23 +
                           Survey:Age,
                         data = X, link = "logit")
  #ordReg <- polr( NeedleHurt ~ Survey + Age + SuggestionSel +
  #                  LessPainSel + GenderOneSelections + Vaccine,
  #                data = X,
  #                method = "logistic", Hess=TRUE)
  #ctable <- coef(summary(ordReg))[1:6,]
  #pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  #cbind(ctable, 'pval'=round(pvals,4))
  ## Adding starts doesn't help

  ctable <- coef(summary(ordReg))

  ctable[length(ctable[,1])-3:0,]
}

computeModel_NF <- function(X){
  #ordReg <- ordinal::clm(NeedleFear ~ Survey + Age +
  #                         LessPainSel +
  #                         MorePainSel +
  #                         GenderOneSelections + Vaccine,
  #                       data = X, link = "logit")
  ordReg <- ordinal::clm(NeedleFear ~ Survey + Age + Male +
                           Vaccine + #Q5R1 +
                           Q5R23 +  Survey:Age,
                         data = X, link = "logit")
  #ordReg <- polr( NeedleFear ~ Survey + Age +
  #                  LessPainSel +
  #                  MorePainSel +
  #                  GenderOneSelections + Vaccine,
  #                data = X,
  #                method = "logistic", Hess=TRUE)
  #ctable <- coef(summary(ordReg))[1:6,]
  #pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  #cbind(ctable, 'pval'=round(pvals,4))
  ## Adding starts doesn't help

  ctable <- coef(summary(ordReg))

  ctable[length(ctable[,1])-5:0,]

}

computeModel_D <- function(X){
  #ordReg <- ordinal::clm(Dizziness ~ Survey + Age +
  #                         LessPainSel + MorePainSel + Vaccine,
  #                       data = X, link = "logit")
  ordReg <- ordinal::clm(Dizziness ~ Survey + Age + #Male + #Other +
                           #Vaccine +
                           Q5R1 +
                           Q5R23,# +
                         #Survey:Age,
                         data = X, link = "logit")
  #ordReg <- polr( Dizziness ~ Survey + Age +
  #                  LessPainSel + MorePainSel + Vaccine,
  #                data = X,
  #                method = "logistic", Hess=TRUE)
  #ctable <- coef(summary(ordReg))[1:5,]
  #pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  #cbind(ctable, 'pval'=round(pvals,4))
  ## Adding starts doesn't help

  ctable <- coef(summary(ordReg))

  ctable[length(ctable[,1])-3:0,]
}

#####
##    General Data Prep
#####
baseData <- cbind(data_use,
                  'Q5R1'=as.factor(data$`Q5-1`),
                  'Q5R2'=as.factor(data$`Q5-2`),
                  'Q5R3'=as.factor(data$`Q5-3`))

baseData$Male <- ifelse(baseData$GenderOneSelections=='M',1,0)
baseData$Other <- ifelse(baseData$GenderOneSelections=='O',1,0)
baseData$Q5R23 <- ifelse(as.numeric(baseData$Q5R2)+
                           as.numeric(baseData$Q5R3)-2 >=1,
                         '1','0')

#####
##    Needle Hurt
#####
# Setup
data_model <- na.omit(baseData[c('NeedleHurt','GenderOneSelections',
                                 'Survey','Vaccine',
                                 'Age','Male', 'Other','Q5R1','Q5R23')])
data_model <- data_model[!is.na(data_model$GenderOneSelections),]
data_model <- data_model[data_model$Vaccine!=2,]
data_model <- data_model[data_model$Age<80,]

# Bootstrap
set.seed(123)
boot_NH <- bootstrap(X=data_model,iters=1000, fn=computeModel_NH,
                     strat = T, strat.full = F, stat.amt = 400, silence = T)
#boot_NH$BSResult
# Make information for table
RQ2_Q1_BS <- tibble(c('CARD','Age','Gender (Male)','Survey:Age Interaction'),
                    round(boot_NH$BSResult, 2)[,1],
                    round(boot_NH$BSResult, 2)[,3],
                    round(boot_NH$BSResult, 2)[,4])
colnames(RQ2_Q1_BS) <- c('Variable','Estimate','Lower 95%','Upper 95%')
tab_cap_RQ2_Q1_BS <- paste0("Coefficients and related 95\\% confidence intervals for the medium resample stratified bootstrapped ordinal regression model on needle hurt\\vspace{1em}")


#ggplot(data=data_model,
#       mapping=aes(x = data_model[['NeedleHurt']],
#                   y = data_model[['Age']],
#                   fill = data_model[['Survey']])) +
#  geom_boxplot(outlier.color='gray') +
#  stat_summary(fun=mean, geom="point",
#               shape=18, size=3, color="black",
#               position = position_dodge2(width = 0.75,
#                                          preserve = "single")) +
#  ylab('Age') +
#  xlab("NeedleHurt") +
#  #ggtitle(mainTitle) +
#  theme_bw() +
#  theme(
#    panel.grid.major.x = element_blank(),
#    panel.grid.minor = element_blank(),
#    plot.title = element_text(hjust = 0.5, size = 16),
#    axis.title = element_text(size=14),
#    axis.text = element_text(size=12),
#    legend.title = element_text(size=12),
#    legend.text = element_text(size=10)) +
#  scale_fill_discrete(name=NULL,labels=c("None", "CARD"))

#####
##    Needle Fear
#####
# Setup
data_model <- na.omit(baseData[c('NeedleFear','GenderOneSelections',
                                 'Survey','Vaccine',
                                 'Age','Male', 'Other','Q5R1','Q5R23')])
data_model <- data_model[!is.na(data_model$GenderOneSelections),]
#data_model <- data_model[data_model$GenderOneSelections!='O',]
data_model <- data_model[data_model$Vaccine!=2,]
data_model <- data_model[data_model$Age<80,]

# Bootstrap
set.seed(123)
boot_NF <- bootstrap(X=data_model,iters=1000, fn=computeModel_NF,
                     strat = T, strat.full = F, stat.amt = 400, silence = T)


# Make information for table
RQ2_Q2_BS <- tibble(c('CARD','Age','Gender (Male)','Dose 2',
                      'Side Effect / Media Fear','Survey:Age Interaction'),
                    round(boot_NF$BSResult, 2)[,1],
                    round(boot_NF$BSResult, 2)[,3],
                    round(boot_NF$BSResult, 2)[,4])
colnames(RQ2_Q2_BS) <- c('Variable','Estimate','Lower 95%','Upper 95%')
tab_cap_RQ2_Q2_BS <- paste0("Coefficients and related 95\\% confidence intervals for the medium resample stratified bootstrapped ordinal regression model on needle fear\\vspace{1em}")

#####
##    Dizziness
#####
# Setup
data_model <- na.omit(baseData[c('Dizziness', 'GenderOneSelections',
                                 'Survey','Vaccine',
                                 'Age','Male', 'Other','Q5R1','Q5R23')])

data_model <- data_model[!is.na(data_model$GenderOneSelections),]
data_model <- data_model[data_model$Vaccine!=2,]
data_model <- data_model[data_model$Age<80,]


# Bootstrap
set.seed(123)
boot_D <- bootstrap(X=data_model,iters=1000, fn=computeModel_D,
                    strat = T, strat.full = F, stat.amt = 400, silence = T)

# Make information for table
RQ2_Q3_BS <- tibble(c('CARD','Age','Needle Fear','Side Effect / Media Fear'),
                    round(boot_D$BSResult, 2)[,1],
                    round(boot_D$BSResult, 2)[,3],
                    round(boot_D$BSResult, 2)[,4])
colnames(RQ2_Q3_BS) <- c('Variable','Estimate','Lower 95%','Upper 95%')
tab_cap_RQ2_Q3_BS <- paste0("Coefficients and related 95\\% confidence intervals for the medium resample stratified bootstrapped ordinal regression model on dizziness\\vspace{1em}")
