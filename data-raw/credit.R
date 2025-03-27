
library(caret)
data(GermanCredit)
dat <- GermanCredit
dat <- dat[,c("Class",'Age','NumberExistingCredits')]
for(val in c('Personal','CheckingAccountStatus','CreditHistory',
             'Purpose','SavingsAccountBonds','EmploymentDuration',
             'OtherDebtorsGuarantors','Property',
             'OtherInstallmentPlans','Housing','Job')){
  tmp <- gsub(paste0(val,'.'), "",
              apply(GermanCredit[,grep(val,colnames(GermanCredit))] , 1,
                    function(x) names(x)[x==1]))
  dat[[val]] <- as.factor(tmp)

}

credit <- dat
usethis::use_data(credit, overwrite = TRUE)

