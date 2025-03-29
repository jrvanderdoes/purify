titanic <- na.omit(titanic::titanic_train[, c("Survived", "Pclass", "Sex", "Age", "Parch")])
titanic <- titanic[titanic$Pclass %in% c(1, 3), ]
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

usethis::use_data(titanic, overwrite = TRUE)
