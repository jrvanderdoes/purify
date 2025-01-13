## code to prepare `cats` dataset goes here
cats <- read.csv('./data-raw/cats.csv')
cats$Sex <- as.factor(cats$Sex)
usethis::use_data(cats, overwrite = TRUE)

subcats<-cats[-c(1:4,6:13,15:19,22:30,47:48),]
usethis::use_data(subcats, overwrite = TRUE)
