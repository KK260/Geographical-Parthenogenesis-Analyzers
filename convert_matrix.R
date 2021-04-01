library(openxlsx)

dat<-read.xlsx("all_het_sites.xlsx")
#str(dat)

head(dat)

dat <- data.frame(lapply(dat, function(x){
  gsub("0/0", "0", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("1/1", "0", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("0/1", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("1/2", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("1/3", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("0/2", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("0/3", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("1/2", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("1/3", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("2/3", "1", x)
  }))



dat <- data.frame(lapply(dat, function(x){
  gsub("1/0", "1", x)
  }))


dat <- data.frame(lapply(dat, function(x){
  gsub("2/0", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("3/0", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("2/2", "0", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("3/3", "0", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("2/1", "1", x)
  }))

dat <- data.frame(lapply(dat, function(x){
  gsub("3/2", "1", x)
  }))


dat <- data.frame(lapply(dat, function(x){
  gsub("3/1", "1", x)
  }))

#check column classes

sapply(dat, class)

dat_only_values<-dat[, 10:313]

dat_stat<-summary(dat_only_values)

write.csv(dat_stat, "dat_stat.csv")
write.csv(dat_only_values, "dat_only_values.csv")










#install.packages("dplyr")
library(dplyr)

#install.packages("magrittr")
library(magrittr)

# solution

dat_only_values %<>% mutate_if(is.factor, as.numeric)

#write.csv(dat_only_values, "dat_only_values.csv")


sapply(dat_only_values, class)
str(dat_only_values)

col_sums<-colSums(dat_only_values, na.rm = TRUE)

