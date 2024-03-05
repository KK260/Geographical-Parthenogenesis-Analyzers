### This script was used in Karbstein et al. (2021, https://onlinelibrary.wiley.com/doi/10.1111/mec.15919) to convert the in MS Excel imported *vcf file into a file assessable for genetic heterozygosity calculations

## "We transformed information about ref- erence and alternative alleles of the *.vcf file (diploid SNP calls, four allowed alleles per locus). Homozygous sites were assigned a value of 0, and heterozygous sites a value of 1. Then, we calculated the ratio of heterozygous sites to all sites per individual (Table S3)." 


if (!requireNamespace("openxlsx", quietly = TRUE))
  install.packages("openxlsx", repo="http://cran.rstudio.com/")
library(openxlsx)

dat<-read.xlsx("all_het_sites.xlsx")
str(dat)

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


# check column classes
sapply(dat, class)

dat_only_values<-dat[, 10:313]

dat_stat<-summary(dat_only_values)

write.csv(dat_stat, "dat_stat.csv")
write.csv(dat_only_values, "dat_only_values.csv")



if (!requireNamespace("dplyr", quietly = TRUE))
  install.packages("dplyr", repo="http://cran.rstudio.com/")
library(dplyr)

if (!requireNamespace("magrittr", quietly = TRUE))
  install.packages("magrittr", repo="http://cran.rstudio.com/")
library(magrittr)


# solution
dat_only_values %<>% mutate_if(is.factor, as.numeric)

sapply(dat_only_values, class)
str(dat_only_values)

col_sums<-colSums(dat_only_values, na.rm = TRUE)

write.csv(dat_only_values, "dat_only_values.csv")


