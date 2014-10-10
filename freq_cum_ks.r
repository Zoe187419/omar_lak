rm(list=ls())
options(width=Sys.getenv("COLUMNS")) 
options(digits = 3)
set.seed(80)

idata = read.csv('ec_pairs.csv')
#ifile = read.csv(file='ec_pairs.csv', header=T, sep=",")
#ifile = read.csv(file='pr1_pairs.csv', header=T, sep=",")



