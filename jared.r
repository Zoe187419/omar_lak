rm(list=ls())
options(width=Sys.getenv("COLUMNS")) 
library(optmatch)
library(RItools)
library(xtable)
library(splines)
library(randomForest)
options(digits = 3)
set.seed(80)
sessionInfo()

# config learning communities
version = 'lc'
ifile = "../data_lak_2015/learning_communities2.csv"
treat_col = 'treat'
treat_val = 1
control_val = 0
dep_var_col = 'CUM_GPA'
# QUESTION: do binary trees work to predict continuous variables?
# if so is it just by making it into lots of categorical bins?
prog_formula = paste0(dep_var_col, ' ~.')
prop_formula = paste0('factor(', treat_col, ')~.')

# config mcdb ecoach
version = 'ec'
ifile = "../data_lak_2015/mcdb.csv"
treat_col = 'usage.ratio_xxx'
treat_val = 'True'
control_val = 'False'
dep_var_col = 'Total.Points'
# QUESTION: do binary trees work to predict continuous variables?
# if so is it just by making it into lots of categorical bins?
# QUESTION: is it ok to use data available only for treatment
# when predicting the propensity
prog_formula = paste0(dep_var_col, ' ~.')
prop_formula = paste0('factor(', treat_col, ')~.')

# config model
hard_reset = FALSE
hard_reset = TRUE
tree.number = 2500

# read and clean data
idat  = read.csv(ifile)
temp = na.omit(idat) 

#low.data = temp[temp[[treat_col]] == control_val,]
#high.data = temp[temp[[treat_col]] == treat_val,]
#colnames(low.data) = colnames(high.data) #make sure we can row bind
#temp = rbind(low.data,high.data) # concat the lows back on... effectively sorted to the bottom.

# build the prognostic score only on control data
if(hard_reset || !file.exists(paste0(version, '_forest1.Rdata'))) {
    cont.mod.rf = randomForest(
        as.formula(prog_formula), # http://stats.stackexchange.com/questions/10712/what-is-the-meaning-of-the-dot-in-r
        data = temp[which(temp[[treat_col]] == control_val),],
        ntree = tree.number,
        importance = TRUE
    )
    save(cont.mod.rf, file=paste0(version, '_forest1.Rdata'))
} else {
    load(paste0(version, '_forest1.Rdata'))
}

#varImpPlot(cont.mod.rf)
vars = setdiff(colnames(temp), c(dep_var_col))

# propensity score model
if(hard_reset || !file.exists(paste0(version, '_forest2.Rdata'))) {
    ppty.mod.rf = randomForest(
        as.formula(prop_formula),
        data = temp[,vars],
        ntree = tree.number,
        importance = TRUE,
        norm.votes = TRUE
    )
    save(ppty.mod.rf, file=paste0(version, '_forest2.Rdata'))
} else {
    load(paste0(version, '_forest2.Rdata'))
}

#varImpPlot(ppty.mod.rf)

# predicted: the predicted values of the input data based on out-of-bag samples
# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
cont.exp.grade = cont.mod.rf$predicted 
trea.exp.grade = predict(cont.mod.rf, temp[which(temp[[treat_col]] == treat_val),vars])
ETT.exp.grades = c(cont.exp.grade, trea.exp.grade)

# add the ppty and prog data for matching
temp$ppty.scores = ppty.mod.rf$votes[,2] #ppty.mod$fitted.values
temp$prog.scores = ETT.exp.grades

# pair match based on both propensity and prognostic
fm.1.1 = pairmatch(factor(temp[[treat_col]]) ~ ppty.scores + prog.scores, data = temp)
#print(summary(fm.1.1))
#cat('--------------------\n\n')

# pair match based only on propensity
fm.1.2 = pairmatch(factor(temp[[treat_col]]) ~ ppty.scores , data = temp)
#print(summary(fm.1.2))
#cat('--------------------\n\n')

#print(temp[which(fm.1.1!='NA'),])
print(table(fm.1.1))
stop('exit')

# pair match based on co-variates only
fm.1.3 = pairmatch(factor(temp[[treat_col]]) ~ Sex+Init.Ethnic.Group.Descrshort+Init.Citizenship.Status.Descrs+
                                        stt+Hours.at.entry+Appl.Ctr.Rtng.Cmp1.Val.Descrsh+
										MaxOfAppl.Ctr.Rtng.Cmp2.Val.Descrsh, data = temp)
#print(summary(fm.1.3))
#cat('--------------------\n\n')

