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

# config problem roulette with ecoach
version = 'pr_ec'
ifile = "../data_lak_2015/pr_lak_ecoach.csv"
treat_col = 'tried_xxx'
treat_val = '1'
control_val = '0'
dep_var_col = 'Exam1_100'
# if so is it just by making it into lots of categorical bins?
predictors = c("Reg_Gender","goal_grade","grade_confidence","interest","prev_chem","math_confidence")
prog_formula = paste0(dep_var_col, ' ~ ', paste(predictors, collapse=' + ' ) )
prop_formula = paste0('factor(', treat_col, ') ~ ', paste(predictors, collapse=' + ' ) )

# config problem roulette with both
version = 'pr_both'
ifile = "../data_lak_2015/pr_lak_both.csv"
treat_col = 'tried_xxx'
treat_val = '1'
control_val = '0'
dep_var_col = 'Exam1_100'
# if so is it just by making it into lots of categorical bins?
predictors = c("Reg_GPA","Reg_Gender","goal_grade","grade_confidence","interest","prev_chem","math_confidence")
prog_formula = paste0(dep_var_col, ' ~ ', paste(predictors, collapse=' + ' ) )
prop_formula = paste0('factor(', treat_col, ') ~ ', paste(predictors, collapse=' + ' ) )

# config problem roulette with reg_gpa
version = 'pr_gpa'
ifile = "../data_lak_2015/pr_lak_gpa.csv"
treat_col = 'tried_xxx'
treat_val = '1'
control_val = '0'
dep_var_col = 'Exam1_100'
# if so is it just by making it into lots of categorical bins?
predictors = c("Reg_GPA","Reg_Gender")
prog_formula = paste0(dep_var_col, ' ~ ', paste(predictors, collapse=' + ' ) )
prop_formula = paste0('factor(', treat_col, ') ~ ', paste(predictors, collapse=' + ' ) )

# config mcdb ecoach
version = 'ec'
ifile = "../data_lak_2015/mcdb2.csv"
treat_col = 'usage.ratio_xxx'
treat_val = '1'
control_val = '0'
dep_var_col = 'Total.Points'
predictors = c("AP_Bio","AP_Chem","Attendance_Anticipated","Confidence","Goal_Grade","Reason__Concentration_req","Reason__Grad_req","Reason__Interest","Reg_Acad_Level","Reg_GPA","Reg_Gender","SLC_Enrolled","Subject_Interest")
prog_formula = paste0(dep_var_col, ' ~ ', paste(predictors, collapse=' + ' ) )
prop_formula = paste0('factor(', treat_col, ') ~ ', paste(predictors, collapse=' + ' ) )

# config learning communities
version = 'lc2'
ifile = "../data_lak_2015/input-new.csv"
treat_col = 'treat'
treat_val = 1
control_val = 0
dep_var_col = 'gpa'
predictors = c("sex","ethnic","citizen","stt","credits","parents","income")
prog_formula = paste0(dep_var_col, ' ~ ', paste(predictors, collapse=' + ' ) )
prop_formula = paste0('factor(', treat_col, ') ~ ', paste(predictors, collapse=' + ' ) )

#################################################################################
# configs above here
##################################################################################

# read and clean data
idat  = read.csv(ifile)
temp = na.omit(idat) 

# config model
tree.number = 2500

#low.data = temp[temp[[treat_col]] == control_val,]
#high.data = temp[temp[[treat_col]] == treat_val,]
#colnames(low.data) = colnames(high.data) #make sure we can row bind
#temp = rbind(low.data,high.data) # concat the lows back on... effectively sorted to the bottom.

# build the prognostic score only on control data
if(!file.exists(paste0(version, '_forest1.Rdata'))) {
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

# propensity score model
if(!file.exists(paste0(version, '_forest2.Rdata'))) {
    ppty.mod.rf = randomForest(
        as.formula(prop_formula),
        #data = temp[,vars],
        data = temp,
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
trea.exp.grade = predict(cont.mod.rf, temp[which(temp[[treat_col]] == treat_val), predictors])
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
#print(table(fm.1.1))
#print(length(table(fm.1.1)))

pairings = sapply(c(names(temp), 'group'),function(x) NULL) # creates empty list with names from temp
for(ii in names(table(fm.1.1)))
{
    pairs = names(fm.1.1[which(ii == fm.1.1)])
    pair1 = temp[pairs[1],]
    pair2 = temp[pairs[2],]
    pair1$group = paste0(ii,'_a')
    pair2$group = paste0(ii, '_a')
    pairings = rbind(pairings, pair1)
    pairings = rbind(pairings, pair2)
}
write.csv(pairings, file = paste0('omar_', version, "_pairs.csv"))

dev.new()
control_progs = pairings[which(pairings[[treat_col]] == control_val),]$prog.scores
treat_progs = pairings[which(pairings[[treat_col]] == treat_val),]$prog.scores
hist(control_progs, col=rgb(0.1, 0.1, 0.1, 0.5))
hist(treat_progs, add=T, col=rgb(0.8, 0.8, 0.8, 0.5))
dev.new()
control_pptys = pairings[which(pairings[[treat_col]] == control_val),]$ppty.scores
treat_pptys = pairings[which(pairings[[treat_col]] == treat_val),]$ppty.scores
hist(control_pptys, col=rgb(0.1, 0.1, 0.1, 0.5))
hist(treat_pptys, add=T, col=rgb(0.8, 0.8, 0.8, 0.5))

stop('exit')


