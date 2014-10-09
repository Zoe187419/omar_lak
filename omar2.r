library(optmatch)
library(RItools)
library(xtable)
library(splines)
library(randomForest)
options(digits = 3)
set.seed(80)
sessionInfo()

#setwd("/users/o/google drive/lak/learning community/")
dat  = read.csv("../data_lak_2015/learning_communities2.csv")

#print(summary(dat))

#transform GPA to numeric scale since GPA is coded as a factor level (for some reason)
#dat$CUM_GPA = as.numeric(as.character(dat$CUM_GPA))

#############################################################
#  Learning Community vs. Non-Learning Community Residents  #
#############################################################
tree.number = 2500
temp = na.omit(dat) #here we drom any observation with missing data (there are only 2 of them)
# removes 2 with non-numeric Hours at entry

low.data = temp[temp$treat == 0,]
high.data = temp[temp$treat == 1,]

colnames(low.data) = colnames(high.data) #make sure we can row bind

temp = rbind(low.data,high.data) # concat the lows back on... effectively sorted to the bottom.

#build the prognostic score only on control data
if(!file.exists('forest1.Rdata')) {
    cont.mod.rf = randomForest(
        CUM_GPA ~., # http://stats.stackexchange.com/questions/10712/what-is-the-meaning-of-the-dot-in-r
        data = temp[which(temp$treat == 0),],
        ntree = tree.number,
        importance = TRUE
    )
    save(cont.mod.rf, file='forest1.Rdata')
} else {
    load('forest1.Rdata')
}

varImpPlot(cont.mod.rf)
vars = setdiff(colnames(temp), c("CUM_GPA"))

if(!file.exists('forest2.Rdata')) {
    ppty.mod.rf = randomForest(
        factor(treat)~.,
        data = temp[,vars],
        ntree = tree.number,
        importance = TRUE,
        norm.votes = TRUE
    )
    save(ppty.mod.rf, file='forest2.Rdata')
} else {
    load('forest2.Rdata')
}

varImpPlot(ppty.mod.rf)

#cat(length(high.data$Sex), '\n')
#cat(length(temp$Sex), '\n')
stop('exit')

cont.exp.grade = cont.mod.rf$predicted
trea.exp.grade = predict(cont.mod.rf, temp[which(temp$treat == 1),vars])
ETT.exp.grades = c(cont.exp.grade, trea.exp.grade)

#ppty_prog matching
temp$ppty.scores = ppty.mod.rf$votes[,2] #ppty.mod$fitted.values
temp$prog.scores = ETT.exp.grades

#pair match based on both propensity and prognostic
fm.1.1 = pairmatch(factor(temp$treat) ~ ppty.scores + prog.scores, data = temp)
summary(fm.1.1)
#pair match based only on propensity
fm.1.2 = pairmatch(factor(temp$treat) ~ ppty.scores , data = temp)
summary(fm.1.2)

#pair match based on co-variates only
fm.1.3 = pairmatch(factor(temp$treat) ~ Sex+Init.Ethnic.Group.Descrshort+Init.Citizenship.Status.Descrs+
                                        stt+Hours.at.entry+Appl.Ctr.Rtng.Cmp1.Val.Descrsh+
										MaxOfAppl.Ctr.Rtng.Cmp2.Val.Descrsh, data = temp)
summary(fm.1.3)
###########Use this to investigate variable balance
balance = xBalance(treat ~ Sex+Init.Ethnic.Group.Descrshort+Init.Citizenship.Status.Descrs+
                           stt+Hours.at.entry+Appl.Ctr.Rtng.Cmp1.Val.Descrsh+
						   MaxOfAppl.Ctr.Rtng.Cmp2.Val.Descrsh,

							  strata = list(unstratified_comparison = NULL,
							              full_matching_1=~fm.1.1,
										  full_matching_2=~fm.1.2,
										  full_matching_3=~fm.1.3),
							  data = temp, report = c("adj.mean.diffs", "p.values",  "chisquare.test"))

balance.scores = xBalance(treat ~ ppty.scores + prog.scores,
							     strata = list(unstratified_comparison = NULL,
							                   full_matching_1=~fm.1.1,
								               full_matching_2=~fm.1.2),
							     data = temp, report = c("adj.mean.diffs", "p.values",  "chisquare.test"))

effect  = xBalance(treat ~ CUM_GPA,
							strata = list(unstratified_comparison = NULL,
							              full_matching_1=~fm.1.1,
										  full_matching_2=~fm.1.2,
										  full_matching_3=~fm.1.3),
							data = temp, report = c("adj.mean.diffs", "p.values","chisquare.test"))
print(balance)
print(balance.scores)
print(effect)

length(which(temp$treat == 1))
length(which(temp$treat == 0))

#Estimating the 95% CI for the three matching approaches
#It looks like using direct matching without propensity and prognostic scores achives the best balance among
#the covariates.  That being said the balance achieved with both the propensity and prognostic matching is
#acceptable (I think).  Also, the confidence interval from using propensity and prognostic score is much smaller
#giving an indication that because of this it might be preferred to use this matching method
est.seq = seq(-1,1,by = 0.01)
mathfmla <- paste("treat ~", paste(paste("I(CUM_GPA -", est.seq,  "*treat)"), collapse = "+"))
mathfmla <- as.formula(mathfmla)

pvals.math <- xBalance(mathfmla, strata = fm.1.1, data = temp, report = c("p.values"))$results[, "p", 1]
names(pvals.math) <- est.seq
est.seq[pvals.math == max(pvals.math)]
range(est.seq[pvals.math >= 0.05])


pvals.math <- xBalance(mathfmla, strata = fm.1.2, data = temp, report = c("p.values"))$results[, "p", 1]
names(pvals.math) <- est.seq
est.seq[pvals.math == max(pvals.math)]
range(est.seq[pvals.math >= 0.05])


pvals.math <- xBalance(mathfmla, strata = fm.1.3, data = temp, report = c("p.values"))$results[, "p", 1]
names(pvals.math) <- est.seq
est.seq[pvals.math == max(pvals.math)]
range(est.seq[pvals.math >= 0.05])
