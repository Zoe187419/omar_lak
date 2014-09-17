library(optmatch)
library(RItools)
library(xtable)
library(splines)
library(randomForest)
options(digits = 3)
set.seed(80)
sessionInfo()

#dat = read.csv("/users/o/google drive/LAK/omar_data.csv")
dat = read.csv("../data_lak_2015/learning_communities.csv")
summary(dat)

######################################
#     Dropping Missing Data Rows     #
######################################
N = 1000
temp = na.omit(dat)
tg = temp[temp$in_treatment == TRUE,]
cg = temp[temp$in_treatment == FALSE,]
beta.matrix = matrix(NA, nrow = N, ncol = 24) #22 in the number of vars when using dummy variables in the GLM below 
ppty.matrix = matrix(NA, nrow = nrow(temp), ncol = N)
model.vars = c("sex","ethnic","act","Init.Home.Acad.Group.Descrsh","in_treatment",
               "condition1","condition2","pre_hours","practice")
for(i in 1:N){
    
	new.temp = rbind(tg[,model.vars], cg[,model.vars])
    select = c(1:nrow(tg), sample( (nrow(tg)+1):nrow(new.temp), nrow(tg), replace = FALSE))
    #print(select)    
	model = glm(in_treatment ~ ., data = new.temp[select,], family = "binomial")
	
	#beta.matrix[i,]       = model$coefficients
	ppty.matrix[select,i] = model$fitted.values

}
temp$ppty.score = rowMeans(ppty.matrix,  na.rm = TRUE)
t.test(ppty.score ~ in_treatment, data = temp)
boxplot(ppty.score ~ in_treatment, data = temp)

fm.1.1 = fullmatch(in_treatment ~ ppty.score, data = temp,
                   within = caliper(match_on(in_treatment ~ ppty.score, data = temp), width = 0.15))
								                                                      
summary(fm.1.1)

pair.match = pairmatch(in_treatment ~ ppty.score, data = temp,
                   within = caliper(match_on(in_treatment ~ ppty.score, data = temp), width = 0.15))
								                                                      
summary(pair.match)

variable.balance  = xBalance(in_treatment ~ .,
							strata = list(unstratified_comparison = NULL, 
							              full_matching_1=~fm.1.1,
										  pair_matching_2=~pair.match),
							data = na.omit(temp), report = c("adj.mean.diffs", "p.values","chisquare.test"))						
print(variable.balance)

pre_gpa.balance  = xBalance(in_treatment ~ pre_gpa,
							strata = list(unstratified_comparison = NULL, 
							              full_matching_1=~fm.1.1,
										  pair_matching_2=~pair.match),
							data = na.omit(temp), report = c("adj.mean.diffs", "p.values","chisquare.test"))						
print(pre_gpa.balance)

######################################
# Without Dropping Missing Data Rows #
######################################
#cont.mod.rf = randomForest(temp$Final.Course.Grade[which(temp$TorC == 0)]~.,    #
#	                           data = temp[which(temp$TorC == 0),vars],           
#							   ntree = tree.number, importance = TRUE)                     
#varImpPlot(cont.mod.rf)
#temp$ppty.scores = ppty.mod.rf$votes[,2]
