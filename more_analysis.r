library(psych)
library(randomForest)
dat = read.csv("/users/o/ecoach/data/W14_data/end_of_semester/STATS250_W14_Full_Data_Set.csv")


#################################################
#    Resource Usage Proportion Statistics       #
#################################################

bad.vars = c(1,14:27,64,65,67,238,255,262,271)
colnames(dat[,bad.vars])
mathy.vars = c("Concentrate__Stats","Concentrate__Math","Concentrate__Physics","Concentrate__Engineering")
sci.non.math.vars = c("Concentrate__Neurosci","Concentrate__Health","Concentrate__Biology_EEB",
                      "Concentrate__Biology_MCDB","Concentrate__Biology","Concentrate__Chemistry")
humanities.vars = c("Concentrate__IDK","Concentrate__Education","Concentrate__Psych_BBCS",
                    "Concentrate__Social_Science_not_Psych", "Concentrate__Humanities")
dat$mathy.concen = rowSums(dat[,mathy.vars])
dat$non.mathy.sci.concen = rowSums(dat[,sci.non.math.vars])
dat$humanities.concen = rowSums(dat[,humanities.vars])

message.names =              c("msg.1.I", "msg.2.I", "msg.3.I",
                               "msg.4.I", "msg.5.I", "msg.6.I",
							   "msg.7.I", "msg.8.I", "msg.9.I",
							   "msg.10.I", "msg.11.I", "msg.12.I",
							   "msg.13.I", "msg.14.I", "msg.15.I",
							   "msg.16.I", "msg.17.I", "msg.18.I",
							   "msg.19.I", "msg.20.I", "msg.21.I",
							   "msg.22.I", "msg.23.I", "msg.24.I",
							   "msg.25.I", "msg.26.I")
#dat$userness = -26 #since there are 26 messages the lowest number is the sum of 26, -1's
dat$ecoach.userness = rowSums(dat[,message.names])

good.vars = c("Final.Course.Grade", "Reg_GPA","Reg_Acad_Level","Subject_Interest","Confidence",
          "confidence_grade","Goal_Grade","Declared","Semesters_Completed","BirthYr",
          "Involved_In__Greek","Involved_In__Volunteering","Parent_Ed", "mathy.concen",
		  "non.mathy.sci.concen", "humanities.concen","Lab.Time",
		  "SNS_INT","VIS_VRB","SEQ_GLO")
		  
tech.vars = c("Tailoring_Level","ecoach.userness","num.urls.visited","num.home.visits",
		      "log.num.clicks.nts","log.num.clicks.non.nts.pr")
#random forest model 		  
mod.1.rf = randomForest(Final.Course.Grade~., data = dat[,good.vars], 
                        na.action=na.omit, importance = TRUE,ntree = 1000)
varImpPlot(mod.1.rf)

dat$predicted = mod.1.rf$predicted

low.index   = which(dat$Tailoring_Level == 0)
high.index  = which(dat$Tailoring_Level == 1)
nt.index    = which(dat$Tailoring_Level == -99)
nt.expect   =  dat$predicted[nt.index]
low.expect  =  dat$predicted[low.index]
high.expect =  dat$predicted[high.index]

summary(nt.expect)
summary(low.expect)
summary(high.expect)
t.test(nt.expect, low.expect)
t.test(nt.expect, high.expect)
t.test(low.expect, high.expect)

hist(nt.expect, 
     freq = FALSE, col = "green", ylim = c(0,0.12),
	 main = "Density Histograms of expected grades",
	 xlab = "Expected Grade", 30)
hist(high.expect, add = TRUE, col = "#FF00007F", freq = FALSE,30 )
hist(low.expect, add = TRUE, col = "#0000FF7F", freq = FALSE,30)
legend("topleft", c("high", "low", "non-user"), text.col = c("#FF00007F","#0000FF7F","green"))

#simulation used to find matched pairs to look at grade difference
#as a function of ecoach-userness
#################################
#    Non-User vs High-Tailor    #
#################################
n.sim = 500
N     = 100
differences = c()
differences.pred = c()
control.index = c()
userness = c()
for(k in 1:n.sim){

    h.sample = sample(high.index, N, replace = TRUE)
	#find min matches
	for(m in h.sample){
	
	    index = which(abs(dat$predicted - dat$predicted[m]) == 
		              min(abs(dat$predicted[nt.index] - dat$predicted[m])))
		index = intersect(index, nt.index)	
        if(length(index) == 0){
		    print("no match found")
		    next
		}		
		differences.pred = c(differences.pred, dat$predicted[m] - dat$predicted[index])
		differences = c(differences, dat$Final.Course.Grade[m] - dat$Final.Course.Grade[index])
		control.index = c(control.index, index)
		userness      = c(userness, rep(dat$ecoach.userness[m], length(index)))
	}
}
hist(differences.pred, main = "Histogram of Predicted Differences")
windows()
hist(differences)
windows()
hist(control.index)
newdat = data.frame(userness = c(-25:25))
bands = predict.lm(mod, newdat, interval="confidence") 
windows()
mod = lm(differences ~ userness)
plot(userness, differences, ylim = c(-3,3), 
     main = "regression plot of differences vs. userness with 95% bands",
	 ylab = "differences (High_Taylor - Non_User) matched by Expected Grade")
lines(newdat$userness,bands[,3], col = "blue")
lines(newdat$userness,bands[,2], col = "blue")
abline(h = 0)
abline(mod)
#################################
#     Non-User vs Low-Tailor    #
#################################
n.sim = 500
N     = 100
differences = c()
differences.pred = c()
control.index = c()
userness = c()
for(k in 1:n.sim){

    h.sample = sample(low.index, N, replace = TRUE)
	#find min matches
	for(m in h.sample){
	
	    index = which(abs(dat$predicted - dat$predicted[m]) == 
		              min(abs(dat$predicted[nt.index] - dat$predicted[m])))
		index = intersect(index, nt.index)	
        if(length(index) == 0){
		    print("no match found")
		    next
		}		
		differences.pred = c(differences.pred, dat$predicted[m] - dat$predicted[index])
		differences = c(differences, dat$Final.Course.Grade[m] - dat$Final.Course.Grade[index])
		control.index = c(control.index, index)
		userness      = c(userness, rep(dat$ecoach.userness[m], length(index)))
	}
}
hist(differences.pred[differences.pred>-0.5], 40, 
     main = "Histogram of Predicted Differences (Low-Tailor - Non-User) matched by Expected Grade")
windows()
hist(differences)
windows()
hist(control.index)
newdat = data.frame(userness = c(-25:25))
bands = predict.lm(mod, newdat, interval="confidence") 
windows()
mod = lm(differences ~ userness)
plot(userness, differences, ylim = c(-3,3), 
     main = "regression plot of differences vs. userness with 95% bands",
	 ylab = "differences (Low_Taylor - Non_User) matched by Expected Grade")
lines(newdat$userness,bands[,3], col = "blue")
lines(newdat$userness,bands[,2], col = "blue")
abline(h = 0)
abline(mod)

#################################
#   Low-Tailor vs High-Tailor   #
#################################
#random forest model 		  
#mod.userness.rf = randomForest(Final.Course.Grade~., data = dat[,c(good.vars, "ecoach.userness")], 
#                        na.action=na.omit, importance = TRUE,ntree = 1000)
#varImpPlot(mod.userness.rf)

n.sim = 1000
N     = 300
differences = c()
differences.userness = c()
#control.index = c()
std.userness = c()
dat$std.userness = -99
dat$std.userness[dat$Tailoring_Level == 0] = (dat$ecoach.userness[dat$Tailoring_Level == 0] - 
                                          mean(dat$ecoach.userness[dat$Tailoring_Level == 0]))/
										  sd(dat$ecoach.userness[dat$Tailoring_Level == 0])
dat$std.userness[dat$Tailoring_Level == 1] = (dat$ecoach.userness[dat$Tailoring_Level == 1] - 
                                          mean(dat$ecoach.userness[dat$Tailoring_Level == 1]))/
										  sd(dat$ecoach.userness[dat$Tailoring_Level == 1])
t.stats = c()
for(k in 1:n.sim){

    l.sample = sample(low.index, N, replace = TRUE)
	h.sample = sample(high.index, N, replace = TRUE)
    low.frame  = dat[l.sample, c("Final.Course.Grade", "std.userness")]
	low.frame  = low.frame[with(low.frame, order(std.userness)),]
    high.frame = dat[h.sample, c("Final.Course.Grade", "std.userness")]
	high.frame  = high.frame[with(high.frame, order(std.userness)),]

	differences = c(differences, high.frame$Final.Course.Grade - low.frame$Final.Course.Grade) 
	differences.userness = c(differences.userness,
                             high.frame$std.userness - low.frame$std.userness) 
	std.userness = c(std.userness,high.frame$std.userness)
	t.stats = c(t.stats, t.test(differences)$statistic)

}

hist(t.stats)

mod = lm(differences ~ std.userness)
newdat = data.frame(std.userness = c(-5:5))
bands = predict.lm(mod, newdat, interval="confidence") 
plot(std.userness, differences, ylim = c(-1.5,1.5), 
     main = "regression plot of differences vs. userness with 95% bands",
	 ylab = "differences in Final Course Grade (High - Low) matched by std.userness")
lines(newdat$std.userness,bands[,3], col = "blue")
lines(newdat$std.userness,bands[,2], col = "blue")
abline(h = 0)
abline(mod)

hist(differences.userness, 40)
