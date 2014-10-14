rm(list=ls())
options(width=Sys.getenv("COLUMNS")) 
options(digits = 3)
set.seed(80)

# config mcdb ecoach
version = 'ec'
ifile = 'ec_pairs.csv'
treat_col = 'usage.ratio_xxx'
pairs_col = 'group'
treat_val = 'True'
control_val = 'False'
predictors = c('Reg_GPA')
predictors = c('Reg_Gender')
predictors = c('Reg_Gender', 'Reg_GPA', 'Reg_Acad_Level')

# config learning communities
version = 'lc'
ifile = 'lc_pairs.csv'
treat_col = 'treat'
pairs_col = 'group'
treat_val = 1
control_val = 0
predictors = c('Sex', 'ethnic', 'citizen', 'parents', 'income')

# config problem roulette exam 1
version = 'pr1'
ifile = 'pr1_pairs.csv'
treat_col = 'tried_xxx'
pairs_col = 'group'
treat_val = 'True'
control_val = 'False'
predictors = c('Reg_Gender', 'Reg_GPA')

# config learning communities 2
version = 'lc2'
ifile = 'lc2_pairs.csv'
treat_col = 'treat'
pairs_col = 'group'
treat_val = 1
control_val = 0
predictors = c("sex","ethnic","citizen")
predictors = c("stt","credits","parents","income")

# config learning communities 2 with Chris's method
version = 'lc2'
ifile = "../data_lak_2015/input-new.csv"
treat_col = 'treat'
pairs_col = 'group'
treat_val = 1
control_val = 0
predictors = c("sex","ethnic","citizen")
predictors = c("stt","credits","parents","income")

idata = read.csv(ifile)
temp = idata
cat('sanity', table(as.character(table(temp[[pairs_col]]))), '\n')  # validate these are all pairs

treatment = temp[which(temp[[treat_col]] == treat_val),]
control = temp[which(temp[[treat_col]] == control_val),]
#cat('treatment', dim(treatment), '\n')
#cat('control', dim(control), '\n')

par(mfrow=c(length(predictors),3))
for(ii in c(1:length(predictors)))
{
    control_xlabels = names(table(control[[predictors[ii]]]))
    control_xvals = c(1:length(control_xlabels)) 
    control_yvals = as.numeric(table(control[[predictors[ii]]]))
    treatment_xlabels = names(table(treatment[[predictors[ii]]]))
    treatment_xvals = c(1:length(treatment_xlabels))
    treatment_yvals = as.numeric(table(treatment[[predictors[ii]]]))

    # dot data
    ymax = max(c(control_yvals, treatment_yvals))
    ymin = min(c(control_yvals, treatment_yvals))
    plot(x=control_xvals, y=control_yvals, type='o', ylim=c(ymin, ymax+0.1*ymax), lty=1, xlab=predictors[[ii]], ylab='frequency', main='Frequency Line Graph', xaxt='n')
    axis(1, at=control_xvals, labels=control_xlabels, col.axis="black", las=0)
    lines(x=treatment_xvals, y=treatment_yvals, type='o', lty=2, pch=17)
    # smoothed density
    control_density = density(as.numeric(control[[predictors[ii]]]))
    treat_density = density(as.numeric(treatment[[predictors[ii]]]))
    ymax = max(c(treat_density$y, control_density$y))
    plot(control_density, ylim=c(0, ymax+0.1*ymax), lty=1, xlab=predictors[[ii]], ylab='frequency', main='Smoothed Line Graph')
    lines(treat_density, lty=2)
    # cumulative distribution
    ks = ks.test(control_yvals, treatment_yvals)
    plot(ecdf(control[[predictors[ii]]]), lty=1, xlab=predictors[[ii]], ylab='cumulative distribution', main=paste0('KS statistic: ', round(ks$statistic, 3), ' p-value: ', round(ks$p.value, 5)))
    lines(ecdf(treatment[[predictors[ii]]]), lty=2)
}

#plot(ecdf(rnorm(10000)), lty=1)
#lines(ecdf(rnorm(10000, 0, .5)), lty=2)
#dev.new()
#plot(d1, color='red', add=T)
#scatter.smooth(x=xvals, y=yvals, color='red')
#plot(x=xvals, y=yvals, color='red', add=T)
#points(x=xvals, y=((10*yvals)/sum(yvals)), color='red', add=T)


