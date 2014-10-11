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

idata = read.csv(ifile)
temp = idata
#cat('sanity', table(as.character(table(temp[[pairs_col]]))), '\n')  # validate these are all pairs

treatment = temp[which(temp[[treat_col]] == treat_val),]
control = temp[which(temp[[treat_col]] == control_val),]
#cat('treatment', dim(treatment), '\n')
#cat('control', dim(control), '\n')

par(mfrow=c(length(predictors),3))
for(ii in c(1:length(predictors)))
{
    #control_xvals = names(table(control[[predictors[ii]]]))
    #control_yvals = as.numeric(table(control$Reg_GPA))
    #treatment_xvals = names(table(treatment$Reg_GPA))
    #treatment_yvals = as.numeric(table(treatment$Reg_GPA))
    if(is.numeric(control[[predictors[ii]]])) {
        control_xvals = as.numeric(names(table(control[[predictors[ii]]])))
        control_yvals = as.numeric(table(control[[predictors[ii]]]))
        treatment_xvals = as.numeric(names(table(treatment[[predictors[ii]]])))
        treatment_yvals = as.numeric(table(treatment[[predictors[ii]]]))
    }
    else {
        control_xlabels = names(table(control[[predictors[ii]]]))
        control_xvals = c(1:length(control_xlabels)) 
        control_yvals = as.numeric(table(control[[predictors[ii]]]))
        treatment_xlabels = names(table(treatment[[predictors[ii]]]))
        treatment_xvals = c(1:length(treatment_xlabels))
        treatment_yvals = as.numeric(table(treatment[[predictors[ii]]]))
    }
    # dot data
    ymax = max(control_yvals)
    ymin = min(control_yvals)
    plot(x=control_xvals, y=control_yvals, type='o', ylim=c(ymin, ymax+0.1*ymax), lty=1)
    lines(x=treatment_xvals, y=treatment_yvals, type='o', lty=2, pch=17)
    # smoothed density
    control_density = density(as.numeric(control[[predictors[ii]]]))
    treat_density = density(as.numeric(treatment[[predictors[ii]]]))
    ymax = max(c(treat_density$y, control_density$y))
    plot(control_density, ylim=c(0, ymax+0.1*ymax), lty=1)
    lines(treat_density, lty=2)
    # cumulative distribution
    plot(ecdf(control[[predictors[ii]]]), lty=1)
    lines(ecdf(treatment[[predictors[ii]]]), lty=2)
}

#plot(ecdf(rnorm(10000)), lty=1)
#lines(ecdf(rnorm(10000, 0, .5)), lty=2)
#dev.new()
#plot(d1, color='red', add=T)
#scatter.smooth(x=xvals, y=yvals, color='red')
#plot(x=xvals, y=yvals, color='red', add=T)
#points(x=xvals, y=((10*yvals)/sum(yvals)), color='red', add=T)


