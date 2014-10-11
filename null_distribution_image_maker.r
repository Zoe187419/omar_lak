setwd("/users/o/matching/omar_lak/")

file.name = "ec_pairs"
dat = read.csv(paste(file.name, ".csv", sep = ""))

head(dat)

strata.names = unique(dat$group)

temp = matrix(NA, nrow = length(strata.names), ncol = 2)

for(i in 1:length(strata.names)){
    
	temp[i, 1] = dat[which(dat$group == strata.names[i] & dat$usage.ratio_xxx == "True"), "Total.Points"]
	temp[i, 2] = dat[which(dat$group == strata.names[i] & dat$usage.ratio_xxx == "False"), "Total.Points"]

}
head(temp)
diff.vec = temp[,1] - temp[,2]
naive.vec = c(temp[,1], temp[,2])
nsamp = 2500
indexes = 1:length(diff.vec)
naive.indexes = 1:length(naive.vec)
statistic = mean(diff.vec)
dist.vals = c()
naive.vals = c()
for(i in 1:nsamp){

    flip.index = sample(indexes, rbinom(1, length(indexes), prob = 1/2))
	diff.vec[flip.index] = (-1)*diff.vec[flip.index]
	dist.vals = c(dist.vals, mean(diff.vec))
	
	select = sample(naive.indexes, nrow(temp))
	naive.vals = c(naive.vals, mean(naive.vec[select]) - mean(naive.vec[-select]))

}




left = range(dist.vals)[1] - abs(statistic)
right = range(dist.vals)[2] + abs(statistic)
dens = density(dist.vals)
naive.dens = density(naive.vals)

#png(paste(file.name, ".png",sep = ""))
plot(dens, xlim = c(left, right))
lines(naive.dens)
abline(v = statistic)
dev.off()
