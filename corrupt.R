
a_num = 1000
b_num = 100
c_num = 1000
a_mean = 0
b_mean = 20
c_mean = -20
a_sd = 10
b_sd = 10
c_sd = 10

aa = rnorm(a_num, mean = a_mean, sd = a_sd)
bb = rnorm(b_num, mean = b_mean, sd = b_sd)
cc = rnorm(c_num, mean = c_mean, sd = c_sd)

treat1 = aa
control1= cc

sig1 = (mean(treat1) - mean(control1)) / (sd(c(treat1, control1)) / sqrt(length(c(treat1, control1))))
cat('real signal:\t\t', sig1, '\n')

treat2 = aa
control2 = c(bb,cc)

sig2 = (mean(treat2) - mean(control2)) / (sd(c(treat2, control2)) / sqrt(length(c(treat2, control2))))
cat('false signal:\t\t', sig2, '\n')

cat('ratio signal:\t\t', sig2/sig1, '\n')

