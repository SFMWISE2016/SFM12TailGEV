
```yaml

Name of QuantLet: SFM12TailGEV

Published in: Statistics of Financial Markets

Description: 'Fits a Generalized Extreme Value distribution to the negative log-returns of a portfolio (Bayer, BMW, Siemens) for the time period from 2000-01-01 to 2016-07-11 and produces a P-P plot.'

Keywords:portfolio, pp-plot, log-returns, stock-price, GEV, pareto, extreme-value
 
Author: Group12

Datafiles: close.csv

```

![github](https://github.com/SFMWISE2016/SFM12TailGEV/blob/master/SFMTailGEV.png) 


```r
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("fExtremes")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
dat <- read.table(file = "close.csv", header = TRUE, stringsAsFactors = FALSE, 
    sep = ",")

# Portfolio
p = dat$Bayer.Close.Price + dat$BMW.Close.Price + dat$Siemens.Close.Price

l = length(p)  # length of portfolio
loss = log(p[1:(l - 1)]/p[2:l])  # negative log-returns

# Determine the Block Maxima data
T = length(loss)
n = 20
k = T/n
z = matrix(, , )

for (j in 1:k) {
    d = loss[((j - 1) * n + 1):(j * n)]
    z[j] = max(d)
}
w = sort(z)

# Fit the Generalized Extreme Value Distribution
GEV = gevFit(w, type = "mle")

# shape parameter
gama = attr(GEV, "fit")$par.ests[1]
gama
# location parameter
mu = attr(GEV, "fit")$par.ests[2]
# scale parameter
sigma = attr(GEV, "fit")$par.ests[3]

t = (1:k)/(k + 1)
y = pgev(w, xi = gama, mu = mu, beta = sigma)

# Plot the PP plot
dev.new()
png("SFMTailGEV.png")
plot(y, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), 
    ylab = c(""))
lines(y, y, type = "l", col = "red", lwd = 2)
title("PP plot, Generalized Extreme Value Distribution")


