# Part B:

gdpdata <- read.csv("RealGDP.csv", header=TRUE)
gdpts <- ts(gdpdata, start = c(1970,1))
gdpts
NZL <- gdpts[,"NZL"]
CMR <- gdpts[,"CMR"]
POL <- gdpts[,"POL"]
SDN <- gdpts[,"SDN"]

# NZL <- window(gdpts[,"NZL"], start=1993, end=2007)
# CMR <- window(gdpts[,"CMR"], start=1993, end=2007)
# POL <- window(gdpts[,"POL"], start=1993, end=2007)
# SDN <- window(gdpts[,"SDN"], start=1993, end=2007)

# Question 2: plot using log scale

lNZL <- log(NZL)
lCMR <- log(CMR)
lPOL <- log(POL)
lSDN <- log(SDN)
plot(lNZL, lwd=2, main="Evolution of Real Per Capita GDP From 1970-2017 in log scale", ylim = range(pretty(c(6,14))),ylab="log(Real Per Capita GDP in 2011 dollars)", xlab="Time")
lines(lCMR, col=2, lty=2, lwd=2)
lines(lPOL, col=3, lty=3, lwd=2)
lines(lSDN, col=4, lty=4, lwd=2)
legend("topleft", c("NZL", "CMR", "POL", "SDN"), col=1:4, lty=1:4, lwd=2, bty='n')






# Question 3: detrend quadratic + comovement

# NZL
tnzl <- time(lNZL, offset=0.5)
t2nzl <- tnzl^2
fitnzl <- lm(lNZL~tnzl+t2nzl)
coefTnzl <- coef(fitnzl)
trendnzl <- coefTnzl[1] + coefTnzl[2]*tnzl + coefTnzl[3]*t2nzl
plot(lNZL-trendnzl, lwd=2, main="Cyclical Component Real Per Capita GDP for NZL From 1970-2017 in log scale",ylab="log(Real Per Capita GDP in 2011 dollars)", xlab="Time")


# CMR
tcmr <- time(lCMR, offset=0.5)
t2cmr <- tcmr^2
fitcmr <- lm(lCMR~tcmr+t2cmr)
coefTcmr <- coef(fitcmr)
trendcmr <- coefTcmr[1] + coefTcmr[2]*tcmr + coefTcmr[3]*t2cmr
plot(lCMR-trendcmr, lwd=2, main="Cyclical Component Real Per Capita GDP for CMR From 1970-2017 in log scale",ylab="log(Real Per Capita GDP in 2011 dollars)", xlab="Time")


# POL
tpol <- time(lPOL, offset=0.5)
t2pol <- tpol^2
fitpol <- lm(lPOL~tpol+t2pol)
coefTpol <- coef(fitpol)
trendpol <- coefTpol[1] + coefTpol[2]*tpol + coefTpol[3]*t2pol
plot(lPOL-trendpol, lwd=2, main="Cyclical Component Real Per Capita GDP for POL From 1970-2017 in log scale",ylab="log(Real Per Capita GDP in 2011 dollars)", xlab="Time")

# SDN
tsdn <- time(lSDN, offset=0.5)
t2sdn <- tsdn^2
fitsdn <- lm(lSDN~tsdn+t2sdn)
coefTsdn <- coef(fitsdn)
trendsdn <- coefTsdn[1] + coefTsdn[2]*tsdn + coefTsdn[3]*t2sdn
plot(lSDN-trendsdn, lwd=2, main="Cyclical Component Real Per Capita GDP for SDN From 1970-2017 in log scale",ylab="log(Real Per Capita GDP in 2011 dollars)", xlab="Time")

# all together
plot(lNZL-trendnzl, lwd=2, main="Cyclical Component Real Per Capita GDP From 1970-2017 in log scale",ylab="log(Real Per Capita GDP in 2011 dollars)", xlab="Time", ylim = range(pretty(c(-1,1))))
lines(lCMR-trendcmr, col=2, lty=2, lwd=2)
lines(lPOL-trendpol, col=3, lty=3, lwd=2)
lines(lSDN-trendsdn, col=4, lty=4, lwd=2)
legend("topleft", c("NZL", "CMR", "POL", "SDN"), col=1:4, lty=1:4, lwd=2, bty='n')







# Question 4:
nzlAgr <- diff(NZL)/lag(NZL, -1)*100
nzlavgAgr <- mean(nzlAgr)
nzl1970 <- lNZL[1]
cmrAgr <- diff(CMR)/lag(CMR, -1)*100
cmravgAgr <- mean(cmrAgr)
cmr1970 <- lCMR[1]
polAgr <- diff(POL)/lag(POL, -1)*100
polavgAgr <- mean(polAgr)
pol1970 <- lPOL[1]
sdnAgr <- diff(SDN)/lag(SDN, -1)*100
sdnavgAgr <- mean(sdnAgr)
sdn1970 <- lSDN[1]
averages <- c(nzlavgAgr,cmravgAgr,polavgAgr,sdnavgAgr)
values <- c(nzl1970,cmr1970,pol1970,sdn1970)
plot(values, averages, xlab="log(1970 Real Per Capita GDP)", ylab="Average Annual Growth Rate of GDP from 1970 to 2017 (%)",main="Average Annual Growth Rate vs. 1970 Real Per capita GDP in logs ", pch = 18, col = "blue")
text(values, averages, c("NZL","CMR", "POL", "SDN"), cex=0.6, pos=4, col="red")

# question 5: 152 countries for 1973 and 2007, thousands of dollars, 2 histograms,
dist1 <- window(gdpts[,-1],start=1973, end = 1973)
dist1 <- dist1/1000

hist(dist1, breaks=25,
     xlab="Real per capita GDP (thousands of international dollars of 2011)",
     main="Distribution of real per capita GDP across countries in 1973",
     col="gray", labels=TRUE, ylim = (range(pretty(c(0,45)))))

dist2 <- window(gdpts[,-1],start=2007, end = 2007)
dist2 <- dist2/1000
hist(dist2, breaks=25,
     xlab="Real per capita GDP (thousands of international dollars of 2011)",
     main="Distribution of real per capita GDP across countries in 2007",
     col="gray", labels=TRUE, ylim = (range(pretty(c(0,60)))))

# question 6: 152 countries for 1993 and 2007, log scale, 2 histograms
dist1 <- log(dist1*1000)
hist(dist1, breaks=25,
     xlab="log(Real per capita GDP (international dollars of 2011))",
     main="Distribution of real per capita GDP across countries in 1973 in log",
     col="gray", labels=TRUE, ylim = (range(pretty(c(0,13)))))

dist2 <- log(dist2*1000)
hist(dist2, breaks=25,
     xlab="log(Real per capita GDP (international dollars of 2011))",
     main="Distribution of real per capita GDP across countries in 2007 in log",
     col="gray", labels=TRUE, ylim = (range(pretty(c(0,15)))))

