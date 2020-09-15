## -----------------------------------------------------------------------------
## Load the package
library(onlineforecast)


## -----------------------------------------------------------------------------
## Keep it in D to simplify notation
D <- Dbuilding


## -----------------------------------------------------------------------------
## The class of D
class(D)


## -----------------------------------------------------------------------------
## Print the names to see the variables in the data
names(D)


## -----------------------------------------------------------------------------
summary(D)


## -----------------------------------------------------------------------------
check(D)


## -----------------------------------------------------------------------------
## The time
class(D$t)
head(D$t)
tail(D$t)


## ---- eval=FALSE--------------------------------------------------------------
## ?as.POSIXct
## ?strftime


## -----------------------------------------------------------------------------
## Convert from a time stamp (tz="GMT" per default)
ct("2019-01-01 11:00")
## Convert from unix time
ct(3840928387)


## -----------------------------------------------------------------------------
str(D$heatload)


## -----------------------------------------------------------------------------
## Same length as time
length(D$t)
length(D$heatload)


## -----------------------------------------------------------------------------
plot(D$t, D$heatload, type="l", xlab="Time", ylab="Headload (kW)")


## -----------------------------------------------------------------------------
## The observation
D$heatload[2]
## Represents the average load between
D$t[1]
## and
D$t[2]


## -----------------------------------------------------------------------------
## Global radiation forecasts
head(D$I)


## -----------------------------------------------------------------------------
## First time point
D$t[1]


## -----------------------------------------------------------------------------
## The forecast available ahead in time is in the first row
D$I[1, ]


## -----------------------------------------------------------------------------
i <- 1:ncol(D$I)
plot(D$t[i], D$I[1, ], type="l", xlab="Time", ylab="Global radiation forecast (I in W/m²)")


## -----------------------------------------------------------------------------
## Just pick some points by
i <- 200:296
plot(D$t[i], D$I$k8[i], type="l", col=2, xlab="Time", ylab="Global radiation (W/m²)")
## Add the observations
lines(D$t[i], D$Iobs[i])
legend("topright", c("8-step forecasts","Observations"), bg="white", lty=1, col=2:1)


## -----------------------------------------------------------------------------
plot(D$t[i], lagvec(D$I$k8[i], 8), type="l", col=2, xlab="Time", ylab="Global radiation (W/m²)")
lines(D$t[i], D$Iobs[i])
legend("topright", c("8-step forecasts lagged","Observations"), bg="white", lty=1, col=2:1)


## -----------------------------------------------------------------------------
plot_ts(D, patterns=c("^I"), c("2010-12-15","2010-12-18"), kseq=c(1,8,24,36))


## ---- eval=FALSE--------------------------------------------------------------
## plotly_ts(D, patterns=c("heatload$","^I"), c("2010-12-15","2010-12-18"), kseq=c(1,8,24,36))



## ---- fig.width=2*fhs, fig.height=fhs, out.width=ows2-------------------------
par(mfrow=c(1,2))
plot(D$Ta$k8, D$heatload)
plot(lagvec(D$Ta$k8, 8), D$heatload)


## ---- fig.height=figwidth-----------------------------------------------------
pairs(D, nms=c("heatload","Taobs","Ta","t"), kseq=c(1,8,24))


## ---- fig.width=fhs, fig.height=fhs, out.width=ows----------------------------
## Lag the 8-step forecasts to be aligned with the observations
x <- lagvec(D$I$k8, 8)
## Take a smaller range
x <- x[i]
## Take the observations
y <- D$Iobs[i]
## Fit a linear regression model
fit <- lm(y ~ x)
## Plot the result
plot(x, y, xlab="8-step forecasts (W/m²)", ylab="Obsservations (W/m²)", main="Global radiation")
abline(fit)


## -----------------------------------------------------------------------------
plot(D$t[i], predict.lm(fit, newdata=data.frame(x)), type="l", ylim=c(0,max(y)), xlab="Time", ylab="Global radiation (W/m^2)", col=2)
lines(D$t[i], y)
legend("topright", c("8-step forecasts lagged","Observations"), lty=1, col=2:1)


## -----------------------------------------------------------------------------
## Take the 1 to 4 values of each variable in D
Dsub <- subset(D, 1:4)
summary(Dsub)


## -----------------------------------------------------------------------------
which(in_range("2010-12-20",D$t,"2010-12-21"))


## -----------------------------------------------------------------------------
Dsub <- subset(D, c("2010-12-20","2010-12-21"))
summary(Dsub)
Dsub$t


## -----------------------------------------------------------------------------
Df <- as.data.frame(Dsub)
names(Df)


## -----------------------------------------------------------------------------
library(data.table)
setDT(Df)
class(Df)


## -----------------------------------------------------------------------------
## Set back to data.frame
setDF(Df)
## Convert to a data.list
Dsub2 <- as.data.list(Df)
## Compare it with the original Dsub
summary(Dsub2)
summary(Dsub)

