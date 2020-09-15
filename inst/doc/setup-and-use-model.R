## -----------------------------------------------------------------------------
# Load the package
library(onlineforecast)
# Set the data in D to simplify notation
D <- Dbuilding


## -----------------------------------------------------------------------------
# Print the first time point
D$t[1]
# Set the score period 
D$scoreperiod <- in_range("2010-12-22", D$t)
# Plot to see it
plot(D$t, D$scoreperiod, xlab="Time", ylab="Scoreperiod")


## -----------------------------------------------------------------------------
# Exclude other points example
scoreperiod2 <- D$scoreperiod
scoreperiod2[in_range("2010-12-30",D$t,"2011-01-02")] <- FALSE


## -----------------------------------------------------------------------------
# Generate new object (R6 class)
model <- forecastmodel$new()
# Set the model output
model$output = "heatload"
# Inputs (transformation step)
model$add_inputs(Ta = "Ta",
                 mu = "one()")
# Regression step parameters
model$add_regprm("rls_prm(lambda=0.9)")
# Optimization bounds for parameters
model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))
# Set the horizons for which the model will be fitted
model$kseq <- c(3,18)


## -----------------------------------------------------------------------------
# Generate new object
model <- forecastmodel$new()
# Set the model output
model$output = "heatload"


## -----------------------------------------------------------------------------
# Inputs (transformation step)
model$add_inputs(Ta = "Ta",
                 mu = "one()")


## -----------------------------------------------------------------------------
# Regression step parameters
model$add_regprm("rls_prm(lambda=0.9)")


## -----------------------------------------------------------------------------
# The evaluation happens with
eval(parse(text="rls_prm(lambda=0.9)"))
# and the result is stored in
model$regprm 


## -----------------------------------------------------------------------------
# Optimization bounds for parameters
model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))


## -----------------------------------------------------------------------------
# Set the horizons for which the model will be fitted
model$kseq <- c(3,18)


## ---- output.lines=15---------------------------------------------------------
# Call the optim() wrapper
model$prm <- rls_optim(model, D)$par


## -----------------------------------------------------------------------------
# Optimized lambda
model$prm


## -----------------------------------------------------------------------------
# Set to fit for all horizons
model$kseq <- 1:36
# Fit for all on entire period in D
fit1 <- rls_fit(model$prm, model, D)


## -----------------------------------------------------------------------------
# See the summary of the fit
summary(fit1)


## -----------------------------------------------------------------------------
# Put the forecasts in D
D$Yhat1 <- fit1$Yhat
# Plot them for selected horizons
plot_ts(D, c("^heatload$|^Y"), kseq = c(1,6,18,36))


## ---- fig.height=4------------------------------------------------------------
# Select a point
i <- 996-48
# and kseq steps ahead
iseq <- i+model$kseq
# The observations ahead in time
plot(D$t[iseq], D$heatload[iseq], type = "b", xlab = "t", ylab = "y")
title(main=pst("Forecast available at ",D$t[i]))
# The forecasts
lines(D$t[iseq], D$Yhat1[i, ], type = "b", col = 2)
legend("topright", c("Observations",pst("Predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = 1:2)


## ---- eval=FALSE--------------------------------------------------------------
## # This will give error
## one()


## -----------------------------------------------------------------------------
# Evaluate input expressions
datatr <- model$transform_data(D)
# See what came out
summary(datatr)
# In particular for the mu = "one()"
head(datatr$mu)


## ---- eval=FALSE--------------------------------------------------------------
## # Set to debug
## #debug(one)
## # Run the input transformation now and it will stop in one()
## datatr <- model$transform_data(D)
## # Set to undebug
## #undebug(one)


## -----------------------------------------------------------------------------
# Just update the Ta input by
model$add_inputs(Ta = "lp(Ta, a1=0.9)")


## -----------------------------------------------------------------------------
# Define a new model with low-pass filtering of the Ta input
model <- forecastmodel$new()
model$output = "heatload"
model$add_inputs(Ta = "lp(Ta, a1=0.9)",
                 mu = "one()")
model$add_regprm("rls_prm(lambda=0.9)")
model$add_prmbounds(Ta__a1 = c(0.5, 0.9, 0.9999),
                    lambda = c(0.9, 0.99, 0.9999))
model$kseq <- c(3,18)


## -----------------------------------------------------------------------------
model$prmbounds


## -----------------------------------------------------------------------------
# Low-pass filter Ta (with a1=0.9 as defined above)
datatr <- model$transform_data(D)
# Actually, lp() can be called directly (although two warnings are thrown)
Talp <- lp(D$Ta, a1=0.99)


## -----------------------------------------------------------------------------
# Plot the Ta$k1 forecasts
plot(D$t, D$Ta$k1, type="l")
# Add the filtered with a1=0.9
lines(D$t, datatr$Ta[ ,"k1"], col=2)
# Add the filtered with a1=0.99
lines(D$t, Talp[ ,"k1"], col=3)


## ---- output.lines=15---------------------------------------------------------
# Optimize the parameters
model$prm <- rls_optim(model, D)$par


## ---- fig.height=4------------------------------------------------------------
# Fit for all horizons
model$kseq <- 1:36
# Fit with RLS
fit2 <- rls_fit(model$prm, model, D)
# Take the forecasts
D$Yhat2 <- fit2$Yhat
# Plot all
plot_ts(D, c("^heatload$|^Y"), kseq = c(1,18))


## -----------------------------------------------------------------------------
summary(fit2)


## -----------------------------------------------------------------------------
# Calculate the score
RMSE1 <- summary(fit1, printit=FALSE)$scoreval
RMSE2 <- summary(fit2, printit=FALSE)$scoreval


## -----------------------------------------------------------------------------
# Check that all NAs in the scoreperiod are at the same positions
all(is.na(fit1$Yhat[fit1$data$scoreperiod, ]) == is.na(fit2$Yhat[fit2$data$scoreperiod, ]))


## -----------------------------------------------------------------------------
# Plot the score for the two models
plot(RMSE1, xlab="Horizon k", ylab="RMSE", type="b", ylim=range(RMSE1,RMSE2))
lines(RMSE2, type="b", col=2)
legend("topleft", c("Input: Ta","Input: Low-pass Ta"), lty=1, col=1:2)


## ---- output.lines=28---------------------------------------------------------
make_tday(D$t, kseq=1:3)


## -----------------------------------------------------------------------------
D$tday <- make_tday(D$t, 1:36)


## -----------------------------------------------------------------------------
D$Tao <- make_input(D$Taobs, kseq=1:36)
model$add_inputs(Tao = "lp(Tao, a1=0.99)")


## -----------------------------------------------------------------------------
m1 <- model$clone_deep()

