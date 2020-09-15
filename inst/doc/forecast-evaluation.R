## -----------------------------------------------------------------------------
# Use the residuals function
R <- residuals(D$Yhat1, D$y)
# And the score as a function of the horizon
score(R, scoreperiod=ok)$scoreval

## -----------------------------------------------------------------------------
RMSE <- sapply(nms, function(nm){
    score(residuals(D[[nm]],D$y), ok)$scoreval
})

