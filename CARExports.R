## ----setup, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----warning=FALSE, include=FALSE------------------------------------------------------
library(astsa)
library(tseries)
library(forecast)
library(knitr)

load("finalproject.Rdata")
mydata<-ts(finalPro_data$Exports, start = 1960, end = 2017)


## --------------------------------------------------------------------------------------
#par(mfrow=c(1,2))
#ts.plot(ts(finalPro_data$GDP/1000000, start = 1960, end = 2017),main = "Central African Republic GDP (USD millions)", xlab = "Year", ylab = "GDP in USD millions")
#ts.plot(ts(finalPro_data$GDP/finalPro_data$Population, start = 1960, end = 2017), main = "Central African Republic GDP per capita", xlab = "Year", ylab = "GDP per capita")


## ----exports, fig.height=3, fig.cap="Central African Republic Exports (% of the GDP)"----
ts.plot(mydata, xlab = "Time", ylab = "Exports (% of GDP)")


## ----include=FALSE---------------------------------------------------------------------
adftestres = adf.test(mydata)
pvaladftest = adftestres$p.value
acf(mydata)
pacf(mydata)


## ----include=FALSE---------------------------------------------------------------------
choice1<-diff(mydata)
par(mfrow = c(2, 2)) 
ts.plot(choice1)
acf(choice1)
pacf(choice1)
adfdiff<-adf.test(choice1)


## ----include=FALSE---------------------------------------------------------------------
par(mfrow = c(2, 2)) 
choice2=log(mydata)
ts.plot(choice2)
acf(choice2)
pacf(choice2)
adfchoice2<-adf.test(choice2)


## ----include=FALSE---------------------------------------------------------------------
par(mfrow = c(2, 2)) 
choice3=diff(choice2)
ts.plot(choice3)
acf(choice3)
pacf(choice3)
adfchoice3<-adf.test(choice3)



## ----fig.cap="Time series plot of transformed variables", fig.height=3-----------------
par(mfrow = c(1, 3)) 
ts.plot(choice1, gpars=list(ylab="Difference"))
ts.plot(choice2, gpars=list(ylab="Log"))
ts.plot(choice3, gpars=list(ylab="Log Difference"))


## --------------------------------------------------------------------------------------
kable(rbind(c("Difference", "Log", "Log difference"),c(round(adfdiff$p.value,3), round(adfchoice2$p.value,3), round(adfchoice3$p.value,3))),caption = "p-values for ADF Test applied to transformations")


## ----include=FALSE---------------------------------------------------------------------
ar1_d = sarima(choice1, 0,0,1)


## ----include=FALSE---------------------------------------------------------------------
ar2_d = sarima(choice1,0,0,2)


## ----include=FALSE---------------------------------------------------------------------
ar1_ld = sarima(choice3,0,0,1 )


## ----include=FALSE---------------------------------------------------------------------
ar2_ld = sarima(choice3,0,0,2 )


## ----include=FALSE---------------------------------------------------------------------
ari1_d = sarima(choice1, 0, 1, 1, no.constant=TRUE)



## ----include=FALSE---------------------------------------------------------------------
arima111_d = sarima(choice1, 1, 1, 1, no.constant=TRUE)


## ----include=FALSE---------------------------------------------------------------------
arma12_d = sarima(choice1, 1, 0, 2, no.constant=TRUE)

## ----include=FALSE---------------------------------------------------------------------
arima112_d = sarima(choice1, 1, 1, 2, no.constant=TRUE)


## ----include=FALSE---------------------------------------------------------------------
arma22_d = suppressMessages(sarima(choice1, 2, 0, 2, no.constant=TRUE, details = TRUE, Model = FALSE))



## ----results='hide',fig.keep='all'-----------------------------------------------------
arma22_ld = suppressMessages(sarima(choice3, 2, 0, 2, no.constant=TRUE, details = TRUE))
  str(arma22_ld)


## --------------------------------------------------------------------------------------
models <- data.frame(
  Model = c("ARIMA(1,0,0) with differenced", 
            "ARIMA(2,0,0) with differenced", 
            "ARIMA(1,0,0) with log difference", 
            "ARIMA(2,0,0) with log difference", 
            "ARIMA(1,1,0) with differenced", 
            "ARIMA(1,1,1) with differenced", 
            "ARIMA(1,0,2) with differenced", 
            "ARIMA(1,1,2) with differenced", 
            "ARIMA(2,0,2) with differenced", 
            "ARIMA(2,0,2) with log difference"),
  Errors_wt_independent = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)


kable(cbind(models,round(rbind(ar1_d$ICs, ar2_d$ICs, ar1_ld$ICs, ar2_ld$ICs, ari1_d$ICs, arima111_d$ICs, arma12_d$ICs, arima112_d$ICs, arma22_d$ICs, arma22_ld$ICs),3)),caption = "Comparison of wt p-vals AIC and BIC for model candidates")


## --------------------------------------------------------------------------------------
kable(arma22_ld$ttable, caption = "Coefficients and significance of ARIMA(2,0,2) model for CAR exports")


## --------------------------------------------------------------------------------------
finalmodel=arima(choice3,order=c(2,0,2))

#exp(finalmodel$coef)


## ----fig.cap="Forecast", fig.height=2.5------------------------------------------------
forecstx = forecast(finalmodel,h=8)
autoplot(forecast(finalmodel))



## --------------------------------------------------------------------------------------
#str(forecstx)
unlist(exp(forecstx$mean))


## ----fig.cap="Central African Republic GDP (USD millions) **Not required but related to exports", fig.height=3----
par(mfrow=c(1,1))
ts.plot(ts(finalPro_data$GDP/1000000, start = 1960, end = 2017),main = "Central African Republic GDP (USD millions)", xlab = "Year", ylab = "GDP in USD millions")



## ----results='hide',fig.keep='all', fig.cap="ARIMA(0,0,2) model for log difference"----
ar2_ld = sarima(choice3,0,0,2 )


## ----results='hide',fig.keep='all', fig.cap="ARIMA((2,0,2) for diff"-------------------
arma22_d = suppressMessages(sarima(choice1, 2, 0, 2, no.constant=TRUE, details = TRUE, Model = FALSE))



## --------------------------------------------------------------------------------------
kable(as.data.frame(unlist(adf.test(mydata))), caption = "Augmented Dickey-Fuller Test for original data")


## ----ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE------------------------------
## NA

