
# import our libraries
library(quantmod)
library(zoo)
library(tidyr)
library(xts)
library(moments)
library(MASS)
library(QRM)


# list out the underlyings we want to compare
underlyingsList <- c("SPY", 
                     "BTC-USD", 
                     "ETH-USD",
                     "BNB-USD",
                     "DOGE-USD",
                     "XRP-USD",
                     "GLD",
                     "SLV",
                     "DX-Y.NYB"
                     )

# define our start date for our series
startDate <- Sys.Date() - 365 * 10

# define our end date for our series
endDate <- Sys.Date()

# create an empty item for our dataFrame
underlyingsDF <- NULL

# use a for loop to fill in our empty df
for (tickerIndex in seq(length(underlyingsList))){
  ticker <- underlyingsList[tickerIndex]
  quantmod::getSymbols(ticker, 
                       verbose = TRUE,
                       src = "yahoo",
                       from = startDate,
                       to = endDate)
  tickerSeries <- as.data.frame(get(ticker))
  tickerSeries$Date <- row.names(tickerSeries)
  tickerSeries$Index <- ticker
  row.names(tickerSeries) <- NULL
  colnames(tickerSeries) <- c("Open",
                         "High",
                         "Low",
                         "Close",
                         "Volume",
                         "Adjusted",
                         "Date",
                         "Symbol")
  tickerSeries <- tickerSeries[c("Date", 
                                 "Symbol",
                                 "Adjusted")]
  underlyingsDF <- rbind(underlyingsDF, tickerSeries)
}

# now let's transpose the data
underlyingsDF <- spread(underlyingsDF, 
                        Symbol,
                        Adjusted)


# make Date a date
underlyingsDF$Date <- as.Date(underlyingsDF$Date)

# drop NAs
underlyingsDF <- na.omit(underlyingsDF)

#create underlyingsDF as ZOO
underlyingsDF <- read.zoo(underlyingsDF, 
                          format = "%Y-%m-%d")

# let's print the head and take a looky-see
class(underlyingsDF)
head(underlyingsDF)
tail(underlyingsDF)
summary(underlyingsDF)


# plot them in one graph
plot.zoo(underlyingsDF)

# plot them in one graph
plot.zoo(underlyingsDF,
         plot.type = "single",
         col = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
legend(julian(legend = names(underlyingsDF)))

# create log returns of our underlyings
underlyingsLogReturns <- diff(log(underlyingsDF))

# lets make a df of weekly log returns
underlyingsWeeklyLogReturns <- apply.weekly(underlyingsLogReturns, 
                                             colSums)

# lets make a df of monthly log returns
underlyingsMonthlyLogReturns <- apply.monthly(underlyingsLogReturns, 
                                             colSums)

# lets make a df of quarterly log returns
underlyingsQuarterlyLogReturns <- apply.quarterly(underlyingsLogReturns, 
                                             colSums)

# plot log returns
plot.zoo(underlyingsLogReturns)

# same but with vertical bars
plot.zoo(underlyingsLogReturns, type = "h")

# plot our weekly log returns
plot.zoo(underlyingsWeeklyLogReturns, type = "h")

# plot our monthly log returns
plot.zoo(underlyingsMonthlyLogReturns, type = "h")

# plot our quarterly log returns
plot.zoo(underlyingsQuarterlyLogReturns, type = "h")

pairs(underlyingsLogReturns)
pairs(underlyingsWeeklyLogReturns)
pairs(underlyingsMonthlyLogReturns)
pairs(underlyingsQuarterlyLogReturns)

# find mean daily log return for btc
mu <- mean(underlyingsLogReturns$`BTC-USD`)

# standard deviation of btc daily log returns
sigma <- sd(underlyingsLogReturns$`BTC-USD`)

# plot a histogram of the daily log returns of btv
hist(underlyingsLogReturns$`BTC-USD`, 
     nclass = 20,
     probability = TRUE)

lines(underlyingsLogReturns$`BTC-USD`,
      dnorm(underlyingsLogReturns$`BTC-USD`,
            mean = mu, 
            sd = sigma),
      col = "red", 
      type = "p")

# kernel density estimate
plot(density(underlyingsLogReturns$`BTC-USD`))

# still having pproblems with these lines calls
lines(underlyingsLogReturns$`BTC-USD`,
      dnorm(underlyingsLogReturns$`BTC-USD`,
            mean = mu, 
            sd = sigma),
      col = "red",
      type = "p")

# q-q plot of BTC log returns against a normal distribution
qqnorm(underlyingsMonthlyLogReturns$`BTC-USD`)
qqline(underlyingsLogReturns$`BTC-USD`, 
       col = "red")

# calculate skewness of btc log returns
skewness(underlyingsLogReturns$`BTC-USD`)

# kurtosis of btc log returns
kurtosis(underlyingsLogReturns$`BTC-USD`)

# jarque bera test for normality
jarque.test(as.vector(underlyingsLogReturns$`BTC-USD`))

# we can apply the skewness and kurtosis of 
# the distribution of the log returns to each
# underlying and plot a comparison
skewScores <- apply(underlyingsLogReturns, 
                    2, 
                    skewness)
kurtosisScores <- apply(underlyingsLogReturns, 
                        2 ,
                        kurtosis)

# and now to plot them
plot(skewScores, 
     kurtosisScores, 
     type = "n")
text(skewScores, 
     kurtosisScores, 
     names(skewScores), 
     cex = 0.6,
     col = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

# calculate the p-value for jarque bera test for normality
# for each underlying in daily weekly and monthly log returns
apply(underlyingsLogReturns, 2, function(v){jarque.test(v)$p.value})
apply(underlyingsWeeklyLogReturns, 2, function(v){jarque.test(v)$p.value})
apply(underlyingsMonthlyLogReturns, 2, function(v){jarque.test(v)$p.value})
apply(underlyingsQuarterlyLogReturns, 2, function(v){jarque.test(v)$p.value})

# let's create a student's-T distribution instead
# first we fit it to the data
tfit <- fit.st(underlyingsLogReturns$`BTC-USD`)

# now we can use this fit to determine mean and sd
tpars <- tfit$par.ests
nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# Plot a histogram of our log returns again
hist(underlyingsLogReturns$`BTC-USD`, 
     nclass = 20, 
     probability = TRUE,
     ylim = range(0, 18))

# Compute the fitted t density based 
# on the metrics obtained by our fit
yvals <- dt((underlyingsLogReturns$`BTC-USD` - mu)/sigma, df = nu)/sigma

# Superimpose a red line to show the fitted t density
lines(underlyingsLogReturns$`BTC-USD`, 
      yvals, 
      col = "red",
      type = "p")

# alright now we are going to look at the acf of the underlyings
# first we set the plot so we get two columns and the number of rows
# as there are underlyings we want to look at
par(mfrow = c(ncol(underlyingsLogReturns) / 3, 3))

# now we plot the acfs
for (colName in names(underlyingsLogReturns)){
  acf(
    underlyingsLogReturns[, colName], 
    na.action = na.pass, 
    main = paste(colName, "ACF")
    )
}

# now let's do this for absolute values
par(mfrow = c(ncol(underlyingsLogReturns) / 3, 3))
for (colName in names(underlyingsLogReturns)){
  acf(
    abs(underlyingsLogReturns[, colName]), 
    na.action = na.pass, 
    main = paste(colName, "Absolute Values ACF")
  )
}

# now let's do this for squared values
par(mfrow = c(ncol(underlyingsLogReturns) / 3, 3))
for (colName in names(underlyingsLogReturns)){
  acf(
    underlyingsLogReturns[, colName] ^ 2, 
    na.action = na.pass, 
    main = paste(colName, "Squared Values ACF")
  )
}
