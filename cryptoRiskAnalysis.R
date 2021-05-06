
# import our libraries
library(quantmod)
library(zoo)
library(tidyr)
library(xts)
library(moments)

# list out the underlyings we want to compare
underlyingsList <- c("SPY", 
                     "IWM", 
                     "IJS", 
                     "BTC-USD", 
                     "ETH-USD",
                     "GLD",
                     "SLV",
                     "DX-Y.NYB",
                     "TLT")

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
                                 "Close")]
  underlyingsDF <- rbind(underlyingsDF, tickerSeries)
}

# now let's transpose the data
underlyingsDF <- spread(underlyingsDF, 
                        Symbol,
                        Close)


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
      col = "red")

# kernel density estimate
plot(density(underlyingsLogReturns$`BTC-USD`))

# still having pproblems with these lines calls
lines(underlyingsLogReturns$`BTC-USD`,
      dnorm(underlyingsLogReturns$`BTC-USD`,
            mean = mu, 
            sd = sigma),
      col = "red")

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
