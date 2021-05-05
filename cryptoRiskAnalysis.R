
# import our libraries
library(quantmod)
library(zoo)
library(tidyr)
library(xts)

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

# plot BTC
plot(underlyingsDF$`BTC-USD`)

# create log returns of our underlyings
underlyingsLogReturns <- diff(log(underlyingsDF))

# plot log returns
plot.zoo(underlyingsLogReturns)
