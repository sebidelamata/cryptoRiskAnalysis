#######################
#######################
##  Import Libraries ##
#######################
#######################


# quantmod api gives us our yahoo data
library(quantmod)
# zoo is great for time series
library(zoo)
# tidyr is that good shit
library(tidyr)
# as is dplyr
library(dplyr)
# more stuff for working with time series
library(xts)
# statistics stuff yall
library(moments)
# more fun statistics stuff
library(MASS)
# risk management stuff
library(QRM)
# plotly for plotting interactive graphs
library(plotly)
# we'll need dash too
library(dash)
# these other packages help us work with dash
library(dashCoreComponents)
library(dashHtmlComponents)


#################################
#################################
## Importing and cleaning Data ##
#################################
#################################


# list out the underlyings we want to compare
underlyingsList <- c(
  "SPY", 
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
  quantmod::getSymbols(
    ticker, 
    verbose = TRUE,
    src = "yahoo",
    from = startDate,
    to = endDate
    )
  tickerSeries <- as.data.frame(get(ticker))
  tickerSeries$Date <- row.names(tickerSeries)
  tickerSeries$Index <- ticker
  row.names(tickerSeries) <- NULL
  colnames(tickerSeries) <- c(
    "Open",
    "High",
    "Low",
    "Close",
    "Volume",
    "Adjusted",
    "Date",
    "Symbol"
    )
  tickerSeries <- tickerSeries[c(
    "Date", 
    "Symbol",
    "Adjusted")]
  underlyingsDF <- rbind(
    underlyingsDF, 
    tickerSeries)
}

# now let's transpose the data
underlyingsDF <- spread(
  underlyingsDF, 
  Symbol,
  Adjusted)


# make Date a date
underlyingsDF$Date <- as.Date(underlyingsDF$Date)

# drop NAs
underlyingsDF <- na.omit(underlyingsDF)

# create log returns of our underlyings
underlyingsLogReturns <- underlyingsDF %>%
  mutate_at(
    .vars = vars(
      -c(Date)
    ),
    .funs = list(~ c(
      0,
      diff(log(.))
           )
      )
    )




###############################
###############################
## Exploratory Data Analysis ##
###############################
###############################





# time series plot of prices
adjReturnsTimeSeries <- plot_ly(
  data = underlyingsDF,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "scatter",
  mode = 'lines+markers'
  )
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  add_trace(
    y = ~`BNB-USD`,
    name = "Binance Coin",
    mode = "lines+markers"
  )
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin",
    mode = "lines+markers"
  )
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum",
    mode = "lines+markers"
  )
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP",
    mode = "lines+markers"
  )
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500",
    mode = "lines+markers"
  )
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  add_trace(
    y = ~GLD,
    name = "Gold Spot Price",
    mode = "lines+markers"
  )


# time series plot of daily log returns
dailyLogReturnTimeSeries <- plot_ly(
  data = underlyingsLogReturns,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "scatter",
  mode = 'lines+markers'
)
dailyLogReturnTimeSeries <- dailyLogReturnTimeSeries %>%
  add_trace(
    y = ~`BNB-USD`,
    name = "Binance Coin",
    mode = "lines+markers"
  )
dailyLogReturnTimeSeries <- dailyLogReturnTimeSeries %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin",
    mode = "lines+markers"
  )
dailyLogReturnTimeSeries <- dailyLogReturnTimeSeries %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum",
    mode = "lines+markers"
  )
dailyLogReturnTimeSeries <- dailyLogReturnTimeSeries %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP",
    mode = "lines+markers"
  )
dailyLogReturnTimeSeries <- dailyLogReturnTimeSeries %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500",
    mode = "lines+markers"
  )
dailyLogReturnTimeSeries <- dailyLogReturnTimeSeries %>%
  add_trace(
    y = ~GLD,
    name = "Gold Spot Price",
    mode = "lines+markers"
  )


#  now let's make some boxplots of the distribution of our daily log returns
dailyLogReturnBoxplots <- plot_ly(
  data = underlyingsLogReturns,
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "box"
)
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  add_trace(
    y = ~`BNB-USD`,
    name = "Binance Coin"
  )
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin"
  )
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum"
  )
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP"
  )
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500"
  )
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  add_trace(
    y = ~GLD,
    name = "Gold Spot Price"
  )

# now let's apply some changes to the plot so the user
# can switch between a box plot and histogram view of distributions

# first let's build our buttons
chartType <- list(
  type = "buttons",
  direction = "right",
  xanchor = "center",
  yanchor = "top",
  pad = list(
    "r" = 0,
    "t" = 0,
    "b" = 0
  ),
  x = 0.5,
  y = 1.27,
  buttons = list(
    list(
      method = "restyle",
      args = list(
        "type",
        "box"
      ),
      label = "Boxplot"
    ),
    list(
      method = "restyle",
      args = list(
        list(
          "type",
          "histogram"
        ),
        list(
          "opacity",
          0.3
        ),
        list(
          "nbinsy",
          20
        )
      ),
      label = "Histogram"
    )
      )
    )
  


# this is what the buttons are will say
annotations <- list(
  text = "Chart<br>Type", 
  x = 0.2,
  y = 1.25,
  xref = "paper",
  yref = "paper",
  showarrow = FALSE
)

# here is where we apply the changes to the layout of the graph
dailyLogReturnBoxplots <- dailyLogReturnBoxplots %>%
  layout(
    updatemenus = list(
      chartType
    ),
    annotations = annotations
  )
















# lets make a df of weekly log returns
underlyingsWeeklyLogReturns <- apply.weekly(
  underlyingsLogReturns, 
  colSums
  )

# lets make a df of monthly log returns
underlyingsMonthlyLogReturns <- apply.monthly(
  underlyingsLogReturns, 
  colSums)

# lets make a df of quarterly log returns
underlyingsQuarterlyLogReturns <- apply.quarterly(
  underlyingsLogReturns, 
  colSums
  )

# plot log returns
f <- plot.zoo(underlyingsLogReturns)
ggplotly(f)

# same but with vertical bars
plot.zoo(
  underlyingsLogReturns, 
  type = "h"
  )

# plot our weekly log returns
plot.zoo(
  underlyingsWeeklyLogReturns, 
  type = "h"
  )

# plot our monthly log returns
plot.zoo(
  underlyingsMonthlyLogReturns, 
  type = "h"
  )

# plot our quarterly log returns
plot.zoo(
  underlyingsQuarterlyLogReturns, 
  type = "h"
  )

pairs(underlyingsLogReturns)
pairs(underlyingsWeeklyLogReturns)
pairs(underlyingsMonthlyLogReturns)
pairs(underlyingsQuarterlyLogReturns)

# find mean daily log return for btc
mu <- mean(
  underlyingsLogReturns$`BTC-USD`
  )

# standard deviation of btc daily log returns
sigma <- sd(underlyingsLogReturns$`BTC-USD`)

# plot a histogram of the daily log returns of btv
hist(
  underlyingsLogReturns$`BTC-USD`, 
  nclass = 20,
  probability = TRUE
  )

lines(
  underlyingsLogReturns$`BTC-USD`,
  dnorm(
    underlyingsLogReturns$`BTC-USD`,
    mean = mu, 
    sd = sigma
    ),
  col = "red", 
  type = "p"
  )

# kernel density estimate
plot(
  density(
    underlyingsLogReturns$`BTC-USD`
    )
  )

# still having pproblems with these lines calls
lines(
  underlyingsLogReturns$`BTC-USD`,
  dnorm(
    underlyingsLogReturns$`BTC-USD`,
    mean = mu, 
    sd = sigma
    ),
  col = "red",
  type = "p"
  )

# q-q plot of BTC log returns against a normal distribution
qqnorm(
  underlyingsMonthlyLogReturns$`BTC-USD`
  )
qqline(
  underlyingsLogReturns$`BTC-USD`, 
  col = "red"
  )

# calculate skewness of btc log returns
skewness(
  underlyingsLogReturns$`BTC-USD`
  )

# kurtosis of btc log returns
kurtosis(
  underlyingsLogReturns$`BTC-USD`
  )

# jarque bera test for normality
jarque.test(
  as.vector(
    underlyingsLogReturns$`BTC-USD`
    )
  )

# we can apply the skewness and kurtosis of 
# the distribution of the log returns to each
# underlying and plot a comparison
skewScores <- apply(
  underlyingsLogReturns, 
  2, 
  skewness
  )
kurtosisScores <- apply(
  underlyingsLogReturns, 
  2,
  kurtosis
  )

# and now to plot them
plot(
  skewScores, 
  kurtosisScores, 
  type = "n"
  )
text(
  skewScores, 
  kurtosisScores, 
  names(
    skewScores
    ), 
  cex = 0.6,
  col = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  )

# calculate the p-value for jarque bera test for normality
# for each underlying in daily weekly and monthly log returns
apply(
  underlyingsLogReturns, 
  2, 
  function(v){jarque.test(v)$p.value}
  )
apply(
  underlyingsWeeklyLogReturns, 
  2, 
  function(v){jarque.test(v)$p.value}
  )
apply(
  underlyingsMonthlyLogReturns, 
  2, 
  function(v){jarque.test(v)$p.value}
  )
apply(
  underlyingsQuarterlyLogReturns, 
  2, 
  function(v){jarque.test(v)$p.value}
  )

# let's create a student's-T distribution instead
# first we fit it to the data
tfit <- fit.st(
  underlyingsLogReturns$`BTC-USD`
  )

# now we can use this fit to determine mean and sd
tpars <- tfit$par.ests
nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# Plot a histogram of our log returns again
hist(
  underlyingsLogReturns$`BTC-USD`, 
  nclass = 20, 
  probability = TRUE,
  ylim = range(
    0, 
    18
    )
  )

# Compute the fitted t density based 
# on the metrics obtained by our fit
yvals <- dt((underlyingsLogReturns$`BTC-USD` - mu)/sigma, df = nu)/sigma

# Superimpose a red line to show the fitted t density
lines(
  underlyingsLogReturns$`BTC-USD`, 
  yvals, 
  col = "red",
  type = "p"
  )

# alright now we are going to look at the acf of the underlyings
# first we set the plot so we get two columns and the number of rows
# as there are underlyings we want to look at
par(
  mfrow = c(
    ncol(underlyingsLogReturns) / 3, 3
    )
  )

# now we plot the acfs
for (colName in names(underlyingsLogReturns)){
  acf(
    underlyingsLogReturns[, colName], 
    na.action = na.pass, 
    main = paste(colName, "ACF")
    )
}

# now let's do this for absolute values
par(
  mfrow = c(
    ncol(underlyingsLogReturns) / 3, 3
    )
  )
for (colName in names(underlyingsLogReturns)){
  acf(
    abs(underlyingsLogReturns[, colName]), 
    na.action = na.pass, 
    main = paste(colName, "Absolute Values ACF")
  )
}

# now let's do this for squared values
par(
  mfrow = c(
    ncol(underlyingsLogReturns) / 3, 3
    )
  )
for (colName in names(underlyingsLogReturns)){
  acf(
    underlyingsLogReturns[, colName] ^ 2, 
    na.action = na.pass, 
    main = paste(colName, "Squared Values ACF")
  )
}




################################
################################
################################
#### Application Time Y'all ####
################################
################################
################################




app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(
        figure = adjReturnsTimeSeries,
        id = "adjReturnsTimeSeries"
        ),
      dccGraph(
        figure = dailyLogReturnTimeSeries,
        id = "dailyLogReturnTimeSeries"
      ),
      dccGraph(
        figure = dailyLogReturnBoxplots,
        id = "dailyLogReturnBoxplots"
      )
    )
  )
)

app$run_server(
  debug=TRUE, 
  dev_tools_hot_reload=FALSE
  )