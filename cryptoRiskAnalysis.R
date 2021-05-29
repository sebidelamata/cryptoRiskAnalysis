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

# let's next do a weekly aggregation
# first we need to define a week
weekCuts <- as.Date(
  cut(
    underlyingsLogReturns$Date,
    "week"
  )
)
# now lets we can do the aggregating
weeklyLogReturns <- aggregate(
  . ~ weekCuts,
  underlyingsLogReturns,
  sum
)
# now we need to rename our weekCuts column to 
# date and drop our original date column
weeklyLogReturns <- weeklyLogReturns %>%
  select(
    -c(Date)
  ) %>%
  rename(
    Date = weekCuts
  )


# now let's repeat this monthly
# first we need to define a month
monthCuts <- as.Date(
  cut(
    underlyingsLogReturns$Date,
    "month"
  )
)
# now lets we can do the aggregating
monthlyLogReturns <- aggregate(
  . ~ monthCuts,
  underlyingsLogReturns,
  sum
)
# now we need to rename our weekCuts column to 
# date and drop our original date column
monthlyLogReturns <- monthlyLogReturns %>%
  select(
    -c(Date)
  ) %>%
  rename(
    Date = monthCuts
  )


# now let's repeat this quarterly
# first we need to define a month
quarterCuts <- as.Date(
  cut(
    underlyingsLogReturns$Date,
    "quarter"
  )
)
# now lets we can do the aggregating
quarterlyLogReturns <- aggregate(
  . ~ quarterCuts,
  underlyingsLogReturns,
  sum
)
# now we need to rename our weekCuts column to 
# date and drop our original date column
quarterlyLogReturns <- quarterlyLogReturns %>%
  select(
    -c(Date)
  ) %>%
  rename(
    Date = quarterCuts
  )


# now let's repeat this annually
# first we need to define a month
yearCuts <- as.Date(
  cut(
    underlyingsLogReturns$Date,
    "year"
  )
)
# now lets we can do the aggregating
annualLogReturns <- aggregate(
  . ~ yearCuts,
  underlyingsLogReturns,
  sum
)
# now we need to rename our weekCuts column to 
# date and drop our original date column
annualLogReturns <- annualLogReturns %>%
  select(
    -c(Date)
  ) %>%
  rename(
    Date = yearCuts
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


# TODO: create a button to switch granularity
# time series plot of daily log returns
dailyLogReturnTimeSeries <- plot_ly(
  data = quarterlyLogReturns,
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
          "type",
          "histogram"
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


# let's make a way to look at our underlyings against market (SPY) beta
portfolioBeta <- plot_ly(
  data = underlyingsLogReturns,
  x = ~SPY,
  y = ~`BTC-USD`,
  type = "scatter",
  mode = "markers"
  )

# let's make a regression line for our beta coefficient
portfolioBetaFit <- lm(
  `BTC-USD` ~ SPY,
  data = underlyingsLogReturns
  )

# now we can add the regression line to the graph
# the second part adds the coefficient of the line
portfolioBeta <- portfolioBeta %>%
  add_lines(
    x = ~SPY,
    y = fitted(
      portfolioBetaFit
    )
  )

# new annotations
annotations <- list(
  x = 0.2,
  y = 0.5,
  text = paste(
    "Market Beta of\n", 
    round(
      portfolioBetaFit$coefficients[2],
      2
      ),
    ""
  ),
  showarrow = FALSE,
  xref = "paper",
  yref = "paper"
)

# now lets add the annotation
portfolioBeta <- portfolioBeta %>%
  layout(
    annotations = annotations
  )


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
      ),
      dccGraph(
        figure = portfolioBeta,
        id = "portfolioBeta"
      )
    )
  )
)

app$run_server(
  debug=TRUE, 
  dev_tools_hot_reload=FALSE
  )