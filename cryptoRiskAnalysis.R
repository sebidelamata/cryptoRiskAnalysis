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
  "DOGE-USD",
  "XRP-USD",
  "GLD",
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
  ) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin",
    mode = "lines+markers"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum",
    mode = "lines+markers"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP",
    mode = "lines+markers"
  ) %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500",
    mode = "lines+markers"
  ) %>%
  add_trace(
    y = ~GLD,
    name = "GLD",
    mode = "lines+markers"
  )

# updatemenus component
updatemenus <- list(
  list(
    active = TRUE,
    type= 'dropdown',
    buttons = list(
      list(
        label = "Bitcoin",
        method = "update",
        args = list(
          list(
            visible = c(
              TRUE,
              FALSE,
              FALSE,
              FALSE,
              FALSE,
              FALSE
            )
          ),
          list(
            title = "BTC Bitcoin"
          )
        )
      ),
      list(
        label = "Dogecoin",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE, 
              TRUE,
              FALSE,
              FALSE,
              FALSE,
              FALSE
            )
          ),
          list(
            title = "DOGE Dogecoin"
          )
        )
      ),
      list(
        label = "Ethereum",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE, 
              FALSE,
              TRUE,
              FALSE,
              FALSE,
              FALSE
            )
          ),
          list(
            title = "ETH Ethereum"
          )
        ) 
      ),
      list(
        label = "XRP",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE, 
              FALSE,
              FALSE,
              TRUE,
              FALSE,
              FALSE
            )
          ),
          list(
            title = "XRP Ripple"
          )
        )
      ),
      list(
        label = "S&P 500",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE, 
              FALSE,
              FALSE,
              FALSE,
              TRUE,
              FALSE
            )
          ),
          list(
            title = "SPY S&P 500 ETF"
          )
        ) 
      ),
      list(
        label = "GLD",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE, 
              FALSE,
              FALSE,
              FALSE,
              FALSE,
              TRUE
            )
          ),
          list(
            title = "GLD SPDR Gold Shares ETF"
          )
        ) 
      )
    )
  )
)


# now lets add the annotation
adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
  layout(
    updatemenus = updatemenus,
    showlegend = FALSE
  )


# time series plot of daily log returns
dailyLogReturnTimeSeries <- plot_ly(
  data = underlyingsLogReturns,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "bar"
) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP"
  ) %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500"
  ) %>%
  add_trace(
    y = ~GLD,
    name = "GLD"
  )


# time series plot of weekly log returns
weeklyLogReturnTimeSeries <- plot_ly(
  data = weeklyLogReturns,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "bar"
) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP"
  ) %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500"
  ) %>%
  add_trace(
    y = ~GLD,
    name = "GLD"
  )


# time series plot of monthly log returns
monthlyLogReturnTimeSeries <- plot_ly(
  data = monthlyLogReturns,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "bar"
) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP"
  ) %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500"
  ) %>%
  add_trace(
    y = ~GLD,
    name = "GLD"
  )


# time series plot of quarterly log returns
quarterlyLogReturnTimeSeries <- plot_ly(
  data = quarterlyLogReturns,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "bar"
) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP"
  ) %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500"
  ) %>%
  add_trace(
    y = ~GLD,
    name = "GLD"
  )


# time series plot of annual log returns
annualLogReturnTimeSeries <- plot_ly(
  data = annualLogReturns,
  x = ~Date, 
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "bar"
) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP"
  ) %>%
  add_trace(
    y = ~SPY,
    name = "S&P 500"
  ) %>%
  add_trace(
    y = ~GLD,
    name = "GLD"
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
    name = "GLD"
  )


# let's make a way to look at our underlyings against market (SPY) beta
portfolioBeta <- plot_ly(
  data = underlyingsLogReturns,
  x = ~SPY,
  type = "scatter"
  ) %>%
  add_trace(
    y = ~`BTC-USD`,
    name = "Bitcoin",
    mode = "markers"
  ) %>%
  add_trace(
    y = ~`DOGE-USD`,
    name = "Dogecoin",
    mode = "markers"
  ) %>%
  add_trace(
    y = ~`ETH-USD`,
    name = "Ethereum",
    mode = "markers"
  ) %>%
  add_trace(
    y = ~`XRP-USD`,
    name = "XRP",
    mode = "markers"
  )


# let's make a regression line for our beta coefficients
BTCBetaFit <- lm(
  `BTC-USD` ~ SPY,
  data = underlyingsLogReturns
  )
DogeBetaFit <- lm(
  `DOGE-USD` ~ SPY,
  data = underlyingsLogReturns
)
EthereumBetaFit <- lm(
  `ETH-USD` ~ SPY,
  data = underlyingsLogReturns
)
XRPBetaFit <- lm(
  `XRP-USD` ~ SPY,
  data = underlyingsLogReturns
)

# now we can add the regression line to the graph
# the second part adds the coefficient of the line
portfolioBeta <- portfolioBeta %>%
  add_trace(
    y = fitted(
      BTCBetaFit
    ),
    mode = "lines"
  ) %>%
  add_trace(
    y = fitted(
      DogeBetaFit
    ),
    mode = "lines"
  ) %>%
  add_trace(
    y = fitted(
      EthereumBetaFit
    ),
    mode = "lines"
  ) %>%
  add_trace(
    y = fitted(
      XRPBetaFit
    ),
    mode = "lines"
  )

# updatemenus component
updatemenus <- list(
  list(
   active = TRUE,
    type= 'dropdown',
    buttons = list(
      list(
        label = "Bitcoin",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE,
              TRUE, 
              FALSE,
              FALSE,
              FALSE,
              TRUE,
              FALSE,
              FALSE,
              FALSE
              )
            ),
          list(
            title = paste(
              "Bitcoin Market Beta of\n", 
              round(
                BTCBetaFit$coefficients[2],
                3
                )
              )
            )
          )
        ),
      list(
        label = "Dogecoin",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE,
              FALSE, 
              TRUE,
              FALSE,
              FALSE,
              FALSE, 
              TRUE,
              FALSE,
              FALSE
              )
          ),
          list(
            title = paste(
              "Dogecoin Market Beta of\n", 
              round(
                DogeBetaFit$coefficients[2],
                3
              )
            )
          )
        )
      ),
      list(
        label = "Ethereum",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE,
              FALSE, 
              FALSE,
              TRUE,
              FALSE,
              FALSE, 
              FALSE,
              TRUE,
              FALSE
              )
          ),
          list(
            title = paste(
              "Ethereum Market Beta of\n", 
              round(
                EthereumBetaFit$coefficients[2],
                3
              )
            )
          )
        ) 
      ),
      list(
        label = "XRP",
        method = "update",
        args = list(
          list(
            visible = c(
              FALSE,
              FALSE, 
              FALSE,
              FALSE,
              TRUE,
              FALSE, 
              FALSE,
              FALSE,
              TRUE
              )
          ),
          list(
            title = paste(
              "XRP Market Beta of\n", 
              round(
                XRPBetaFit$coefficients[2],
                3
              )
              )
          )
        )
      )
      )
    )
  )
      

# now lets add the annotation
portfolioBeta <- portfolioBeta %>%
  layout(
    updatemenus = updatemenus,
    showlegend = FALSE
  )


################################
################################
################################
#### Application Time Y'all ####
################################
################################
################################




# let's initialize our application
# we can declare our external CSS stylesheet here
app <- Dash$new(
  external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css"
)

# this is where we layout the look of the application
app$layout(
  htmlDiv(
    list(
      htmlH1("HODL-dar"),
      htmlH2("Cryptocurrency Risk Analysis Tool"),
      htmlBr(),
      htmlBr(),
      htmlH3("Adjusted Closing Price"),
      dccGraph(
        figure = adjReturnsTimeSeries,
        id = "adjReturnsTimeSeries"
        ),
      htmlBr(),
      htmlBr(),
      htmlH3("Aggregated Log Returns"),
      dccTabs(id = "tabs", 
              children=list(
                dccTab(
                  label='D', 
                  children=list(
                    htmlDiv(
                      list(
                        dccGraph(
                          figure = dailyLogReturnTimeSeries,
                          id = "dailyLogReturnTimeSeries"
                          )
                        )
                      )
                    )
                  ),
        dccTab(
          label='W', 
          children=list(
            htmlDiv(
              list(
                dccGraph(
                  figure = weeklyLogReturnTimeSeries,
                  id = "weeklyLogReturnTimeSeries"
                  )
                )
              )
            )
          ),
        dccTab(
          label='M', 
          children=list(
            htmlDiv(
              list(
                dccGraph(
                  figure = monthlyLogReturnTimeSeries,
                  id = "monthlyLogReturnTimeSeries"
                  )
                )
              )
            )
          ),
        dccTab(
          label='Q', 
          children=list(
            htmlDiv(
              list(
                dccGraph(
                  figure = quarterlyLogReturnTimeSeries,
                  id = "quarterlyLogReturnTimeSeries"
                  )
                )
              )
            )
          ),
        dccTab(
          label='Y', 
          children=list(
            htmlDiv(
              list(
                dccGraph(
                  figure = annualLogReturnTimeSeries,
                  id = "annualLogReturnTimeSeries"
                  )
                )
              )
            )
          )
        )
        ),
      htmlBr(),
      htmlBr(),
      htmlH3("Distribution of Log Returns"),
      dccGraph(
        figure = dailyLogReturnBoxplots,
        id = "dailyLogReturnBoxplots"
      ),
      htmlBr(),
      htmlBr(),
      htmlH3("Market Risk Factor"),
      dccGraph(
        figure = portfolioBeta,
        id = "portfolioBeta"
      )
    )
  )
)

app$run_server(
  dev_tools_hot_reload=FALSE
  )
