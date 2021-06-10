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
# bootstrap stuff
library(dashBootstrapComponents)


#####################
## Set Random Seed ##
#####################

set.seed(666)


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



# let's claim the colors we want to use here
colors <- list(
  background = '#121212',
  text = '#f2f5f0'
)



# this and any other commented out graphs have been moved to the callback section 
# due to having interactive buttons added but I wanted to keep the original here bc
# it makes more sense to read this way (to me).

# time series plot of prices
# adjReturnsTimeSeries <- plot_ly(
#   data = underlyingsDF,
#   x = ~Date, 
#   y = ~`BTC-USD`,
#   name = "Bitcoin",
#   type = "scatter",
#   mode = 'lines+markers'
#   ) %>%
#   add_trace(
#     y = ~`DOGE-USD`,
#     name = "Dogecoin",
#     mode = "lines+markers"
#   ) %>%
#   add_trace(
#     y = ~`ETH-USD`,
#     name = "Ethereum",
#     mode = "lines+markers"
#   ) %>%
#   add_trace(
#     y = ~`XRP-USD`,
#     name = "XRP",
#     mode = "lines+markers"
#   ) %>%
#   add_trace(
#     y = ~SPY,
#     name = "S&P 500",
#     mode = "lines+markers"
#   ) %>%
#   add_trace(
#     y = ~GLD,
#     name = "GLD",
#     mode = "lines+markers"
#   )
# 
# # updatemenus component
# updatemenus <- list(
#   list(
#     active = -1,
#     type= 'dropdown',
#     buttons = list(
#       list(
#         label = "Bitcoin",
#         method = "update",
#         args = list(
#           list(
#             visible = c(
#               TRUE,
#               FALSE,
#               FALSE,
#               FALSE,
#               FALSE,
#               FALSE
#             )
#           ),
#           list(
#             title = "BTC Bitcoin"
#           )
#         )
#       ),
#       list(
#         label = "Dogecoin",
#         method = "update",
#         args = list(
#           list(
#             visible = c(
#               FALSE, 
#               TRUE,
#               FALSE,
#               FALSE,
#               FALSE,
#               FALSE
#             )
#           ),
#           list(
#             title = "DOGE Dogecoin"
#           )
#         )
#       ),
#       list(
#         label = "Ethereum",
#         method = "update",
#         args = list(
#           list(
#             visible = c(
#               FALSE, 
#               FALSE,
#               TRUE,
#               FALSE,
#               FALSE,
#               FALSE
#             )
#           ),
#           list(
#             title = "ETH Ethereum"
#           )
#         ) 
#       ),
#       list(
#         label = "XRP",
#         method = "update",
#         args = list(
#           list(
#             visible = c(
#               FALSE, 
#               FALSE,
#               FALSE,
#               TRUE,
#               FALSE,
#               FALSE
#             )
#           ),
#           list(
#             title = "XRP Ripple"
#           )
#         )
#       ),
#       list(
#         label = "S&P 500",
#         method = "update",
#         args = list(
#           list(
#             visible = c(
#               FALSE, 
#               FALSE,
#               FALSE,
#               FALSE,
#               TRUE,
#               FALSE
#             )
#           ),
#           list(
#             title = "SPY S&P 500 ETF"
#           )
#         ) 
#       ),
#       list(
#         label = "GLD",
#         method = "update",
#         args = list(
#           list(
#             visible = c(
#               FALSE, 
#               FALSE,
#               FALSE,
#               FALSE,
#               FALSE,
#               TRUE
#             )
#           ),
#           list(
#             title = "GLD SPDR Gold Shares ETF"
#           )
#         ) 
#       )
#     )
#   )
# )
# 
# 
# # now lets add the annotation button and stylings and shit
# adjReturnsTimeSeries <- adjReturnsTimeSeries %>%
#   layout(
#     updatemenus = updatemenus,
#     showlegend = FALSE,
#     plot_bgcolor = colors$background,
#     paper_bgcolor = colors$background,
#     font = list(
#       color = colors$text
#     )
#   )


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
  ) %>%
  layout(
    plot_bgcolor = colors$background,
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
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
  ) %>%
  layout(
    plot_bgcolor = colors$background,
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
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
  ) %>%
  layout(
    plot_bgcolor = colors$background,
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
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
  ) %>%
  layout(
    plot_bgcolor = colors$background,
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
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
  ) %>%
  layout(
    plot_bgcolor = colors$background,
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
  )


#  now let's make some boxplots of the distribution of our daily log returns
dailyLogReturnBoxplots <- plot_ly(
  data = underlyingsLogReturns,
  y = ~`BTC-USD`,
  name = "Bitcoin",
  type = "box"
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
  ) %>%
  layout(
    plot_bgcolor = colors$background,
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
  )


# let's make a way to look at our underlyings against market (SPY) beta
portfolioBeta <- plot_ly(
  data = underlyingsLogReturns,
  x = ~SPY,
  type = "scatter",
  opacity = 0.65,
  marker = list(
    size = 15,
    line = list(
      width = 0.5, 
      color = colors$text)
    )
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
    mode = "lines",
    opacity = 1,
    line = list(
      color = colors$text, 
      width = 3
      ),
    marker = list(
      size = 0.1,
      line = list(
        width = 0.0001, 
        color = colors$text
      )
    )
  ) %>%
  add_trace(
    y = fitted(
      DogeBetaFit
    ),
    mode = "lines",
    opacity = 1,
    line = list(
      color = colors$text, 
      width = 3
    ),
    marker = list(
      size = 0.1,
      line = list(
        width = 0.0001, 
        color = colors$text
      )
    )
  ) %>%
  add_trace(
    y = fitted(
      EthereumBetaFit
    ),
    mode = "lines",
    opacity = 1,
    line = list(
      color = colors$text, 
      width = 3
    ),
    marker = list(
      size = 0.1,
      line = list(
        width = 0.0001, 
        color = colors$text
      )
    )
  ) %>%
  add_trace(
    y = fitted(
      XRPBetaFit
    ),
    mode = "lines",
    opacity = 1,
    line = list(
      color = colors$text, 
      width = 3
    ),
    marker = list(
      size = 0.1,
      line = list(
        width = 0.0001, 
        color = colors$text
      )
    )
  )

# updatemenus component
updatemenus <- list(
  list(
   active = -1,
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
    showlegend = FALSE,
    plot_bgcolor = colors$background,
    yaxis = list(
      title = ""
    ),
    paper_bgcolor = colors$background,
    font = list(
      color = colors$text
    )
  )


# # VaR Graph
# 
# # lets calculate the density of btc
# densityBTC <- density(underlyingsLogReturns$`BTC-USD`)
# # now we can calculate VaR to the 90%
# VaRBTC <- qnorm(
#   0.95,
#   mean = mean(underlyingsLogReturns$`BTC-USD`),
#   sd = sd(underlyingsLogReturns$`BTC-USD`)
#   )
# # now lets calculate expexted shortfall
# ESBTC <- ESnorm(
#   0.95,
#   mu = mean(underlyingsLogReturns$`BTC-USD`),
#   sd = sd(underlyingsLogReturns$`BTC-USD`)
# )
# 
# # lets calculate the density of doge
# densityDoge <- density(underlyingsLogReturns$`DOGE-USD`)
# # now we can calculate VaR to the 90%
# VaRDoge <- qnorm(
#   0.95,
#   mean = mean(underlyingsLogReturns$`DOGE-USD`),
#   sd = sd(underlyingsLogReturns$`DOGE-USD`)
# )
# # now lets calculate expexted shortfall
# ESDoge <- ESnorm(
#   0.95,
#   mu = mean(underlyingsLogReturns$`DOGE-USD`),
#   sd = sd(underlyingsLogReturns$`DOGE-USD`)
# )
# 
# # lets calculate the density of ethereum
# densityETH <- density(underlyingsLogReturns$`ETH-USD`)
# # now we can calculate VaR to the 90%
# VaRETH <- qnorm(
#   0.95,
#   mean = mean(underlyingsLogReturns$`ETH-USD`),
#   sd = sd(underlyingsLogReturns$`ETH-USD`)
# )
# # now lets calculate expexted shortfall
# ESETH <- ESnorm(
#   0.95,
#   mu = mean(underlyingsLogReturns$`ETH-USD`),
#   sd = sd(underlyingsLogReturns$`ETH-USD`)
# )
# 
# # lets calculate the density of xrp
# densityXRP <- density(underlyingsLogReturns$`XRP-USD`)
# # now we can calculate VaR to the 90%
# VaRXRP <- qnorm(
#   0.95,
#   mean = mean(underlyingsLogReturns$`XRP-USD`),
#   sd = sd(underlyingsLogReturns$`XRP-USD`)
# )
# # now lets calculate expexted shortfall
# ESXRP <- ESnorm(
#   0.95,
#   mu = mean(underlyingsLogReturns$SPY),
#   sd = sd(underlyingsLogReturns$SPY)
# )
# 
# # lets calculate the density of spy
# densitySPY <- density(underlyingsLogReturns$SPY)
# # now we can calculate VaR to the 90%
# VaRSPY <- qnorm(
#   0.95,
#   mean = mean(underlyingsLogReturns$SPY),
#   sd = sd(underlyingsLogReturns$SPY)
# )
# # now lets calculate expexted shortfall
# ESSPY <- ESnorm(
#   0.95,
#   mu = mean(underlyingsLogReturns$SPY),
#   sd = sd(underlyingsLogReturns$SPY)
# )
# 
# # lets calculate the density of gld
# densityGLD <- density(underlyingsLogReturns$GLD)
# # now we can calculate VaR to the 90%
# VaRGLD <- qnorm(
#   0.95,
#   mean = mean(underlyingsLogReturns$GLD),
#   sd = sd(underlyingsLogReturns$GLD)
# )
# # now lets calculate expexted shortfall
# ESGLD <- ESnorm(
#   0.95,
#   mu = mean(underlyingsLogReturns$GLD),
#   sd = sd(underlyingsLogReturns$GLD)
# )
# 
# 
# # the actual graphing part
# # let's do our daily VaR and cVaR graph
# VaRPlot <- plot_ly(
#   data = underlyingsLogReturns,
#   x = ~densityBTC$x,
#   y = ~densityBTC$y,
#   type = "scatter",
#   mode = "lines",
#   fill = "tozeroy",
#   alpha = 0.6
# ) %>%
#   add_segments(
#     x = VaRBTC, 
#     xend = VaRBTC, 
#     y = 0, 
#     yend = 20
#     ) %>%
#   add_segments(
#     x = ESBTC, 
#     xend = ESBTC, 
#     y = 0, 
#     yend = 20
#   ) %>%
#   layout(
#     plot_bgcolor = colors$background,
#     paper_bgcolor = colors$background,
#     font = list(
#       color = colors$text
#     )
#   )



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
  external_stylesheets = dbcThemes$SPACELAB
)

# this is where we layout the look of the application
app$layout(
  htmlDiv(
    list(
      htmlH1(
        "HODL-dar",
        style = list(
          textAlign = "right",
          color = colors$text
        )
      ),
      htmlH6(
        "Cryptocurrency Risk Analysis Tool",
        style = list(
          textAlign = "right",
          color = colors$text
        )
      ),
      htmlBr(),
      htmlBr(),
      htmlH3(
        "Adjusted Closing Price",
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      dccGraph(
        figure = adjReturnsTimeSeries,
        id = "adjReturnsTimeSeries"
        ),
      htmlBr(),
      dbcRadioItems(
        id = "priceTimeSeriesSelector",
        options=list(
          list(
            "label" = "1W", 
            "value" = "1W"
            ),
          list(
            "label" = "1M", 
            "value" = "1M"
            ),
          list(
            "label" = "3M", 
            "value" = "3M"
            ),
          list(
            "label" = "1Y", 
            "value" = "1Y"
            ),
          list(
            "label" = "5Y", 
            "value" = "5Y"
            ),
          list(
            "label" = "Max",
            "value" = "Max"
          )
        ),
        value = "Max",
        inline = TRUE,
        labelStyle = list(
          "display" = "inline-block"
          ),
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      htmlBr(),
      htmlBr(),
      htmlH3(
        "Aggregated Log Returns",
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      dccGraph(
        figure = dailyLogReturnTimeSeries,
        id = "dailyLogReturnTimeSeries"
      ),
      htmlBr(),
      dbcRadioItems(
        id = "logTimeSeriesSelector",
        options=list(
          list(
            "label" = "1W", 
            "value" = "1W"
          ),
          list(
            "label" = "1M", 
            "value" = "1M"
          ),
          list(
            "label" = "3M", 
            "value" = "3M"
          ),
          list(
            "label" = "1Y", 
            "value" = "1Y"
          ),
          list(
            "label" = "5Y", 
            "value" = "5Y"
          ),
          list(
            "label" = "Max",
            "value" = "Max"
          )
        ),
        value = "Max",
        labelStyle = list(
          "display" = "inline-block"
        ),
        inline = TRUE,
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      htmlBr(),
      dbcRadioItems(
        id = "logTimePeriodSelector",
        options=list(
          list(
            "label" = "Daily\nLog Returns", 
            "value" = "D"
          ),
          list(
            "label" = "Weekly\nLog Returns", 
            "value" = "W"
          ),
          list(
            "label" = "Monthly\nLog Returns", 
            "value" = "M"
          ),
          list(
            "label" = "Quarterly\nLog Returns", 
            "value" = "Q"
          ),
          list(
            "label" = "Annual\nLog Returns", 
            "value" = "A"
          )
        ),
        value = "D",
        labelStyle = list(
          "display" = "inline-block"
        ),
        inline = TRUE,
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      htmlBr(),
      htmlBr(),
      htmlH3(
        "Distribution of Log Returns",
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      dccGraph(
        figure = dailyLogReturnBoxplots,
        id = "dailyLogReturnBoxplots"
      ),
      htmlBr(),
      htmlBr(),
      htmlH3(
        "Market Risk Factor",
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      dccGraph(
        figure = portfolioBeta,
        id = "portfolioBeta"
      ),
      htmlBr(),
      htmlBr(),
      htmlH3(
        "Value-at-Risk",
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      dccGraph(
        id = "VaRPlot"
      ),
      htmlBr(),
      dbcRadioItems(
        id = "VaRChecklist",
        options=list(
          list(
            "label" = "Bitcoin", 
            "value" = "BTC"
            ),
          list(
            "label" = "Dogecoin", 
            "value" = "DOGE"
            ),
          list(
            "label" = "Ethereum", 
            "value" = "ETH"
            ),
          list(
            "label" = "XRP", 
            "value" = "XRP"
            ),
          list(
            "label" = "S&P 500", 
            "value" = "SPY"
            ),
          list(
            "label" = "GLD Gold Shares ETF", 
            "value" = "GLD"
            )
        ),
        value = list(
          "BTC"
          ),
        inline = TRUE,
        labelStyle = list(
          "display" = "inline-block"
        ),
        style = list(
          textAlign = "center",
          color = colors$text
        )
      ),
      htmlBr(),
      dbcRadioItems(
        id = "VaRPeriodicitySelector",
        options=list(
          list(
            "label" = "Daily", 
            "value" = "Daily"
          ),
          list(
            "label" = "Weekly", 
            "value" = "Weekly"
          ),
          list(
            "label" = "Monthly", 
            "value" = "Monthly"
          ),
          list(
            "label" = "Quarterly", 
            "value" = "Quarterly"
          ),
          list(
            "label" = "Annualy", 
            "value" = "Annualy"
          )
        ), 
        value = list(
          "Daily"
          ),
        labelStyle = list(
          "display" = "inline-block"
        ),
        inline = TRUE,
        style = list(
          textAlign = "center",
          color = colors$text
        )
        ),
      htmlBr(),
      htmlBr(),
      htmlFooter(
        "The Content is for informational purposes only, you should not construe any such information or other material as legal, tax, investment, financial, or other advice. Nothing contained on our Site constitutes a solicitation, recommendation, endorsement, or offer by Miguel Sebastian de la Mata or any third party service provider to buy or sell any securities or other financial instruments in this or in in any other jurisdiction in which such solicitation or offer would be unlawful under the securities laws of such jurisdiction. All Content on this site is information of a general nature and does not address the circumstances of any particular individual or entity. Nothing in the Site constitutes professional and/or financial advice, nor does any information on the Site constitute a comprehensive or complete statement of the matters discussed or the law relating thereto. Miguel Sebastian de la Mata is not a fiduciary by virtue of any person's use of or access to the Site or Content. You alone assume the sole responsibility of evaluating the merits and risks associated with the use of any information or other Content on the Site before making any decisions based on such information or other Content. In exchange for using the Site, you agree not to hold him, his affiliates or any third party service provider liable for any possible claim for damages arising from any decision you make based on information or other Content made available to you through the Site. There are risks associated with investing in securities. Investing in stocks, bonds, exchange traded funds, mutual funds, cryptocurrencies, and money market funds involve risk of loss.  Loss of principal is possible. Some high risk investments may use leverage, which will accentuate gains & losses. Foreign investing involves special risks, including a greater volatility and political, economic and currency risks and differences in accounting methods. A security's or a firm's past investment performance is not a guarantee or predictor of future investment performance.",
        style = list(
          textAlign = "left",
          color = colors$text,
          fontSize = 6
          )
      )
    ),
    style = list(
      backgroundColor = colors$background,
      borderColor = colors$background
      )
  )
)


# app callback for our price data
app$callback(
  output(
    id = "adjReturnsTimeSeries", 
    property = "figure"
    ),
  params = list(
    input(
      id = "priceTimeSeriesSelector", 
      property = "value"
      )
    ),
  function(value, value2) {
    durationSelected <- NULL
    if (value == "1W") {
      durationSelected <- which(underlyingsDF$Date >= max(underlyingsDF$Date) - 7)
      durationSelected <- underlyingsDF[durationSelected,]
    } else if (value == "1M") {
      durationSelected = which(underlyingsDF$Date >= max(underlyingsDF$Date) - 30)
      durationSelected <- underlyingsDF[durationSelected,]
    } else if (value == "3M") {
      durationSelected = which(underlyingsDF$Date >= max(underlyingsDF$Date) - 90)
      durationSelected <- underlyingsDF[durationSelected,]
    } else if (value == "1Y") {
      durationSelected = which(underlyingsDF$Date >= max(underlyingsDF$Date) - 360)
      durationSelected <- underlyingsDF[durationSelected,]
    } else if (value == "5Y") {
      durationSelected = which(underlyingsDF$Date >= max(underlyingsDF$Date) - 360 * 5)
      durationSelected <- underlyingsDF[durationSelected,]
    } else {
      durationSelected <- underlyingsDF
    }
    
    figure <- plot_ly(
      data = durationSelected,
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
    
    updatemenus <- list(
      list(
        active = -1,
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
    
    figure <- figure %>%
      layout(
        updatemenus = updatemenus,
        yaxis = list(
          title = "Price (USD)",
          tickformat = "$"
          ),
        showlegend = FALSE,
        plot_bgcolor = colors$background,
        paper_bgcolor = colors$background,
        font = list(
          color = colors$text
        )
      )
  }
  )


# we are going to do another app callback for the log returns time series
app$callback(
  output(
    id = 'dailyLogReturnTimeSeries', 
    property = 'figure'
    ),
  params = list(
    input(
      id = 'logTimeSeriesSelector', 
      property = 'value'
      ),
    input(
      id = "logTimePeriodSelector", 
      property = "value"
      )
    ),
  function(value, value2) {
    perdiodicitySelected <- NULL
    if (value2 == "D"){
      perdiodicitySelected <- underlyingsLogReturns
    } else if (value2 == "W"){
      perdiodicitySelected <- weeklyLogReturns
    } else if (value2 == "M"){
      perdiodicitySelected <- monthlyLogReturns
    } else if (value2 == "Q"){
      perdiodicitySelected <- quarterlyLogReturns
    } else {
      perdiodicitySelected <- annualLogReturns
    }
    durationSelected <- NULL
    if (value == "1W") {
      durationSelected <- which(perdiodicitySelected$Date >= max(perdiodicitySelected$Date) - 7)
      durationSelected <- perdiodicitySelected[durationSelected,]
    } else if (value == "1M") {
      durationSelected = which(perdiodicitySelected$Date >= max(perdiodicitySelected$Date) - 30)
      durationSelected <- perdiodicitySelected[durationSelected,]
    } else if (value == "3M") {
      durationSelected = which(perdiodicitySelected$Date >= max(perdiodicitySelected$Date) - 90)
      durationSelected <- perdiodicitySelected[durationSelected,]
    } else if (value == "1Y") {
      durationSelected = which(perdiodicitySelected$Date >= max(perdiodicitySelected$Date) - 365)
      durationSelected <- perdiodicitySelected[durationSelected,]
    } else if (value == "5Y") {
      durationSelected = which(perdiodicitySelected$Date >= max(perdiodicitySelected$Date) - 365 * 5)
      durationSelected <- perdiodicitySelected[durationSelected,]
    } else {
      durationSelected <- perdiodicitySelected
    }
    
    figure <- plot_ly(
      data = durationSelected,
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
      ) %>%
      layout(
        plot_bgcolor = colors$background,
        paper_bgcolor = colors$background,
        yaxis = list(
          title = "Percent return",
          tickformat = "%"
        ),
        font = list(
          color = colors$text
        )
      )
  }
)



# we are going to do another app callback for the VaR
app$callback(
  output(
    id = 'VaRPlot', 
    property = 'figure'
  ),
  params = list(
    input(
      id = 'VaRChecklist', 
      property = 'value'
    ),
    input(
      id = "VaRPeriodicitySelector",
      property = "value"
    )
  ),
  function(item, value2) {
    
    # select what period of time we want to estimate risk for
    perdiodicitySelected <- NULL
    if (value2 == "Daily"){
      perdiodicitySelected <- underlyingsLogReturns
    } else if (value2 == "Weekly"){
      perdiodicitySelected <- weeklyLogReturns
    } else if (value2 == "Monthly"){
      perdiodicitySelected <- monthlyLogReturns
    } else if (value2 == "Quarterly"){
      perdiodicitySelected <- quarterlyLogReturns
    } else {
      perdiodicitySelected <- annualLogReturns
    }
    
    # VaR Graph
    figure <- plot_ly() %>%
      layout(
        plot_bgcolor = colors$background,
        paper_bgcolor = colors$background,
        font = list(
            color = colors$text
          )
        )
    
    if(item == "BTC"){
      # lets calculate the density of btc
      itemDensity <- density(perdiodicitySelected$`BTC-USD`)
      # now we can calculate VaR to the 90%
      itemVaR <- qnorm(
        0.95,
        mean = mean(perdiodicitySelected$`BTC-USD`),
        sd = sd(perdiodicitySelected$`BTC-USD`)
        )
      # now lets calculate expexted shortfall
      itemES <- ESnorm(
        0.95,
        mu = mean(perdiodicitySelected$`BTC-USD`),
        sd = sd(perdiodicitySelected$`BTC-USD`)        
        )
        
      figure %>%
        add_trace(
          x = ~itemDensity$x,
          y = ~itemDensity$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          alpha = 0.6
        ) %>%
        add_segments(
          x = itemVaR, 
          xend = itemVaR, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "VaR"
          ) %>%
        add_segments(
          x = itemES, 
          xend = itemES, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "CVaR"
          )
    } else if(item == "DOGE"){
      # lets calculate the density of btc
      itemDensity <- density(perdiodicitySelected$`DOGE-USD`)
      # now we can calculate VaR to the 90%
      itemVaR <- qnorm(
        0.95,
        mean = mean(perdiodicitySelected$`DOGE-USD`),
        sd = sd(perdiodicitySelected$`DOGE-USD`)
        )
      # now lets calculate expexted shortfall
      itemES <- ESnorm(
        0.95,
        mu = mean(perdiodicitySelected$`DOGE-USD`),
        sd = sd(perdiodicitySelected$`DOGE-USD`)
        )
        
      figure %>%
        add_trace(
          x = ~itemDensity$x,
          y = ~itemDensity$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          alpha = 0.6
          ) %>%
        add_segments(
          x = itemVaR, 
          xend = itemVaR, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "VaR"
          ) %>%
        add_segments(
          x = itemES, 
          xend = itemES, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "CVaR"
          )
     } else if(item == "ETH"){
      # lets calculate the density of btc
      itemDensity <- density(perdiodicitySelected$`ETH-USD`)
      # now we can calculate VaR to the 90%
      itemVaR <- qnorm(
        0.95,
        mean = mean(perdiodicitySelected$`ETH-USD`),
        sd = sd(perdiodicitySelected$`ETH-USD`)
        )
      # now lets calculate expexted shortfall
      itemES <- ESnorm(
        0.95,
        mu = mean(perdiodicitySelected$`ETH-USD`),
        sd = sd(perdiodicitySelected$`ETH-USD`)
        )
        
      figure %>%
        add_trace(
          x = ~itemDensity$x,
          y = ~itemDensity$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          alpha = 0.6
          ) %>%
        add_segments(
          x = itemVaR, 
          xend = itemVaR, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "VaR"
          ) %>%
        add_segments(
          x = itemES, 
          xend = itemES, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "CVaR"
          )
    } else if(item == "XRP"){
      # lets calculate the density of btc
      itemDensity <- density(perdiodicitySelected$`XRP-USD`)
      # now we can calculate VaR to the 90%
      itemVaR <- qnorm(
        0.95,
        mean = mean(perdiodicitySelected$`XRP-USD`),
        sd = sd(perdiodicitySelected$`XRP-USD`)
        )
      # now lets calculate expexted shortfall
      itemES <- ESnorm(
        0.95,
        mu = mean(perdiodicitySelected$`XRP-USD`),
        sd = sd(perdiodicitySelected$`XRP-USD`)
        )
        
      figure %>%
        add_trace(
          x = ~itemDensity$x,
          y = ~itemDensity$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          alpha = 0.6
          ) %>%
        add_segments(
          x = itemVaR, 
          xend = itemVaR, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "VaR"
          ) %>%
        add_segments(
          x = itemES, 
          xend = itemES, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "CVaR"
          )
    } else if(item == "SPY"){
      # lets calculate the density of btc
      itemDensity <- density(perdiodicitySelected$SPY)
      # now we can calculate VaR to the 90%
      itemVaR <- qnorm(
        0.95,
        mean = mean(perdiodicitySelected$SPY),
        sd = sd(perdiodicitySelected$SPY)
        )
      # now lets calculate expexted shortfall
      itemES <- ESnorm(
        0.95,
        mu = mean(perdiodicitySelected$SPY),
        sd = sd(perdiodicitySelected$SPY)
        )
        
      figure %>%
        add_trace(
          x = ~itemDensity$x,
          y = ~itemDensity$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          alpha = 0.6
          ) %>%
        add_segments(
          x = itemVaR, 
          xend = itemVaR, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "VaR"
          ) %>%
        add_segments(
          x = itemES, 
          xend = itemES, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "CVaR"
          )
    } else {
      # lets calculate the density of btc
      itemDensity <- density(perdiodicitySelected$GLD)
      # now we can calculate VaR to the 90%
      itemVaR <- qnorm(
        0.95,
        mean = mean(perdiodicitySelected$GLD),
        sd = sd(perdiodicitySelected$GLD)
        )
      # now lets calculate expexted shortfall
      itemES <- ESnorm(
        0.95,
        mu = mean(perdiodicitySelected$GLD),
        sd = sd(perdiodicitySelected$GLD)
        )
        
      figure %>%
        add_trace(
          x = ~itemDensity$x,
          y = ~itemDensity$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          alpha = 0.6
          ) %>%
        add_segments(
          x = itemVaR, 
          xend = itemVaR, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "VaR"
          ) %>%
        add_segments(
          x = itemES, 
          xend = itemES, 
          y = 0, 
          yend = max(itemDensity$y) * 1.1,
          name = "CVaR"
          )
      }
    }
  )



# now we run our app
app$run_server(
  dev_tools_hot_reload=FALSE,
  debug = TRUE
  )
