## ----load_packages, include=FALSE----------------------------------------
library(wid)
library(knitr)
library(tidyverse)

## ---- eval=FALSE---------------------------------------------------------
#  download_wid(
#      indicators, # Codes corresponding to indicators to retrieve
#      areas, # Areas (mostly countries) for which to retrieve the indicators
#      years, # Years for which to retrieve the indicators
#      perc, # Percentiles (part of the distribution)
#      ages, # Age groups (adults, all ages, elderly, etc.)
#      pop, # Population type (individual, households, tax units, etc.)
#      metadata, # Logical: should it fetch metadata too (eg. sources, etc.)
#      verbose # Logical: should it display messages showing progress
#  )

## ------------------------------------------------------------------------
data <- download_wid(
    indicators = "sptinc", # Shares of pre-tax national income
    areas = "US", # In the United States
    years = 2010:2015, # Time period: 2010-2015
    perc = "p99p100" # Top 1% only
)
kable(data) # Pretty display of the data.frame

## ------------------------------------------------------------------------
data <- download_wid(
    indicators = "sptinc", # Shares of pre-tax national income
    areas = "US", # In the United States
    years = 2010:2015, # Time period: 2010-2015
    perc = "p99p100", # Top 1% only
    metadata = TRUE # Also request metadata
)
colnames(data)

## ---- message=FALSE, fig.align='center', fig.height=4--------------------
data <- download_wid(
    indicators = "shweal", # Shares of personal wealth
    areas = "FR", # In France
    perc = c("p90p100", "p99p100") # Top 1% and top 10%
)

library(ggplot2)
library(scales)

ggplot(data) +
    geom_line(aes(x=year, y=value, color=percentile)) +
    ylab("top share") +
    scale_y_continuous(label=percent) +
    scale_color_discrete(labels=c("p90p100"="top 10%", "p99p100"="top 1%")) + 
    ggtitle("Top 1% and top 10% personal wealth shares in France, 1800-2015")

## ---- message=FALSE, fig.align='center', fig.height=4--------------------
# We use the tidyverse to manipulate the data, see http://tidyverse.org
library(tidyverse)

# Average incomes data
data <- download_wid(
    indicators = "aptinc", # Average pre-tax national income
    areas = c("FR", "CN", "US"), # France, China and United States
    perc = "p0p50", # Bottom half of the population
    pop = "j", # Equal-split individuals
    year = 1978:2015
) %>% rename(value_lcu=value)

# Purchasing power parities with US dollar
ppp <- download_wid(
    indicators = "xlcusp", # US PPP
    areas = c("FR", "CN", "US"), # France, China and United States
    year = 2016 # Reference year only
) %>% rename(ppp=value) %>% select(-year, -percentile)

# Convert from local currency to PPP US dollar
data <- merge(data, ppp, by="country") %>%
    mutate(value_ppp=value_lcu/ppp)

ggplot(data) +
    geom_line(aes(x=year, y=value_ppp, color=country)) +
    ylab("2016 $ PPP") +
    scale_color_discrete(labels=c("CN"="China", "US"="USA", "FR"="France")) + 
    ggtitle("Bottom 50% pre-tax national income")

## ---- message=FALSE, fig.align='center', fig.height=4--------------------
# Average national income data
data <- download_wid(
    indicators = "anninc", # Average net national income
    areas = c("FR", "US", "DE", "GB"),
    ages = 992 # Adults
) %>% rename(value_lcu=value)

# Purchasing power parities with US dollar
ppp <- download_wid(
    indicators = "xlcusp", # US PPP
    areas = c("FR", "US", "DE", "GB"), # France, China and United States
    year = 2016 # Reference year only
) %>% rename(ppp=value) %>% select(-year, -percentile)

# Convert from local currency to PPP US dollar
data <- merge(data, ppp, by="country") %>%
    mutate(value_ppp=value_lcu/ppp)

ggplot(data) +
    geom_line(aes(x=year, y=value_ppp, color=country)) +
    scale_y_log10(breaks=c(2e3, 5e3, 1e4, 2e4, 5e4)) +
    ylab("2016 $ PPP") +
    scale_color_discrete(
        labels=c("US"="USA", "FR"="France", "DE"="Germany", "GB"="UK")
    ) + 
    ggtitle("Average net national income per adult")

## ---- message=FALSE, fig.align='center', fig.height=4--------------------
data <- download_wid(
    indicators = "tptinc", # Thresholds of pre-tax national income
    areas = "US", # United States
    perc = c("p10p100", "p50p100", "p90p100", "p99p100", "p99.9p100")
)

# Keep the value for 1970 in a separate data.frame
data1970 <- data %>% filter(year == 1970) %>%
    rename(value1970=value) %>%
    select(-year)

# Divide series by the reference year (1970)
data <- merge(data, data1970, by=c("country", "percentile")) %>%
    mutate(value=100*value/value1970)

ggplot(data) +
    geom_line(aes(x=year, y=value, color=percentile)) +
    ylab("1970 = 100") +
    scale_color_discrete(
        labels=c("p10p100"="P10", "p50p100"="P50", "p90p100"="P90",
            "p99p100"="P99", "p99.9p100"="P99.9")
    ) + 
    ggtitle("Divergence of pre-tax national income in the United States")

