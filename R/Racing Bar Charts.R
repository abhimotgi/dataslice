library(tidyquant)
library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)

tickers = c("FB", "AAPL", "MSFT", "GOOGL", "AMZN")

prices = tq_get(tickers, 
                from = "1980-01-01", 
                to = "2020-05-12", 
                get = "stock.prices")

fb = tq_get("FB",  
            from = "1980-01-01", 
            to = "2020-05-12", 
            get = "stock.prices")
fb = fb %>% 
  group_by(floor_date('month'))


# Get share volume data
get_shares = function(link) {
  # Scrape share volume given a link
  shares = read_html(link) %>%
    html_nodes("table") %>% 
    .[2] %>%
    html_table() %>%
    .[[1]]
  
  # Rename columns
  colnames(shares) = c("date", "shares")
  
  shares = shares %>%
    mutate(year = year(date), 
           quarter = quarter(date),
           shares = as.numeric(str_remove_all(shares, ","))) %>%
    select(year, quarter, shares)
  
  shares.q3 = shares %>%
    filter(quarter == '1', year == '2020') %>%
    select(shares) %>%
    .[1]
  
  shares = shares %>%
    rbind(data.frame(quarter = 2, year = 2020, shares = shares.q3)) 
  
  return(shares)
}

msft.shares = get_shares("https://www.macrotrends.net/stocks/charts/MSFT/microsoft/shares-outstanding")
fb.shares = get_shares("https://www.macrotrends.net/stocks/charts/FB/facebook/shares-outstanding")
googl.shares = get_shares("https://www.macrotrends.net/stocks/charts/GOOGL/alphabet/shares-outstanding")
aapl.shares = get_shares("https://www.macrotrends.net/stocks/charts/AAPL/apple/shares-outstanding")
amzn.shares = get_shares("https://www.macrotrends.net/stocks/charts/AMZN/amazon/shares-outstanding")


# Getting stock price data

# Function to fetch stock price data, 
# join with shares data, and calculate market cap

get_mkt_cap = function(ticker, shares_df) {
  mkt_cap = tq_get(ticker, 
                  from = "2005-01-01", 
                  to = today(), 
                  get = "stock.prices") %>%
    select(date, price = adjusted) %>%
    mutate(quarter = quarter(date), year = year(date)) %>%
    left_join(shares_df, by=c("year", "quarter")) %>%
    mutate(market_cap = shares * price) %>% 
    select(date, market_cap) %>%
    drop_na(market_cap)
  
  return(mkt_cap)
}

fb.mkt_cap = get_mkt_cap("FB", fb.shares) %>% 
  rename(fb.cap = market_cap)
amzn.mkt_cap = get_mkt_cap("AMZN", amzn.shares) %>% 
  rename(amzn.cap = market_cap)
msft.mkt_cap = get_mkt_cap("MSFT", msft.shares) %>% 
  rename(msft.cap = market_cap)
googl.mkt_cap = get_mkt_cap("GOOGL", googl.shares) %>% 
  rename(googl.cap = market_cap)
aapl.mkt_cap = get_mkt_cap("AAPL", aapl.shares) %>%
  rename(aapl.cap = market_cap)


# Graph to confirm values are correct
market_caps.plot = fb.mkt_cap %>%
  full_join(amzn.mkt_cap, by = "date") %>%
  full_join(msft.mkt_cap, by = "date") %>%
  full_join(googl.mkt_cap, by = "date") %>%
  full_join(aapl.mkt_cap, by = "date") %>%
  melt(id = "date") %>%
  ggplot(aes(x = date, y = value, color = variable)) + geom_line()

# Join all market caps into one dataframe 
# and reshape so dates are on x-axis
market_caps = fb.mkt_cap %>%
  full_join(amzn.mkt_cap, by = "date") %>%
  full_join(msft.mkt_cap, by = "date") %>%
  full_join(googl.mkt_cap, by = "date") %>%
  full_join(aapl.mkt_cap, by = "date") %>%
  arrange(date) %>%
  mutate(date.f = paste0(year(date), " Q", quarter(date))) %>%
  # mutate(date.f = floor_date(date, unit = "month")) %>%
  group_by(date.f) %>%
  summarise(amzn.mkt_cap = mean(amzn.cap),
            msft.mkt_cap = mean(msft.cap),
            googl.mkt_cap = mean(googl.cap),
            aapl.mkt_cap = mean(aapl.cap),
            fb.mkt_cap = mean(fb.cap)) %>%
  # summarise(amzn.mkt_cap = mean(amzn.mkt_cap)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column()

write_csv(market_caps, "market_caps.csv")

# https://public.flourish.studio/visualisation/2419690/