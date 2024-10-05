install.packages("tidyverse")
library(tidyverse)
file_list <- list.files(path = "E:/Sreyesh/F/Finance Yahoo", pattern = "*.csv", full.names = TRUE)
all_data <- file_list %>%
  map_dfr(~ read_csv(.x) %>%
            mutate(Stock_Symbol = basename(.x) %>% str_remove(".csv")))
head(all_data)
glimpse(all_data)
colSums(is.na(all_data))
all_data <- all_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
summary_stats <- all_data %>%
  group_by(Stock_Symbol) %>%
  summarise(
    mean_close = mean(Close, na.rm = TRUE),
    median_close = median(Close, na.rm = TRUE),
    sd_close = sd(Close, na.rm = TRUE),
    mean_volume = mean(Volume, na.rm = TRUE)
  )
head(summary_stats)
library(ggplot2)
ggplot(all_data %>% filter(Stock_Symbol %in% c("AAPL", "MSFT","GOOGL")), 
       aes(x = Date, y = Close, color = Stock_Symbol)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Stock Prices Over Time", x = "Date", y = "Closing Price")
pivot_data <- all_data %>%
  select(Date, Stock_Symbol, Close) %>%
  pivot_wider(names_from = Stock_Symbol, values_from = Close)
cor_matrix <- cor(pivot_data %>% select(-Date), use = "complete.obs")
print(cor_matrix)
install.packages("forecast")
library(forecast)
library(dplyr)
#stock prediction for Google
stock_ts <- ts(all_data %>% filter(Stock_Symbol == "AAPL") %>% pull(Close), frequency = 365)
fit <- auto.arima(stock_ts)
forecast(fit, h = 30) %>% autoplot()
forecast_data <- forecast(fit, h = 30)
autoplot(forecast_data) +
  geom_line(aes(x = 1:length(forecast_data$mean),y = forecast_data$mean), color = "blue", linewidth = 1.5, linetype = "solid") + 
  theme_minimal() +
  labs(title = "Stock Price Forecast from ARIMA", x = "Time", y = "Stock Price") +
  theme(legend.position = "top") 
# Calculate the first and last closing prices for each stock
stock_summary <- all_data %>%
  group_by(Stock_Symbol) %>%
  summarise(
    first_close = first(Close, order_by = Date),  # First available closing price
    latest_close = last(Close, order_by = Date),  # Most recent closing price
    growth = (latest_close - first_close) / first_close * 100,  # Percentage growth
    latest_date = max(Date)  # Most recent date for each stock
  )
head(stock_summary)
# Identify top 5 and bottom 5 stocks by growth
top_5_growth <- stock_summary %>%
  arrange(desc(growth)) %>%
  slice(1:5)

bottom_5_growth <- stock_summary %>%
  arrange(growth) %>%
  slice(1:5)

top_bottom_stocks <- bind_rows(
  top_5_growth %>% mutate(type = "Top 5 Growth"),
  bottom_5_growth %>% mutate(type = "Bottom 5 Growth"),
  top_5_price %>% mutate(type = "Top 5 Price"),
  bottom_5_price %>% mutate(type = "Bottom 5 Price")
)
ggplot(top_5_growth, aes(x = reorder(Stock_Symbol, growth), y = growth)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Top 5 Stocks by Highest Growth", x = "Stock Symbol", y = "Growth (%)") +
  theme_minimal()

ggplot(bottom_5_growth, aes(x = reorder(Stock_Symbol, growth), y = growth)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Bottom 5 Stocks by Lowest Growth", x = "Stock Symbol", y = "Growth (%)") +
  theme_minimal()

stock_summary <- all_data %>%
  group_by(Stock_Symbol) %>%
  summarise(
    first_close = first(Close, order_by = Date),  # First available closing price
    latest_close = last(Close, order_by = Date),  # Most recent closing price
    growth = (latest_close - first_close) / first_close * 100,  # Percentage growth
    latest_date = max(Date)  # Most recent date for each stock
  )
top_5_latest_price <- stock_summary %>%
  arrange(desc(latest_close)) %>%
  slice(1:5)

bottom_5_latest_price <- stock_summary %>%
  arrange(latest_close) %>%
  slice(1:5)

ggplot(top_5_latest_price, aes(x = reorder(Stock_Symbol, latest_close), y = latest_close)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 5 Stocks by Highest Latest Price", x = "Stock Symbol", y = "Closing Price") +
  theme_minimal()

ggplot(bottom_5_latest_price, aes(x = reorder(Stock_Symbol, latest_close), y = latest_close)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Bottom 5 Stocks by Lowest Latest Price", x = "Stock Symbol", y = "Closing Price") +
  theme_minimal()
install.packages("e1071")
library(e1071)
stock_metrics <- all_data %>%
  group_by(Stock_Symbol) %>%
  summarise(
    skewness = skewness(Close, na.rm = TRUE),
    kurtosis = kurtosis(Close, na.rm = TRUE),
    .groups = 'drop'
  )
head(stock_metrics)
ggplot(stock_metrics, aes(x = Stock_Symbol, y = skewness)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Skewness of Closing Prices by Stock", x = "Stock Symbol", y = "Skewness")
ggplot(stock_metrics, aes(x = Stock_Symbol, y = kurtosis)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Kurtosis of Closing Prices by Stock", x = "Stock Symbol", y = "Kurtosis")
top_5_kurtosis <- stock_metrics %>%
  arrange(desc(kurtosis)) %>%
  slice(1:5)
print("Top 5 Stocks by Kurtosis:")
print(top_5_kurtosis)
top_5_skewness <- stock_metrics %>%
  arrange(desc(skewness)) %>%
  slice(1:5)
print(top_5_skewness)

summary_stats <- all_data %>%
  group_by(Stock_Symbol) %>%
  summarise(
    mean_close = mean(Close, na.rm = TRUE),
    median_close = median(Close, na.rm = TRUE),
    sd_close = sd(Close, na.rm = TRUE),
    mean_volume = mean(Volume, na.rm = TRUE)
  )
all_data <- all_data %>%
  group_by(Stock_Symbol) %>%
  arrange(Date) %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close) * 100)
head(all_data)
head(summary_stats)
dim(all_data)
#all_data_filtered <- all_data %>%
 # filter(Stock_Symbol %in% c("AAPL", "MSFT", "GOOGL") & Date >= "2020-01-01")
#volatility_data <- all_data_filtered %>%
 # group_by(Stock_Symbol) %>%
  #summarise(volatility = sd(Daily_Return, na.rm = TRUE))
#Due to large memory access and exceeding time limit for character search
#this function is not functional.

library(zoo)

all_data <- all_data %>%
  group_by(Stock_Symbol) %>%
  arrange(Date) %>%
  mutate(Rolling_Avg_30 = rollmean(Close, k = 30, fill = NA, align = "right"),
         Rolling_Volatility_30 = rollapply(Daily_Return, width = 30, FUN = sd, fill = NA, align = "right"))

# Check the head of the modified data
head(all_data)
ggplot(all_data %>% filter(Stock_Symbol %in% c("AAPL", "MSFT", "GOOGL","HON")), 
       aes(x = Date, y = Rolling_Avg_30, color = Stock_Symbol)) +
  geom_line() +
  theme_minimal() +
  labs(title = "30-Day Rolling Average of Stock Prices", x = "Date", y = "30-Day Average Closing Price")
ggplot(all_data %>% filter(Stock_Symbol %in% c("AAPL", "MSFT", "GOOGL","HON")), 
       aes(x = Date, y = Rolling_Volatility_30, color = Stock_Symbol)) +
  geom_line() +
  theme_minimal() +
  labs(title = "30-Day Rolling Volatility of Stock Returns", x = "Date", y = "30-Day Volatility")

# Create pivot data for daily returns
pivot_returns <- all_data %>%
  select(Date, Stock_Symbol, Daily_Return) %>%
  pivot_wider(names_from = Stock_Symbol, values_from = Daily_Return)

# Calculate the correlation matrix of daily returns
cor_returns_matrix <- cor(pivot_returns %>% select(-Date), use = "complete.obs")
print(cor_returns_matrix)
install.packages("ggcorrplot")

# Convert the correlation matrix to a long format (ignoring diagonal values)
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%  # Exclude the diagonal (self correlations)
  mutate(pair = pmap_chr(list(Var1, Var2), ~paste(sort(c(..1, ..2)), collapse = "-"))) %>%
  distinct(pair, .keep_all = TRUE) %>%  # Keep unique pairs
  arrange(desc(Freq))  # Sort by correlation values

# Extract top 5 and bottom 5 correlations
top_5_correlations <- cor_long %>%
  slice(1:5)

bottom_5_correlations <- cor_long %>%
  arrange(Freq) %>%
  slice(1:5)

# Combine top and bottom correlations into one dataset
top_bottom_correlations <- bind_rows(
  top_5_correlations %>% mutate(type = "Top 5 Correlations"),
  bottom_5_correlations %>% mutate(type = "Bottom 5 Correlations")
)

# Check the data
print(top_bottom_correlations)

