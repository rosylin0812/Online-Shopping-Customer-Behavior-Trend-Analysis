library(tidyverse)

df <- read.csv("US Onine Superstore Data.csv", col_names = TRUE)
head(df, n=5)

# Removing column "Row ID", "Country", "Postal Code"
df_clean = df[,-c(1,9,12)]

# formatting column name by replacing the space or "-" with an underscore
names(df_clean) <- gsub(" ", "_", names(df_clean)) 
names(df_clean) <- gsub("-", "_", names(df_clean))

# Formatting data type order_date and ship_date to only display date w/out time
df_clean$Order_Date = as.Date(df_clean$Order_Date)
df_clean$Ship_Date = as.Date(df_clean$Ship_Date)

# assigning df_clean to Superstore_df variable to utilize with queries
Superstore_df = df_clean
Superstore_df



# Data Exploration (Identify any trends)
# Queries accounts for all 4 years


# Selling categories Information

top_category = Superstore_df

# ranking categories based on total Quantities
top_category %>%
  group_by(Category) %>%
  summarise(sum_quantity = sum(Quantity)) %>% # Total quantity for all 4 years
  arrange(desc(sum_quantity)) # Rank the results

# Ranking categories based on total profit
top_category %>%
  group_by(Category) %>%
  summarise(sum_profits = sum(Profit)) %>% # Total profit for all 4 years
  arrange(desc(sum_profits)) #rank the results 





# Selling Sub-Category based on Category

top_sub_category = Superstore_df

# Ranked selling Sub-Category and category based on total quantity
top_sub_category %>%
  group_by(Sub_Category, Category) %>% # group by the Category, then Subcategory
  summarise(sum_quantity = sum(Quantity)) %>% # Total quantity for all 4 years
  arrange(desc(Category), desc(sum_quantity)) 

# Ranked selling Sub-Category and category based on total Profits
top_sub_category %>%
  group_by(Sub_Category, Category) %>% # group by the Category, then Subcategory
  summarise(sum_profits = sum(Profit)) %>% # Total profit for all 4 years
  arrange(desc(Category), desc(sum_profits)) 




# Selling Products information

top_products = Superstore_df

# Top 5 selling products based on total quantity
top_products %>%
  group_by(Product_Name) %>% #group by product name
  summarise(sum_quantity = sum(Quantity)) %>% # Total quantity for all 4 years
  arrange(desc(sum_quantity)) %>%
  head(n=5)

# Top 5 selling products based on total Profits
top_products %>%
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for all 4 years and avg discount for product
  arrange(desc(sum_profit)) %>%
  head(n=5)

# Least 5 selling products based on total Quantity
top_products %>%
  group_by(Product_Name) %>% #group by product name
  summarise(sum_quantity = sum(Quantity)) %>% # Total quantity for all 4 years
  arrange(desc(sum_quantity)) %>%
  tail(n=5)

# Least 5 selling products based on total profits
top_products %>%
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for all 4 years and avg discount for product
  arrange(desc(sum_profit)) %>%
  tail(n=5)




# Yearly Trends

yearly_trend = Superstore_df

# Yearly trend selling products based on profits

# 2014 top selling products
yearly_trend %>%
  filter(Order_Date < "2015-01-01") %>% # look at all 2014 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2014 bottom selling products
yearly_trend %>%
  filter(Order_Date < "2015-01-01") %>% # look at all 2014 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  tail(n=5)

# 2015 top selling products
yearly_trend %>%
  filter(Order_Date > "2014-12-31", Order_Date < "2016-01-01") %>%
  # look at all 2015 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2015 bottom selling products
yearly_trend %>%
  filter(Order_Date > "2014-12-31", Order_Date < "2016-01-01") %>%
  # look at all 2015 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  tail(n=5)

# 2016 top selling products
yearly_trend %>%
  filter(Order_Date > "2015-12-31", Order_Date < "2017-01-01") %>%
  # look at all 2016 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2016 bottom selling products
yearly_trend %>%
  filter(Order_Date > "2015-12-31", Order_Date < "2017-01-01") %>%
  # look at all 2016 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  tail(n=5)

# 2017 top selling products
yearly_trend %>%
  filter(Order_Date > "2016-12-31", Order_Date < "2018-01-01") %>%
  # look at all 2017 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2017 bottom selling products
yearly_trend %>%
  filter(Order_Date > "2016-12-31", Order_Date < "2018-01-01") %>%
  # look at all 2017 data only
  group_by(Product_Name) %>% #group by product name
  summarise(sum_profit = sum(Profit),avg_discount = mean(Discount)) %>%
  # Total profit for single years and avg discount for product
  arrange(desc(sum_profit)) %>%
  tail(n=5)




# Yearly trend product category based on profits

# 2014 top products Category
yearly_trend %>%
  filter(Order_Date < "2015-01-01") %>% # look at all 2014 data only
  group_by(Category) %>% #group by category
  summarise(sum_profit = sum(Profit)) %>% # total profit for single year 
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2015 top products Category
yearly_trend %>%
  filter(Order_Date > "2014-12-31", Order_Date < "2016-01-01") %>%
  # look at all 2015 data only
  group_by(Category) %>% #group by category
  summarise(sum_profit = sum(Profit)) %>% # total profit for single year 
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2016 top products Category
yearly_trend %>%
  filter(Order_Date > "2015-12-31", Order_Date < "2017-01-01") %>%
  # look at all 2016 data only
  group_by(Category) %>% #group by category
  summarise(sum_profit = sum(Profit)) %>% # total profit for single year 
  arrange(desc(sum_profit)) %>%
  head(n=5)

# 2017 top products Category
yearly_trend %>%
  filter(Order_Date > "2016-12-31", Order_Date < "2018-01-01") %>%
  # look at all 2017 data only
  group_by(Category) %>% #group by category
  summarise(sum_profit = sum(Profit)) %>% # total profit for single year 
  arrange(desc(sum_profit)) %>%
  head(n=5)




# Profit/Loss by Discount Level

discount_level = Superstore_df

# group all discount levels together and find the avg profit for each level
# help determine at what discount level do we start to see a loss in sales
discount_level %>%
  group_by(Discount) %>%
  summarise(sum_profits = mean(Profit))
