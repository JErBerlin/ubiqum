# DPLYR Workshop

# In the following exercises, we will use these six functions:
# 1. filter() -> pick observations
# 2. arrange() -> reorder rows
# 3. select() -> pick columns
# 4. mutate() -> add new variables as functions of others
# 5. summarise() -> collapse many values to a summary
# 6. group by() -> group observations

# Loading libraries ####
library(downloader) # to get the data
library(dplyr)

# Reading data ####
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")

# Exercises ####

# 1. Select the "name" column and all columns starting with "sl"
s1 <- select(msleep, name, starts_with("sl"))
s1 <- msleep %>% select(name, starts_with("sl"))

# 2. Let's take a look at the average sleep time of the animals according to their eating habits.
# Create a new datafame with two columns: "vore" and "mean_sleep"
msleepByVore <- msleep %>% group_by(vore) %>% summarise(mean_sleep = mean(sleep_total)) %>% arrange(mean_sleep)

# 3. Build again the same df, but this time we want to exclude animals that sleep less than 2 hours or more than 19
msleepByVore_2to19h <- msleep %>% filter(sleep_total >= 2.0) %>% filter(sleep_total <= 19.0)  %>% group_by(vore) %>% summarise(mean_sleep = mean(sleep_total)) %>% arrange(mean_sleep) 

# 4. Same df as before, but don't want domesticated animals in our table
msleepByVore_2to19h_wild <- 
  msleep %>% 
  filter(conservation != 'domesticated') %>% 
  filter(sleep_total >= 2.0) %>% 
  filter(sleep_total <= 19.0)  %>% 
  group_by(vore) %>% 
  summarise(mean_sleep = mean(sleep_total)) %>% 
  arrange(mean_sleep) 

# 5. Now, exclude NAs from your df
msleepByVore_2to19h_wild_NNA <- 
  msleep %>% 
  filter(!is.na()) %>% 
  filter(conservation != 'domesticated') %>% 
  filter(sleep_total >= 2.0, sleep_total <= 19.0)  %>% 
  group_by(vore) %>% 
  summarise(mean_sleep = mean(sleep_total)) %>% 
  arrange(mean_sleep) 

# 6. Add a column to your df with their brain-to-body mass ratio

# 7. Add a column to your dataframe with the count of animals for each row

# 8. Order your df by the count column in descending order
