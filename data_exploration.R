# R for Data Exploration
# www.dani-sola.com/r-for-data-exploration/

# Loading and preparation of data
crimes <- read.csv("boro.csv.gz", stringsAsFactors=FALSE)
str(crimes)

library(dplyr) # install.packages("dplyr")
crimes %>% top_n(5)

names(crimes) <- c("date", "type", "specific_type", "borough", "num")
crimes$date <- as.Date(paste(crimes$date, "01", sep=""), "%Y%m%d")
summary(crimes)

# Question: how many crimes of each type Westminster had in November 2014?
westminster.2014.11 <- crimes %>%
  filter(borough == "Westminster" & date == "2014-11-01") %>%
  select(borough, type, num)

westminster.2014.11 <- westminster.2014.11 %>% 
  group_by(borough, type) %>%
  summarise(num=sum(num)) %>%
  arrange(desc(num))

# Question: how each type of crime has evolved over time in Westminster?
westminster <- crimes %>%
  filter(borough == "Westminster") %>%
  group_by(date, type) %>%
  summarise(num = sum(num))

# Question: can we visualise how each type of crime has evolved over time in Westminster?
library(lattice) # install.packages("lattice")
xyplot(num ~ date, westminster)
xyplot(num ~ date, westminster,
       groups = type, 
       auto.key=list(space="right"),
       t="l")
xyplot(num ~ date | type, westminster, t="l")

# What is the distribution of the number of drug-related crimes?
histogram(~ num, westminster %>% filter(type == "Drugs"), breaks = 6)

# Question: how does crime numbers have evolved over time in Westminster, Camden and Islington?
crime.boroughs <- crimes %>%
  filter(borough %in% c("Westminster", "Camden", "Islington")) %>%
  group_by(date, borough) %>%
  summarise(num = sum(num))

xyplot(num ~ date, crime.boroughs, groups=borough, auto.key=list(columns=3), t="l")

# Question: what are the normal ranges of crime in each borough?
bwplot(num ~ borough, crime.boroughs)

# Question: how does crime numbers change depending on the month?
crime.by.date <- crimes %>%
  filter(borough %in% c("Westminster", "Camden", "Islington")) %>%
  group_by(borough, date) %>%
  summarise(num = sum(num))

crime.by.month <- crime.by.date %>%
  mutate(month = as.numeric(format(date, "%m"))) %>%
  group_by(borough, month) %>%
  summarise(num = mean(num))

barchart(num ~ month, crime.by.month, groups = borough,
         horizontal=FALSE, 
         auto.key=list(columns=3),
         ylim = 0:5500)

# That's all folks!