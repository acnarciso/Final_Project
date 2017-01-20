library(tidyverse)
library(stringr)
library(broom)
library(dplyr)
library(ggplot2)
library(maptools)
library(readr)
library(lubridate)
library(Quandl)
library(tm)
library(knitr)

PQI_county <- read_csv("PQI_County_2009.csv")
AQI_2014 <- read_csv("daily_summary_2014_ozone.csv")
NY_all_data_2014 <- read_csv("ACS_2014_all_data.csv")

# SIDE NOTE: PQI

important_PQIs <- PQI_county %>% 
  filter(`Patient County` != "STATEWIDE", `Year`== 2014, 
         `PQI Number` == c("PQI_05", "PQI_08", "PQI_11", "PQI_12", "PQI_10", "PQI_03",
                           "PQI_01", "PQI_15", "PQI_07", "PQI_13", "PQI_16", "PQI_14")) %>%
  group_by(`PQI Name`) %>% 
  summarise(`Rate Per 100,000` = mean(`Observed Rate Per 100,000 People`)) %>% 
  arrange(desc(`Rate Per 100,000`)) %>% 
  mutate(`Rate Per 100,000` = round(`Rate Per 100,000`, 0)) %>% 
  head(5)
important_PQIs

important_PQIs_plot <- important_PQIs %>% 
  ggplot(data = ., aes(x = reorder(`PQI Name`, -`Rate Per 100,000`), 
                       y = `Rate Per 100,000`)) +
  geom_bar(stat = "identity") + 
  labs(x = "Disease")
important_PQIs_plot

# AIR QUALITY (AQI)

NY_AQI_2014 <- AQI_2014 %>% 
  filter(`State Name` == "New York") %>%
  mutate(year = year(`Date Local`),
         county = str_to_title(`County Name`)) %>% 
  rename(county_code = `County Code`, date = `Date Local`) %>% 
  select(county_code, AQI, year, date, county) 

# AQI in NY state (fixed repeated values BY finding the avg AQI /day/county)
NY_AQI_2014_fixed <- NY_AQI_2014 %>% 
  group_by(year, county, county_code, date) %>% 
  summarise(AQI_fixed = mean(AQI))

# AQI - Avg Year (WORKING SET)
NY_AQIavg_2014 <- NY_AQI_2014_fixed %>% 
  group_by(year, county, county_code) %>% 
  summarise(avg_year_AQI = mean(AQI_fixed))

# ASTHMA

# Asthma (young and older adults) Rates by County (2011-2014)
asthma_county <- PQI_county %>% 
  mutate(asthma = ifelse(`PQI Name` == "Asthma in Younger Adults", "asthma_under18",
                         ifelse(`PQI Name` == 
                                  "Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults", 
                                "asthma_COPD_over18", "0")),
         county = str_to_title(`Patient County`)) %>% 
  rename(rate_per_100000 = `Observed Rate Per 100,000 People`, year = `Year`) %>% 
  select(year, rate_per_100000, asthma, county) %>% 
  filter(asthma != "0", county != "Statewide")

# Asthma (young and older adults) Rates by County in 2014 
asthma_county_2014 <- asthma_county %>% 
  filter(year == 2014) %>% 
  select(-year) 

# DEMOGRAPHIC DATA (income, health insurance)

# Cut down to only county data
NY_countydata_2014 <- NY_all_data_2014 %>% 
  filter(between(GEO.id2, 36000, 37000)) %>% 
  mutate(`GEO.display-label` = removeWords(`GEO.display-label`, " County, New York")) %>% 
  rename(county = `GEO.display-label`)
# note to self: missing counties - only 39/62

# Demographic data (WORKING SET)
NY_demo_data_2014 <- NY_countydata_2014 %>% 
  rename(per_capita_income = `HC01_VC118`, mean_household_income = `HC01_VC86`,
         median_household_income = `HC01_VC85`, percent_below_poverty = `HC03_VC161`,
         w_ins = `HC03_VC131`, private_ins = `HC03_VC132`, 
         public_ins = `HC03_VC133`, no_ins = `HC03_VC134`) %>% 
  select(county, per_capita_income, mean_household_income, 
         median_household_income, percent_below_poverty,  
         w_ins, private_ins, public_ins, no_ins) %>% 
  mutate(per_capita_income_K = round(per_capita_income / 1000, 0))

# COMBINING DATA BY COUNTY - ASTHMA RATES + AIR QUALITY, DEMOGRAPHICS

# Part 1: Combine Demographics and Air Quality
demo_AQI_2014 <- inner_join(NY_demo_data_2014, NY_AQIavg_2014, by = "county")
# Part 2: Combine Demographics & Air Quality w/ Asthma Rates
combined_data <- left_join(demo_AQI_2014, asthma_county_2014, by = "county")

# Graph Representation of Asthma vs. AQI
asthma_AQI_plot_2014 <- combined_data %>% 
  ggplot(data = ., aes(x = avg_year_AQI, y = rate_per_100000,
                       col = per_capita_income_K, size = no_ins)) + 
  facet_wrap(~asthma, scales = "free") + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text(aes(label = ifelse(county %in% c("Bronx", "Queens", "New York"),
                               as.character(county), "")),hjust=-0.2,vjust=0.2) +
  labs(x = "Average Air Quality Index (AQI) in 2014", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)")
asthma_AQI_plot_2014

# Graph Representation of Asthma vs. AQI: NO OUTLIERS
asthma_AQI_plot_2014_2 <- combined_data %>% 
  filter(!(county %in% c("Bronx"))) %>%
  ggplot(data = ., aes(x = avg_year_AQI, y = rate_per_100000)) +
  facet_wrap(~asthma, scales = "free") + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Average Air Quality Index (AQI) in 2014", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)") 
asthma_AQI_plot_2014_2

# Graph Representation of Asthma vs. Per Capita Income
per_capita_income_asthma_plot <- ggplot(data = combined_data, 
                               aes(x = per_capita_income_K, y = rate_per_100000,
                                   col = no_ins)) +
  facet_wrap(~asthma, scales = "free") + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = ifelse(no_ins > 10 | county == "New York",
                               as.character(county), "")),hjust=0.5,vjust=1.2) +
  labs(x = "Income Per Capita (Thousand USD)", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)") + 
  coord_cartesian(xlim=c(15,70))
per_capita_income_asthma_plot

# Graph Representation of Asthma vs. Per Capita Income - NO OUTLIERS
per_capita_income_asthma_plot_2 <- combined_data %>% 
  filter(!(county %in% c("Bronx"))) %>%
  ggplot(data = ., aes(x = per_capita_income_K, y = rate_per_100000)) +
  facet_wrap(~asthma, scales = "free") + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = ifelse(per_capita_income_K > 60,
                               as.character(county), "")),hjust=1,vjust=1.2) +
  labs(x = "Income Per Capita (Thousand USD)", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)")
per_capita_income_asthma_plot_2

# Graph Representation of Asthma vs. No Insurance 
no_ins_asthma_plot <- ggplot(data = combined_data, 
                             aes(x = no_ins,y = rate_per_100000))+
  facet_wrap(~asthma, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = ifelse(no_ins >12,
                               as.character(county), ""),hjust=0.8,vjust=1.2)) +
  labs(x = "% Without Health Insurance", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)") + 
  coord_cartesian(xlim=c(3,15))
no_ins_asthma_plot

# Graph Representation of Asthma vs. No Insurance - NO OUTLIERS (GOOD)
no_ins_asthma_plot2 <- combined_data %>% 
  filter(!(county %in% c("Bronx"))) %>% 
  ggplot(data = ., aes(x = no_ins,y = rate_per_100000)) +
  facet_wrap(~asthma, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = ifelse(no_ins > 12,
                               as.character(county), ""),hjust=0.8,vjust=1.2)) +
  labs(x = "% Without Health Insurance", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)") 
no_ins_asthma_plot2

# Graph Representation of Asthma vs. WITH insurance (BAD)
w_ins_asthma_plot2 <- ggplot(data = combined_data, 
                             aes(x = w_ins,y = rate_per_100000,
                                 col = avg_year_AQI, size = per_capita_income_K)) +
  facet_wrap(~asthma, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = ifelse(w_ins < 90 | avg_year_AQI > 37.5 | per_capita_income_K > 50,
                               as.character(county), ""),hjust=0.5,vjust=1.2)) +
  labs(x = "% With Health Insurance", 
       y = "Rate of Asthma (Hospital Admissions per 100,000)") 
w_ins_asthma_plot2

# Graph Representation of Per Capita Income, Asthma, Age, AQI 
combined_plot <- ggplot(data = combined_data, 
                             aes(x = per_capita_income_K, y = rate_per_100000,
                                 col = avg_year_AQI)) +
  facet_wrap(~asthma, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_text(aes(label = as.character(county),hjust=-0.1,vjust=0))
combined_plot

# SIDE NOTE: INVESTIGATING THE BRONX

important_PQIs_Bronx <- PQI_county %>% 
  filter(`Patient County` != "STATEWIDE", `Year` == 2014,
         `PQI Number` == "PQI_05") %>%
  group_by(`PQI Name`, `Patient County`) %>% 
  summarise(`Rate Per 100,000` = mean(`Observed Rate Per 100,000 People`)) %>% 
  arrange(desc(`Rate Per 100,000`)) %>% 
  mutate(`Rate Per 100,000` = round(`Rate Per 100,000`, 0)) 
important_PQIs_Bronx

# LINEAR MODEL

combined_data2 <- combined_data %>% 
  spread(asthma, rate_per_100000)

# LM: Over 18
lm_over18 <- lm(asthma_COPD_over18 ~ avg_year_AQI + per_capita_income_K + no_ins,
                data = combined_data2)
lm_over18_table <- tidy(lm_over18, conf.int = TRUE) %>% 
  select(term, estimate, p.value, conf.low, conf.high) %>% 
  mutate(estimate = round(estimate, 1),
         p.value = round(p.value, 2),
         conf.low = round(conf.low, 1),
         conf.high = round(conf.high, 1)) %>% 
  kable()
lm_over18_table

# LM: Under 18
lm_under18 <- lm(asthma_under18 ~ avg_year_AQI + per_capita_income_K + no_ins,
                data = combined_data2)

lm_under18_table <- tidy(lm_under18, conf.int = TRUE) %>% 
  select(term, estimate, p.value, conf.low, conf.high) %>% 
  mutate(estimate = round(estimate, 1),
         p.value = round(p.value, 2),
         conf.low = round(conf.low, 1),
         conf.high = round(conf.high, 1)) %>% 
  kable()
lm_under18_table

# LINEAR MODEL W/O OUTLIERS

# PROBLEM: SHOULD YOU REMOVE OUTLIERS IN LMs?????

combined_data_no_outliers <- combined_data2 %>% 
  filter(county != "Bronx")

# LM: Over 18
lm_over18_out <- lm(asthma_COPD_over18 ~ avg_year_AQI + per_capita_income_K + no_ins,
                data = combined_data_no_outliers)
lm_over18_out_table <- tidy(lm_over18_out, conf.int = TRUE) %>% 
  select(term, estimate, p.value, conf.low, conf.high) %>% 
  mutate(estimate = round(estimate, 1),
         p.value = round(p.value, 2),
         conf.low = round(conf.low, 1),
         conf.high = round(conf.high, 1)) %>% 
  kable()
lm_over18_out_table

# LM: Under 18
lm_under18_out <- lm(asthma_under18 ~ avg_year_AQI + per_capita_income_K + no_ins,
                 data = combined_data_no_outliers)
lm_under18_out_table <- tidy(lm_under18_out, conf.int = TRUE) %>% 
  select(term, estimate, p.value, conf.low, conf.high) %>% 
  mutate(estimate = round(estimate, 1),
         p.value = round(p.value, 2),
         conf.low = round(conf.low, 1),
         conf.high = round(conf.high, 1)) %>% 
  kable()
lm_under18_out_table

