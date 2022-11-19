####################################
## loading necessary packages
####################################
library("readr")
library("dplyr")
library("data.table")
library('tidyr')

####################################
## loading datasets
####################################
base_url <- "https://static.usafacts.org/public/data/covid-19"
deaths_url <- paste(base_url, "covid_deaths_usafacts.csv", sep = "/")
cases_url <- paste(base_url, "covid_confirmed_usafacts.csv", sep = "/")
population_url <- paste(base_url, "covid_county_population_usafacts.csv", sep = "/")

# options are "read.csv", "read_csv" and "fread"
#fread like read.table but faster
csv_loader <- fread

deaths <- csv_loader(deaths_url)
cases <- csv_loader(cases_url)
population <- csv_loader(population_url)

####################################
## data cleaning
####################################

## clean column names
## they all look correct

## change all column names to lower case
names(cases) <- tolower(names(cases))
names(deaths) <- tolower(names(deaths))
names(population) <- tolower(names(population))

## change date column to format "year-month-day"
## not doing because format looks correct

## Collect the date column names of both deaths and cases data frames
##and save them into the variables death_dates and case_dates.
death_dates <- deaths %>% select(starts_with('2')) %>% names()
case_dates <- cases %>% select(starts_with('2')) %>% names()

## Aggregate the deaths, cases and population from each county to their state
## using sum, and re-assign them to deaths, cases and population
deaths<-deaths %>% group_by(state) %>% summarize_at(death_dates, sum)
cases<-cases %>% group_by(state) %>% summarize_at(case_dates, sum)
population<-population %>% group_by(state) %>% summarize_at('population', sum)

## Reshape the deaths and cases so that each row is from a single state and a single date. 
## The date column should be named "date" 
## and the count column should be named "deaths" for deaths and "cases" for cases.
deaths_pivoted <- deaths %>% pivot_longer(
  cols = all_of(death_dates), names_to="date", values_to="deaths"
  )

cases_pivoted <- cases %>% pivot_longer(
  cols = all_of(case_dates), names_to="date", values_to="cases"
)

## Merge deaths, cases and population into one data frame called counts. 
## deaths and cases will be merged by matchingstate and date 
## and the population should be merged by matching state.
merged_all <- deaths_pivoted %>% 
  full_join(cases_pivoted, by =c("state","date")) %>% 
  full_join(population, by="state")

####################################
## data validation and processing 
####################################

## Order the data by state and date.
merged_all<-arrange(merged_all,"state","date")

## Compute the new deaths and cases for each state, 
## by using the current day's count minus the yesterday's count
## Column name for new deaths should be "new_deaths" 
## and the column name for new cases should be "new_cases".

merged_all_new <- merged_all%>% group_by(state) %>% 
  mutate(new_deaths = deaths-lag(deaths)) %>% 
  mutate(new_cases = cases-lag(cases))

##creates NA for first value of state due to lagging
##replacing NA with zero just in case
merged_all_new <- merged_all_new %>% 
  mutate_at(c('new_deaths','new_cases'), ~replace_na(.,0))

## for new deaths or new cases Replace the negative value by 0
##replacing negative numbers with zero
merged_all_new <- merged_all_new %>% 
  mutate_at(c('new_deaths','new_cases'), ~ ifelse(.x < 0, 0, .x))

## and then re-compute the deaths and cases as the cumulative sum of the new deaths and new cases
merged_all_new <- merged_all_new %>% group_by(state) %>% 
  mutate(cumsum_deaths = cumsum(new_deaths)) %>% 
  mutate(cumsum_cases = cumsum(new_cases))

## Compute the infection fatality rate as the ratio between the deaths and cases. 
##The column name should be ifr.
merged_all_new <- merged_all_new %>% group_by(state) %>% 
  mutate(ifr=cumsum_deaths/cumsum_cases)

#putting in a zero for NaN for clarity
merged_all_new <- merged_all_new %>%  mutate_at(c('ifr'),~replace(., is.nan(.), 0))

## Compute the mortality rate as the ratio between the deaths and population. 
## The column name should be mr.
merged_all_new <- merged_all_new %>% group_by(state) %>% 
  mutate(mr=deaths/population)

####################################
## data analysis and results
####################################
 agg_states <- merged_all_new %>%
  group_by(state) %>%
  summarise(
    mean_deaths = mean(new_deaths),
    mean_cases = mean(new_cases),
    mean_ifr=mean(ifr),
    mean_mr=mean(mr)
  )

## List the top five states with the most deaths, cases.
## top 5 states by highest mean deaths
agg_states %>% arrange(desc(mean_deaths)) %>% slice (1:5)

##top 5 states by highest mean cases
agg_states %>% arrange(desc(mean_cases)) %>% slice (1:5)

## List the top five states with the highest infection fatality rate and mortality rate.

##top 5 states by highest mean ifr
agg_states %>% arrange(desc(mean_ifr)) %>% slice (1:5)

##top 5 states by highest mean mr
agg_states %>% arrange(desc(mean_mr)) %>% slice (1:5)

