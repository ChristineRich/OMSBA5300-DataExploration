#Load libraries
library(tidyverse)
library(purrr)
library(vtable)
library(ggplot2)
library(jtools)
library(car)

#Read trend files
trend_files <- list.files(path="../Raw Data", pattern="trends_up_to", full.names=TRUE)
kw_trends <- trend_files %>% 
  map_dfr(read_csv, .id="source") %>% 
  #Remove rows without a schid or a schname or an index
  filter(!is.na(schid) & !is.na(schname) & !is.na(index)) %>% 
  #Divide month or week into start and end dates
  mutate(start_date=as.Date(str_sub(monthorweek,1,10))) %>%
  mutate(end_date=as.Date(str_sub(monthorweek,14,23)))

vtable(kw_trends)

#Remove rows not in the year before scoreboard released or in year after release
kw_trends <- kw_trends %>% filter(start_date >= as.Date("2014-09-01") & end_date <= as.Date("2016-08-31"))

#Read data dictionary
data_dict <- read_csv("../Raw Data/CollegeScorecardDataDictionary-09-08-2015.csv")

#Read id/name link
id_name <- read_csv("../Raw Data/id_name_link.csv")

#Read most recent cohorts
cohorts <- read_csv("../Raw Data/Most+Recent+Cohorts+(Scorecard+Elements).csv")

#Find duplicate school names
dups <- id_name %>% group_by(schname) %>% summarise(name_count = n()) %>% filter(name_count > 1)

#Get unit ID's of duplicates
dup_unitid <- id_name %>% filter(schname %in% dups$schname)

#Remove duplicates
kw_trends <- kw_trends %>% filter(!(schname %in% dups$schname))
cohorts <- cohorts %>% filter(!(UNITID %in% dup_unitid$unitid))

#Summarize data frame
vtable(cohorts)

#Filter to bachelor degree predominant colleges
cohorts <- cohorts %>% filter(PREDDEG == 3)

#Standardize keyword index scores
keywords_means <- kw_trends %>% group_by(keyword, schname) %>% summarise(mean = mean(index), sd=sd(index))
kw_trends_new <- left_join(kw_trends, keywords_means, by=c("keyword"="keyword", "schname"="schname"))
trends_standardized <- kw_trends_new %>% mutate(index_standardized = (index - mean)/sd) %>% 
  select(schname, keyword, index_standardized, start_date, end_date)

trends_standardized <- trends_standardized %>% 
  #Filter to the year after Scorecard released
  filter(start_date >= as.Date("2015-09-01") & end_date <= as.Date("2016-08-31"))

#Group data to be avg index/college for year after Scorecard released
school_index_scores <- trends_standardized %>% group_by(schname) %>% summarise(avg_index=mean(index_standardized))

#Join cohorts to index scores data frame
master_df <- inner_join(school_index_scores, id_name, by=c("schname" = "schname"))
master_df <- inner_join(master_df, cohorts, by=c("unitid"="UNITID", "opeid"="OPEID"))

model_df <- master_df %>% select(schname, avg_index, `md_earn_wne_p10-REPORTED-EARNINGS`, LOCALE, CONTROL, GRAD_DEBT_MDN_SUPP) %>%
  mutate(LOCALE=as.integer(LOCALE)) %>%
  mutate(GRAD_DEBT_MDN_SUPP=as.integer(GRAD_DEBT_MDN_SUPP)) %>% filter(!is.na(GRAD_DEBT_MDN_SUPP))

#Create locale and control dummy variables
locale_id <- c(11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43)
locale_type <- c("City", "City", "City", "Suburb", "Suburb", "Suburb", "Town", "Town", "Town", "Rural", "Rural", "Rural")
locale_df <- data.frame(locale_id, locale_type)
model_df <- inner_join(model_df, locale_df, by=c("LOCALE"="locale_id")) 
model_df <- model_df %>%  
  mutate(city = locale_type=="City") %>%
  mutate(suburb=locale_type=="Suburb") %>%
  mutate(town=locale_type=="Town") %>%
  mutate(rural=locale_type=="Rural")
control_df <- data.frame(control_id=c(1,2,3), control_name=c("Public", "Private (Nonprofit)", "Private (For-Profit)"))
model_df <- inner_join(model_df, control_df, by=c("CONTROL"="control_id"))
model_df <- model_df %>%
  mutate(public = control_name=="Public") %>%
  mutate(nonprofit = control_name=="Private (Nonprofit)") %>%
  mutate(forprofit = control_name=="Private (For-Profit)")

#Remove non-numeric income rows
model_df <- model_df %>% mutate(earnings=as.integer(`md_earn_wne_p10-REPORTED-EARNINGS`)) %>% filter(!is.na(earnings))

#Plot earnings vs avg_index
ggplot(model_df, aes(x=earnings, y=avg_index, color=control_name)) + geom_point()
ggplot(model_df, aes(x=earnings, y=avg_index, color=locale_type)) + geom_point()

sumtable(model_df)

#Assign earnings groups
model_df <- model_df %>%
  mutate(earning_group = ifelse(earnings < mean(earnings) - sd(earnings)/2,'Low', 
                                ifelse(earnings > mean(earnings) + sd(earnings)/2, 'High', 'Med')))
sumtable(model_df)

ggplot(model_df, aes(x=earnings, y=avg_index, color=earning_group)) + geom_point()

lm <- lm(data=model_df, avg_index ~ earning_group + control_name)
export_summs(lm)

linearHypothesis(lm, c("GRAD_DEBT_MDN_SUPP"))
plot_coefs(lm)
effect_plot(lm, earning_group, plot.points = TRUE)




