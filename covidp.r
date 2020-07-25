setwd("/home/suman/Documents/data science/practical")

#original dataset
covid<-read.csv("Covid_dataset2020.csv" , stringsAsFactors = FALSE)
str(covid)
View(covid)


## CLEANING/TIDYING DATASET ##

# 1. renaming columns case_in_country and sub_country 
# 2. removing unique_id column (is not necessary for analysis) 
# 3. converted 'reporting_date' , 'symptom_onset' ,'visit_date_hosp' ,'exposure_Startdate' ,exposure_enddate' ,
#  'death' , 'recovered' to Date format 
#  converting to Date format to remove anomalies of different patterns of date format being used 
#  using default format "year-month-date"
# 4. removed 0 and 1 values(anomaly as it should contain dates) from columns 'death' and 'recovered' 
# 5. filled NA values in column 'intl_traveler' for all blank entries 
# 6. filled NA value for 'intl_traveler' where the mistaken value was 'W'
# 7. filled NA values in column 'lives_in_Wuhan' for all blank entries 
# 8. filled NA values in column 'visiting_Wuhan' for all blank entries 
# 9. filled NA values in column 'gender' for all blank entries 
# 10. filled NA values in column 'sub_country' for all blank entries 
# 11. filled NA values in column 'symptom' for all blank entries 

library(tidyverse)

covid %>%
  select(-unique_id) %>%
  rename(no_of_cases = case_in_country , region = sub_country) %>%
  mutate(reporting_date = as.Date(reporting_date , format = "%m/%d/%Y")) %>%
  mutate(symptom_onset = as.Date(symptom_onset , format = "%m/%d/%Y")) %>%
  mutate(visit_date_hosp = as.Date(visit_date_hosp , format = "%m/%d/%Y"))  %>%
  mutate(exposure_startdate = as.Date(exposure_startdate , format = "%m/%d/%Y")) %>%
  mutate(exposure_enddate = as.Date(exposure_enddate , format = "%m/%d/%Y")) %>%
  mutate(death = as.Date(death , format = "%m/%d/%Y")) %>%
  mutate(recovered = as.Date(recovered , format = "%m/%d/%Y")) %>%
  mutate(intl_traveler = na_if(intl_traveler , "")) %>%
  mutate(intl_traveler = na_if(intl_traveler , "W")) %>%
  mutate(lives_in_Wuhan = na_if(lives_in_Wuhan , "")) %>%
  mutate(visiting_Wuhan = na_if(visiting_Wuhan , "")) %>%
  mutate(gender = na_if(gender , "")) %>%
  mutate(region = na_if(region, "")) %>%
  mutate(symptom = na_if(symptom , "")) -> newcovid


View(newcovid)
str(newcovid)

## creating cleaned data csv file 

write.csv(newcovid , file = "/home/suman/Documents/data science/practical/tidydataset_2017CSC1067.csv" ,
          row.names = FALSE)

## RESEARCH PROBLEM ##
## ** taking care of all the NA values ** ##
## no of people affected living in Wuhan and visiting Wuhan , how does this affects the death rate and 
## recovery rate , no. of cases reported date wise with the date of symptoms onset , symptoms seen most 
## frequently , age wise and country wise infection transmission , no of cases present region wise , how 
## many travellers are affected .

rd<-read.csv("tidydataset_2017CSC1067.csv")
str(rd)
View(rd)
summary(rd)


## continuous data - age , no_of_cases , intl_traveler , dom_traveler
## categorical data - country , region , gender , death , recovered , symptom , visitinh_Wuhan , 
## lives_in_Wuhan , exposure_startdate , exposure_enddate , visit_hosp , symptom_onset , reporting_date

# variance , correlation 
## on continuous data 
## age and no_of_cases
var(rd[ ,c(2,6)] , na.rm = TRUE)
cor(rd[ ,c(2,6)] , use = "na.or.complete")

## corrleation between no_of_cases and intl_traveler/dom_traveler cant be computed because standard 
## deviation is 0 
# variance also doesnt show much information 
# var(rd[ ,c(2,9)] , na.rm = TRUE)
# var(rd[ ,c(2,10)] , na.rm = TRUE)

## extracting importatnt conclusions 

# region wise no_of_cases 
a<-summarise(group_by(rd , region) , cases = n())
View(a)

# gender wise no_of_cases , males are more infected 
b<-summarise(group_by(rd , gender) , cases = n() )
b

# age wise no_of_cases reported 
c<-summarise(group_by(rd , age) , cases = n())
View(c)

# counting the no. of cases using diffrent factors 
## no_of_cases reported of people living in Wuhan and not living 
aggregate(no_of_cases ~ lives_in_Wuhan , data = rd , FUN = length , na.action = na.omit)

## no_of_cases reported of people visiting Wuhan and not 
aggregate(no_of_cases ~ visiting_Wuhan , data = rd , FUN = length , na.action = na.omit)

## no_of_cases shown country wise and gender wise 
aggregate(no_of_cases ~ country + gender , data = rd , FUN = length , na.action = na.omit)

## count of people not got infected after travelling within country 
aggregate(no_of_cases ~ dom_traveler , data = rd , FUN = length , na.action = na.omit)

## people travelled internationally and got infected 
aggregate(no_of_cases ~ intl_traveler , data = rd , FUN = length , na.action = na.omit)

# gender wise summary of no. of cases 
by(rd$no_of_cases , rd$gender , summary , simplify = TRUE)

# mean of the deaths occured in total no. of cases datewise 
by(rd$no_of_cases , rd$death , mean)

# mean of the recovered people in total no. of cases datewise 
by(rd$no_of_cases , rd$recovered , mean)


## GRAPHS ##

## UNIVARIATE ANALYSIS
## continuous data 
summary(rd$no_of_cases)
## central tendency of no_of_cases
boxplot(rd$no_of_cases , border = "red" , col = "grey" , ylab = "no_of_cases")
## density curve for no_of cases 
plot(density(rd$no_of_cases[!is.na(rd$no_of_cases)]) , col = "purple" , main = "density curve for 
     no_of_cases")

## density curves for intl_traveller and dom_traveler doesnt gives much details 
#plot(density(rd$dom_traveler[!is.na(rd$dom_traveler)]) , col = "green" , main = "density curve for 
#     dom_traveller")

#plot(density(rd$intl_traveler[!is.na(rd$intl_traveler)]) , col = "pink" , main = "density curve for 
#     intl_traveller")

## categorical data 
plot(rd$lives_in_Wuhan , col = "pink" , main = "count of people in Wuhan")
plot(rd$visiting_Wuhan , col = "green" , main = "count of people visiting Wuhan")

pie(table(rd$gender) , main = "distribution of male and female")
## pie charts of same (not much clear to observe the difference)
#pie(table(rd$lives_in_Wuhan))
#pie(table(rd$visiting_Wuhan))

## on categorical and continuous data
## BIVARIATE ANALYSIS
# no. of cases in different countries distributed according to gender    
ggplot(rd , aes(x=country , y=no_of_cases , color=gender)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm")
# same graph as above shown using boxplot
ggplot(rd , aes(x=country , y=no_of_cases , color=gender)) +
  geom_boxplot() 

# no of cases in different countries age wise
ggplot(rd , aes(x=country , y=no_of_cases , color=age)) +
  geom_boxplot() 
## same graph as above using quickplot 
qplot(country , no_of_cases , data = rd , geom = c("point" , "line" , "jitter") , color = age)

#cases reported country wise and the count of male and females are shown 
# most of the case is reported in month Feb , USA has most of the reportig in March
ggplot(rd , aes(x=country , y=reporting_date , color = gender)) +
  geom_point() +
  geom_smooth()


# graph between countries reporing_date_month and symptom_onset date
# symptoms are also mostly seen in Feb in most of the counties 
d<-as.Date(rd$reporting_date , format = "%Y-%m-%d")
rd %>%
  mutate(reporting_month = format(d ,"%b")) %>%
  ggplot(aes(x=reporting_month , y=symptom_onset , color = country)) +
  geom_point() +
  facet_grid(~gender)


## time delay graph between symptom_onset and visiting hospital
## all the people with symptoms were taken to hospitals in time , visiting hospital rate increases
## as the no. of symptomatic cases increases , males are more affected as seen in graph 
d1<-as.Date(rd$symptom_onset , format = "%Y-%m-%d")
rd %>%
  mutate(sym_onset_month = format(d1 , "%b")) %>%
  ggplot(aes(x=sym_onset_month , y=visit_date_hosp , color = country)) +
  geom_point() +
  geom_smooth() +
  geom_jitter() +
  facet_wrap(~gender , ncol = 3 ,scales = "free_y")


## UNIVARIATE ANALYSIS
# people with positive cases living/not living in Wuhan from different countries gender wise  
## 0 / no - not living 
## 1 / yes - living 
## the graph matches the below data (the data is already extracted above) 
## aggregate(no_of_cases ~ lives_in_Wuhan , data = rd , FUN = length , na.action = na.omit)
ggplot(rd , aes(x=lives_in_Wuhan , color = gender)) +
  geom_bar()  +
  facet_wrap(~country)

## BIVARIATE ANALYSIS
## graph linking deaths of people visiting Wuhan 
## death rate of people not visiting Wuhan is more 
ggplot(rd , aes(x=visiting_Wuhan , y=death , color = country)) +
  geom_point() +
  geom_jitter() 

## graph between recovered people with the end of exposure 
## the data of recovered column is less (mostly NA)
## recoverey rate is higher for counties like Vietnam , USA , Italy in Jan and Feb month 
ggplot(rd , aes(x=recovered , y=exposure_enddate , color = country)) +
  geom_point() +
  geom_jitter()

## graph between exposure_start and symptoms shown 
# fever and cough are most common symptoms found 
# people of age above 60 are more affected   
df<-rd %>% filter(symptom %in% c("fever","nausea","chills","cough","sore body")) 
ggplot(df , aes(x=symptom , y=exposure_startdate , color = age)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~country ,ncol = 3)










