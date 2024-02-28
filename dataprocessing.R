#Data processing



# Import libraries & data -------------------------------------------------

library(tidyverse)
#library(countrycode) #used to get ISO3 names from countries

source('dataloading.R')

# Ppath (Individual-level data) -------------------------------------------

#Filter data
### Rows ### 
# 1. Filter for individuals who arrived after 2013 (psample is 17,18,19)
# 2. Filter for direct refugee experience (2), indirect refugee experience (3)

### Columns ###
#pid - 
#sid -	Survey Year
#gebjahr - birthyear (four digit)
#gebmonat - birth month
#psample - Sample Member
#immiyear - Year Moved to Germany
#corigin - Country Born In

ppath <- ppath %>% 
  filter(psample %in% c(15,16,17,18,19) & arefback %in% c(2,3)) %>% 
  select(pid, arefback, sex, gebjahr, gebmonat, psample, immiyear, corigin)



# Spell (trajectory data) -------------------------------------------------

#Combine and clean the spell data
### Rows ###
# 1. Remove illogical start & end sequences

### Columns ###
#pid
#sample1 - Sample Member
#mignr - Running no. of move
#start_date - invented variable date of start
#end_date - invented variable date of end
#staytime - Duration of stay (months)
#country - Country of the stay

#Bind and clean spell data
spells <- bind_rows(migspell, refugspell) %>% 
  #remove illogical start and end dates
  filter(start>=1 & end>start) %>% 
  #remove no answer (-1), Does not apply (-2), 
  #Not assignable (999), Ethnic minority (199)
  filter(!(country %in% c(-2,-1,199,999))) %>% 
  
  #Create a data variable that transforms months from jan 1900
  mutate(start_date = months(start) + dmy('01-01-1900'),
         end_date = months(end) + dmy('01-01-1900')) %>% 
  select(pid, sample1, mignr, start_date, end_date, start, end, staytime, country)

#Remove no longer needed individual refugee and mig spell datasets
rm(refugspell)
rm(migspell)


# Combine ppathl & spell data; Edit sequence dataset ---------------------------------------------

sequence <- left_join(spells, ppath, by = "pid") %>% 
  filter(!is.na(arefback)) 
#distinct(pid, mignr, .keep_all = TRUE)


#Clean the sequence dataset
start_month <- 1320 #1320 is starting the sequence in 2010

#Filter out all individuals whose first migration (mignr = 1) was before start_month variable
sequence <- sequence %>% 
  group_by(pid) %>% 
  filter(any(start > start_month & mignr == 1)) 



# Country Labels ----------------------------------------------------------

sequence <- sequence %>% 
  mutate(country = case_when(country == 1 ~ "Germany",
                                   country == 2 ~ "Turkey",
                                   country == 3 ~ "Yugoslavia",
                                   country == 4 ~ "Greece",
                                   country == 5 ~ "Italy",
                                   country == 6 ~ "Spain",
                                   country == 10 ~ "Austria",
                                   country == 11 ~ "France",
                                   country == 13 ~ "Denmark",
                                   country == 14 ~ "Great Britain",
                                   country == 15 ~ "Sweden",
                                   country == 16 ~ "Norway",
                                   country == 17 ~ "Finland",
                                   country == 18 ~ "USA",
                                   country == 19 ~ "Switzerland",
                                   country == 20 ~ "Chile",
                                   country == 21 ~ "Romania",
                                   country == 22 ~ "Poland",
                                   country == 23 ~ "Korea",
                                   country == 24 ~ "Iran",
                                   country == 25 ~ "Indonesia",
                                   country == 26 ~ "Hungary",
                                   country == 27 ~ "Bolivia",
                                   country == 28 ~ "Portugal",
                                   country == 29 ~ "Bulgaria",
                                   country == 30 ~ "Syria",
                                   country == 31 ~ "Czech Republic",
                                   country == 32 ~ "Russia",
                                   country == 34 ~ "Mexico",
                                   country == 35 ~ "Argentina",
                                   country == 37 ~ "Benin",
                                   country == 38 ~ "Philippines",
                                   country == 39 ~ "Israel",
                                   country == 40 ~ "Japan",
                                   country == 41 ~ "Australia",
                                   country == 42 ~ "India",
                                   country == 43 ~ "Afghanistan",
                                   country == 44 ~ "Thailand",
                                   country == 45 ~ "Jamaica",
                                   country == 46 ~ "Saudi Arabia",
                                   country == 47 ~ "Ethiopia",
                                   country == 48 ~ "Colombia",
                                   country == 49 ~ "Ghana",
                                   country == 50 ~ "Bangladesh",
                                   country == 51 ~ "Venezuela",
                                   country == 52 ~ "Tunisia",
                                   country == 53 ~ "Mauritius",
                                   country == 54 ~ "Nigeria",
                                   country == 55 ~ "Canada",
                                   country == 56 ~ "New Zealand",
                                   country == 58 ~ "Cyprus",
                                   country == 59 ~ "Cuba",
                                   country == 60 ~ "Iraq",
                                   country == 61 ~ "Brazil",
                                   country == 63 ~ "Hong Kong",
                                   country == 64 ~ "Peru",
                                   country == 65 ~ "Sri Lanka",
                                   country == 66 ~ "Nepal",
                                   country == 67 ~ "Morocco",
                                   country == 68 ~ "China",
                                   country == 70 ~ "Iceland",
                                   country == 71 ~ "Ireland",
                                   country == 73 ~ "Moldova",
                                   country == 74 ~ "Kazakhstan",
                                   country == 75 ~ "Albania",
                                   country == 76 ~ "Lebanon",
                                   country == 77 ~ "Kyrgyzstan",
                                   country == 78 ~ "Ukraine",
                                   country == 79 ~ "Algeria",
                                   country == 80 ~ "Mozambique",
                                   country == 81 ~ "Egypt",
                                   country == 82 ~ "Tajikistan",
                                   country == 83 ~ "Vietnam",
                                   country == 84 ~ "Somalia",
                                   country == 85 ~ "Pakistan",
                                   country == 86 ~ "South Africa",
                                   country == 87 ~ "UAE",
                                   country == 88 ~ "El Salvador",
                                   country == 89 ~ "Eritrea",
                                   country == 90 ~ "Jordan",
                                   country == 93 ~ "Singapore",
                                   country == 94 ~ "Burkina Faso",
                                   country == 95 ~ "Zambia",
                                   country == 96 ~ "Ecuador",
                                   country == 97 ~ "Uzbekistan",
                                   country == 100 ~ "Laos",
                                   country == 101 ~ "Estonia",
                                   country == 102 ~ "Angola",
                                   country == 103 ~ "Latvia",
                                   country == 105 ~ "Namibia",
                                   country == 108 ~ "Dominican Republic",
                                   country == 109 ~ "Nicaragua",
                                   country == 110 ~ "Kenya",
                                   country == 111 ~ "Libya",
                                   country == 112 ~ "Malta",
                                   country == 113 ~ "Botswana",
                                   country == 116 ~ "Luxembourg",
                                   country == 117 ~ "Belgium",
                                   country == 118 ~ "The Netherlands",
                                   country == 119 ~ "Croatia",
                                   country == 120 ~ "Bosnia-Herzegovina",
                                   country == 121 ~ "Macedonia",
                                   country == 122 ~ "Slovenia",
                                   country == 123 ~ "Slovakia",
                                   country == 125 ~ "Guinea",
                                   country == 126 ~ "Kuwait",
                                   country == 127 ~ "Ivory Coast",
                                   country == 128 ~ "Malaysia",
                                   country == 129 ~ "Samoa",
                                   country == 130 ~ "Azerbaijan",
                                   country == 132 ~ "Belarus",
                                   country == 133 ~ "Uruguay",
                                   country == 135 ~ "Uganda",
                                   country == 136 ~ "Oman",
                                   country == 138 ~ "Mali",
                                   country == 139 ~ "Cameroon",
                                   country == 140 ~ "Kosovo",
                                   country == 141 ~ "Georgia",
                                   country == 142 ~ "Sudan",
                                   country == 143 ~ "Congo",
                                   country == 144 ~ "Togo",
                                   country == 145 ~ "Mongolia",
                                   country == 146 ~ "Lithuania",
                                   country == 147 ~ "Chad",
                                   country == 148 ~ "Armenia",
                                   country == 149 ~ "Kurdistan",
                                   country == 150 ~ "Liberia",
                                   country == 151 ~ "Yemen",
                                   country == 152 ~ "Palestine",
                                   country == 154 ~ "Taiwan",
                                   country == 155 ~ "Turkmenistan",
                                   country == 158 ~ "Sierra Leone",
                                   country == 161 ~ "Bahrain",
                                   country == 162 ~ "Senegal",
                                   country == 165 ~ "Serbia",
                                   country == 166 ~ "Gambia",
                                   country == 168 ~ "Montenegro",
                                   country == 170 ~ "Surinam",
                                   country == 171 ~ "Guyana",
                                   country == 173 ~ "Zimbabwe",
                                   country == 174 ~ "Madagascar",
                                   country == 178 ~ "Rwanda",
                                   country == 179 ~ "Malawi",
                                   country == 181 ~ "Myanmar",
                                   country == 183 ~ "Niger",
                                   country == 186 ~ "South Sudan",
                                   country == 187 ~ "Mauritania",
                                   country == 188 ~ "Russia",
                                   country == 190 ~ "Djibouti",
                                   country == 193 ~ "Qatar",
                                   country == 194 ~ "Sahara",
                                   country == 195 ~ "Russia"))



# Syria sequence data-----------------------------------------------------

#Select only those who were born in Syria
syria_data <- sequence[sequence$corigin == 30, ]


#set all start dates to jan 1 2010 to provide common
#point of refrence. If dates started at different points in time
#there'd be no way to account for temporal factors, so need a 
#unified start
syria_sequence_sync <- syria_data %>% #this is its own dataset since its useful for vizs
  mutate(start_date = if_else(mignr == 0, 
                              as_date("2010-01-01"), 
                              start_date)) %>% 
  #update month number
  mutate(start = if_else(mignr == 0, 
                         1320, 
                         start)) 


#change Germany to be an 'absorbing state', meaning
#once germany is reached time spent is 1
syria_sequence <- syria_sequence_sync %>% 
  mutate(end = if_else(country == "Germany",
                       start, 
                       end)) %>% 
  #redefine end date, so it's formatted correctly
  mutate(end_date = months(end) + dmy('01-01-1900')) %>% 
  #recalculate staytime
  mutate(staytime = end-start+1)



#Remove countries that received <2% of individuals
#These 8 represent 90% of all individuals, the rest are spread between ~40 countries
small_sample <- syria_sequence %>% 
  ungroup() %>% 
  filter(!country %in% c('Syria', 'Germany')) %>% 
  count(country) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  filter(prop < 0.02) %>% 
  pull(1)

syria_sequence <- syria_sequence %>% 
  group_by(pid) %>% 
  filter(!any(country %in% small_sample)) %>%
  ungroup() 


# Transform for specific outputs ------------------------------------------


## Transform in to format for model

#Turn data into long sequence where each row represents a month
syria_mhmm <- syria_sequence %>% 
  rowwise() %>%
  mutate(pid,
         date = list(seq(start_date, end_date, by = "month"))) %>% 
  unnest(date) %>% 
  #sort data in chronlogical order
  group_by(pid, date) %>% 
  arrange(date, .by_group = TRUE) %>% 
  ungroup() %>% 
  select(pid,country) 





  






