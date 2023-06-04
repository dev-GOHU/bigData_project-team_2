employment = read.csv("./Employment_rate.csv", 
                      header = TRUE,
                      quote = "\"")
table(employment$Flag.Codes)
library(dplyr)
employment <- employment %>%
  filter(Flag.Codes=="" & SUBJECT=="TOT" & nchar(TIME)==4 & MEASURE=="PC_WKGPOP" & !is.na(Value)) %>%
  select(LOCATION, TIME, Value)

employment$TIME = as.integer(employment$TIME)
names(employment)[names(employment)=="Value"] <- "Employment_rate"

##################

#대학 진학률
univ <- read.csv("./college_entrance_rate.csv", header=TRUE)
univ <- univ %>%
  filter(SUBJECT=="25_34" & !is.na(Value)) %>%
  select(LOCATION, TIME, Value) 
names(univ)[names(univ)=="Value"] = "univ_rate"

##################

#GDP
gdp <- read.csv("./OECD_GDP.csv", header=TRUE, quote="\"")
gdp <- gdp %>%
  filter(MEASURE=="USD_CAP" & !is.na(Value) & Flag.Codes=="") %>%
  select(LOCATION, TIME, Value)
names(gdp)[names(gdp)=="Value"] = "gdp"

###################

#노동 가능 인구
work_pop <- read.csv("./OECD_working age pop.csv", header=TRUE, quote="\"")
work_pop <- work_pop %>%
  filter(!is.na(Value)) %>%
  select(LOCATION, TIME, Value)
names(work_pop)[names(work_pop)=="Value"] = "work_pop"

##################
# 병합
df1 <- employment %>%
  merge(univ, by=c("LOCATION", "TIME")) %>%
  merge(gdp, by=c("LOCATION", "TIME")) %>%
  merge(work_pop, by=c("LOCATION", "TIME"))

plot(df1 %>%select(-LOCATION, -TIME))
View(df1)
  
df <- df1 %>% 
  select(-LOCATION, -TIME, -work_pop)

save(df, file="./df.rda")
