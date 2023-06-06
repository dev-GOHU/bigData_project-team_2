#Flag.Codes: 데이터 상태
## ..Not available
# B Break in series
# E Estimated value
# F Forecast value
# X Not applicable
# P Provisional data
# S Strike
# - Nil

# working Directory 설정 및 필요 패키지 설치 및 적용
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages("dplyr")
library(dplyr)

########################
# 고용률 (Employment rate)
emp = read.csv("./Employment_rate.csv", 
                      header = TRUE,
                      quote = "\"")

emp <- emp %>%
  filter(Flag.Codes=="" & SUBJECT=="TOT" & FREQUENCY=="A" & MEASURE=="PC_WKGPOP" & !is.na(Value)) %>%
  select(LOCATION, TIME, Value)

emp$TIME = as.integer(emp$TIME)
emp$LOCATION = factor(emp$LOCATION)
names(emp)[names(emp)=="Value"] <- "EMP"

##################

#대학 진학률
univ <- read.csv("./college_entrance_rate.csv", header=TRUE, stringsAsFactors = TRUE)
univ <- univ %>%
  filter(SUBJECT=="25_34" & !is.na(Value)) %>%
  select(LOCATION, TIME, Value) 
names(univ)[names(univ)=="Value"] = "UNIV"

##################

#1인당 GDP
gdp <- read.csv("./OECD_GDP.csv", header=TRUE, quote="\"", stringsAsFactors = TRUE)
gdp <- gdp %>%
  filter(MEASURE=="USD_CAP" & !is.na(Value) & Flag.Codes=="") %>%
  select(LOCATION, TIME, Value)
names(gdp)[names(gdp)=="Value"] = "GDP_CAP"

###################

#노동 가능 인구
work_pop <- read.csv("./OECD_working age pop.csv", header=TRUE, quote="\"", stringsAsFactors=TRUE)
work_pop <- work_pop %>%
  filter(!is.na(Value)) %>%
  select(LOCATION, TIME, Value)
names(work_pop)[names(work_pop)=="Value"] = "WKGPOP"

#경제 활동 비율
lfpr <- read.csv("./OECD_Labour force participation rate.csv", header=TRUE, quote="\"", stringsAsFactors=TRUE)
lfpr <- lfpr %>%
  filter(!is.na(Value) & SUBJECT=="15_64") %>%
  select(LOCATION, TIME, Value)
names(lfpr)[names(lfpr)=="Value"] = "LFPR"

#노동 가능 인구 비율 * 경제 활동 비율 / 100
lfp_tot <- lfpr %>%
  merge(work_pop, by=c("LOCATION", "TIME")) %>%
  mutate(LFP_TOT = LFPR*WKGPOP/100) %>%
  select(LOCATION, TIME, LFP_TOT)

##################

#임금 (PPP환율)
wages <- read.csv("./OECD_Avg Wages.csv", header=TRUE, quote="\"", stringsAsFactors =  TRUE)
wages <- wages %>%
  filter(!is.na(Value)) %>%
  select(LOCATION, TIME, Value)
names(wages)[names(wages)=="Value"] = "WAGES"

##################
# 병합
EMP_data <- emp %>%
  merge(univ, by=c("LOCATION", "TIME")) %>%
  merge(gdp, by=c("LOCATION", "TIME")) %>%
  merge(lfp_tot, by=c("LOCATION", "TIME")) %>%
  merge(wages, by=c("LOCATION", "TIME"))

save(EMP_data, file="./EMP_data.rda")
