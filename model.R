install.packages("rstudioapi")

#현재 파일이 있는 Directory에 있는 data directory에 접근
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("./data/df.rda")

install.packages("caret")
install.packages("dplyr")

library(caret)
library(dplyr)

####################
#학습셋 테스트셋 분리
install.packages("rsample")
library(rsample)

#랜덤으로 추출하기 위해 시드 초기화
set.seed(123)
#학습셋과 테스트셋으로 나눔
data_split <- initial_split(select(EMP_data, -LOCATION, -TIME), prop=0.8)

train <- training(data_split)
test <- testing(data_split)
###################
# k-nn regression 학습
install.packages("caret")
library(caret)

k_value = 1
min_mse = 100
for(i in 1:(nrow(train)%/%2)){
  set.seed(123)
  data_split <- initial_split(train, prop=0.8)
  validation <- testing(data_split)
  train2 <- training(data_split)
  
  knnModel <- knnreg(Employment_rate~., data = train2, k=i)
  mse <- mean((validation$Employment_rate - predict(knnModel, select(validation, -Employment_rate)))^2)
  if(min_mse > mse){
    min_mse = mse
    k_value = i
  }
}
knnModel <- knnreg(Employment_rate~., data = train, k=k_value)
mse <- mean((test$Employment_rate - predict(knnModel, select(test, -Employment_rate)))^2)

####################

#SVR 모델
install.packages("e1071")
library(e1071)
svrModel <- svm(Employment_rate~., data=train, gamma=1, cost=16)
mse <- mean((test$Employment_rate - predict(svrModel, select(test, -Employment_rate)))^2)
mse

####################

#GLM 모델
lmModel <- lm(Employment_rate~., data=train)
mse <- mean((test$Employment_rate - predict(lmModel, select(test, -Employment_rate)))^2)
mse

####################

##### 분석할때는 svrModel로 해주세요. 제일 오차가 적어요 #####

#값 적용 예시
## 한국의 시대별 실제 고용률과 예측 고용률 비교 그래프
EMP_data %>% 
  filter(LOCATION=="KOR") %>%
  mutate(predEmp = predict(svrModel, select(., -LOCATION, -TIME, -Employment_rate))) %>%
  ggplot(aes(x=TIME, y=Employment_rate)) +
  geom_point(color='blue') + geom_line(color='blue') +
  geom_point(aes(y=predEmp), color='green') + geom_line(aes(y=predEmp), color='green')