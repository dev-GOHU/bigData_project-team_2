load("./df.rda")

install.packages("caret")
library(caret)
library(dplyr)

train <- sample_frac(df, size=0.7)
test <- df %>% anti_join(train)

model <- knnreg(Employment_rate~univ_rate,
                 data = df)
p1 <- predict(model, test[,c("univ_rate")])
