## Project4 C3 ################################################################
# 기간: 21.11.22(월)
# C3조: 오준서 천성한 한호종 황윤수
# 환경설정

rm(list=ls())
install.packages("caret")
install.packages("NbClust")
install.packages("nnet")
install.packages("neuralnet")
library(caret)
library(NbClust)
library(nnet)
library(neuralnet)

## 1번문제 ####################################################################
# 위스콘신 유방암 데이터셋을 대상으로 분류기법 2개를 적용하여 기법별 결과를 비교하시오.
# 종속변수는 diagnosis: Benign(양성), Malignancy(악성)




## 2번문제 ####################################################################
# Abalone Data 데이터셋을 대상으로 전복의 나이를 예측하고자 한다. 예측기법 2개를
# 적용하여 기법별 결과를 비교하시오
# 종속변수는 Rings를 사용

# 2-2)다중회귀분석 ####
# (1)데이터 전처리
abalone <- read.csv("abalone.csv", header = F)
names(abalone) <- c("Sex","Length","Diameter continuous","Height","Whole weight","Shucked weight",
                    "Viscera weight","Shell weight","Rings")
abalone2 <- abalone
abalone2$Sex[abalone2$Sex == "M"] <- 1 # 문자열 숫자로 변화
abalone2$Sex[abalone2$Sex == "F"] <- 2
abalone2$Sex[abalone2$Sex == "I"] <- 0
abalone2$Sex <- as.numeric(abalone2$Sex) # 문자열을 수열로 변환

# (2)회귀분석모델 생성
formula2 <- Rings ~ .
idx2 <- sample(1:nrow(abalone2), 0.7*nrow(abalone2))
train2 <- abalone2[idx2,] #학습데이터
test2 <- abalone2[-idx2,] #검정데이터

a_model <- lm(formula2, data = train2) # 모델생성
a_model
summary(a_model)

# (3)모델 평가
pred1 <- predict(a_model,test2) #예측치 생성
pred1

cor(pred1, test2$Rings) #회귀모델 평가




## 3번문제 ####################################################################
# iris 데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering을 실행하고
# 시각화 하시오.



