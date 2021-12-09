## Project4 C3 ################################################################
# 기간: 21.11.22(월)
# C3조: 오준서 천성한 한호종 황윤수
# 환경설정

rm(list=ls())
install.packages("caret") #데이터파티션()
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




## 3번문제 ####################################################################
# iris 데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering을 실행하고
# 시각화 하시오.

# 3-1)K-means clustering
# (1)데이터 전처리
data("iris")
iris1 <- iris[,-5] #species 칼럼 데이터 제거

inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F) #균등한 테스트를 위해 iris데이터 이용
table(iris$Species[inTrain])

training <- iris1[inTrain,]
testing <- iris1[-inTrain,]

# (2)표준화
training.data <- scale(training)
summary(training.data)

# (3)모델생성
iris.kmeans <- kmeans(training.data, centers = 3, iter.max = 10000)
iris.kmeans$centers

# (4)군집확인
training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = training)
qplot(Sepal.Width, Sepal.Length, colour = cluster, data = training)

# (5)예측모델, 모델 정확성 확인
training.data <- as.data.frame(training.data)
modFit <- train(x = training.data, 
                y = training$cluster,
                method = "rpart")

testing.data <- as.data.frame(scale(testing))
testClusterPred <- predict(modFit, testing.data)



