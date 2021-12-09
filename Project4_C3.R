## Project4 C3 ################################################################
# 기간: 21.11.22(월)
# C3조: 오준서 천성한 한호종 황윤수
# 환경설정

rm(list=ls())
install.packages("caret")
install.packages("NbClust")
install.packages("nnet")
install.packages("neuralnet") #인공신경망
install.packages('rpart') #의사결정트리
install.packages("rpart.plot") #트리 시각화
install.packages('randomForest')

library(randomForest) # 랜덤포레스트()
library(rpart)
library(rpart.plot)
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




# 2-3)의사결정트리 ####
# (1) 데이터 준비
abalone <- read.csv('C:/Rwork/abalone.csv', header = F); #데이터파일
names(abalone) <- c('Sex', 'Length', 'Diameter', 'Height', 'WholeWeight',
                    'ShuckedWeight','VisceraWeight','ShellWeight','Rings')
abalone3 <- abalone
abalone3$Sex[abalone3$Sex == 'M'] <- 1
abalone3$Sex[abalone3$Sex == 'F'] <- 2
abalone3$Sex[abalone3$Sex == 'I'] <- 0
sexc <- abalone3$Sex
sexi <- as.integer(sexc)
abalone3$Sex <- sexi

# (2) 학습데이터, 검정데이터 생성
idx3 <- createDataPartition(abalone3$Rings, p = 0.7, list = F)
abaloneTrain3 <- abalone3[idx3,] #학습데이터
abaloneTest3 <- abalone3[-idx3,] #테스트데이터

# (3) 의사결정트리(Decision Tree) 모델 생성
rpartFit <- rpart(Rings ~ ., data = abaloneTrain3)
summary(rpartFit)

# (4) 시각화 및 성능평가
rpart.plot(rpartFit, digits = 3, type = 0, extra = 1, fallen.leave = F, cex = 1)

rpartPre <- predict(rpartFit, newdata = abaloneTest3)
table(rpartPre, abaloneTest3$Rings)
cor(rpartPre, abaloneTest3$Rings)




# 2-4)인공신경망 ####
# (1)데이터 전처리
abalone <- read.csv('C:/Rwork/abalone.csv', header = F); #데이터파일
names(abalone) <- c('Sex', 'Length', 'Diameter', 'Height', 'WholeWeight',
                    'ShuckedWeight','VisceraWeight','ShellWeight','Rings')
abalone4 <- abalone
abalone4$Sex[abalone4$Sex == 'M'] <- 1
abalone4$Sex[abalone4$Sex == 'F'] <- 2
abalone4$Sex[abalone4$Sex == 'I'] <- 0
sexc <- abalone4$Sex
sexi <- as.integer(sexc)
abalone4$Sex <- sexi

idx4 <- createDataPartition(abalone4$Rings, p = 0.7, list = F)
abaloneTrain <- abalone4[idx4,] #학습데이터
abaloneTest <- abalone4[-idx4,] #테스트데이터

# (2)데이터 정규화
normal <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}

# (3)학습데이터, 검정데이터 생성
training_nor4 <- as.data.frame(lapply(abaloneTrain, normal))
summary(training_nor4)
testing_nor4 <- as.data.frame(lapply(abaloneTest, normal))
summary(testing_nor4)

# (4)인공신경망 모델 생성
model_net41 = neuralnet(Rings ~ ., data = training_nor4, hidden = 1)
model_net41

model_net43 = neuralnet(Rings ~ ., data = training_nor4, hidden = 3)
model_net43

# (5)시각화
plot(model_net41)
plot(model_net43)

# (6)분류모델 성능 평가
# compute()사용
model_result41 <- compute(model_net41, testing_nor4[-9])
model_result43 <- compute(model_net43, testing_nor4[-9])
# 상관관계를 통한 정확도 평가
cor(model_result41$net.result, testing_nor4$Rings)
cor(model_result43$net.result, testing_nor4$Rings)




# 2-5)앙상블 기법-랜덤 포레스트 기법 ####
# (1) 데이터 전처리
abalone5 <- read.csv('C:/Rwork/abalone.csv', header = F); #데이터파일
names(abalone5) <- c('Sex', 'Length', 'Diameter', 'Height', 'WholeWeight',
                     'ShuckedWeight','VisceraWeight','ShellWeight','Rings')

# (2) 학습데이터, 테스트데이터 생성
idx5 <- createDataPartition(abalone5$Rings, p = 0.7, list = F)
abaloneTrain <- abalone5[idx5,] #학습데이터
abaloneTest <- abalone5[-idx5,] #테스트데이터

# (3) 랜덤포레스트 모델 생성
rfAba <- randomForest(Rings ~ ., data=abaloneTrain, ntree=100, proximity=TRUE) #컬럼명에 띄어쓰기 있으면 오류
#proximity=TRUE 는 개체들 간의 근접도 행렬을 제공 : 동일한 최종노드에 포함되는 빈도에 기초함
table(predict(rfAba), abaloneTrain$Rings)
rfAba

# (4) 중요 변수 출력 및 시각화
importance(rfAba)

varImpPlot(rfAba)
plot(rfAba)

# (5) 모델 평가
rf.predAba <- predict(rfAba, newdata = abaloneTest)
table(rf.predAba, abaloneTest$Rings)
cor(rf.predAba, abaloneTest$Rings)




## 3번문제 ####################################################################
# iris 데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering을 실행하고
# 시각화 하시오.



