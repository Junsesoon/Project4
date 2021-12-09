## Project4 C3 ################################################################
# 기간: 21.11.22(월)
# C3조: 오준서 천성한 한호종 황윤수
# 환경설정

rm(list=ls())
install.packages("caret")
install.packages("NbClust")
install.packages("nnet")
install.packages("neuralnet")
install.packages('randomForest') #랜덤포레스트()

library(randomForest)
library(caret)
library(NbClust)
library(nnet)
library(neuralnet)

## 1번문제 ####################################################################
# 위스콘신 유방암 데이터셋을 대상으로 분류기법 2개를 적용하여 기법별 결과를 비교하시오.
# 종속변수는 diagnosis: Benign(양성), Malignancy(악성)

# 1-2) 나이브 베이즈 ####
# (1)데이터 전처리
data1 <- read.csv('C:/rwork/wdbc.csv',header = F)
wisconsin2 <- data1 %>% select(2:12,) %>% 
  `colnames<-`(c('양성여부','반경','질감','둘레','면적','매끄러움','조그만정도',
                 '오목함','오목점','대칭','프랙탈차원'))

# (2) 학습 데이터, 검정 데이터 생성
# wisconsin$양성여부 <- as.character(wisconsin$양성여부)
tr_data2 <- createDataPartition(y=wisconsin2$양성여부,p=0.7,list=FALSE)
train_Bayes <- wisconsin2[tr_data2,]
test_Bayes <- wisconsin2[-tr_data2,]

# (3) 나이브 베이즈 모델 생성
model_Bayes <- naiveBayes(양성여부~.,data=train_Bayes)
model_Bayes

# (4) 예측값 생성
predicted <- predict(model_Bayes, test_Bayes, type = "class")
table(predicted,test_Bayes$양성여부)

positiveTest <- as.factor(test_Bayes$양성여부)
confusionMatrix(predicted,positiveTest)




# 1-3)의사결정트리 ####
# (1) 데이터 전처리
data1 <- read.csv('wdbc.csv',header = F)
wisconsin3 <- data1 %>% select(1:12,) %>% 
  `colnames<-`(c('id','양성여부','반경','질감','둘레','면적','매끄러움',
                 '조그만정도','오목함','오목점','대칭','프랙탈차원'))
# 자료형 변환
wisconsin3$양성여부 <- as.factor(wisconsin3$양성여부)

# (2) 분류모델 생성
model_tree <- ctree(양성여부 ~ .,data=wisconsin3)

tree <- rpart(양성여부 ~ ., data=wisconsin3)
summary(tree)

# (3) 분류분석 결과
plot(model_tree)
prp(tree, type=4, extra=2)

# (4) 교차타당성오차
tree$cptable
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt,"CP"]
prune.c <- prune(tree,cp=cp)
plotcp(tree)


# id변수가 빠진 트리와의 비교
wisconsin2 <- data1 %>% select(2:12,) %>% 
  `colnames<-`(c('양성여부','반경','질감','둘레','면적','매끄러움','조그만정도',
                 '오목함','오목점','대칭','프랙탈차원'))
wisconsin2$양성여부 <- as.factor(wisconsin2$양성여부)
model_tree2 <- ctree(양성여부 ~ .,data=wisconsin2)
plot(model_tree2)




# 1-4)인공신경망 ####
# (1) 데이터셋 생성
breastCancer <- read.csv('C:/Rwork/wdbc.csv', header = F); #데이터파일
names(breastCancer) <- c('IDNumber', 'Diagnosis', 'radius', 'texture', 'perimeter',
                         'area', 'smoothness', 'compactness', 'concavity', 'concavePoints',
                         'symmetry', 'fractalDimension')
breastCancer4 <- breastCancer[c(2:12)]

# (2) 범주형 변수를 수치형으로 변환
breastCancer4$Diagnosis[breastCancer4$Diagnosis == 'B'] <- 1
breastCancer4$Diagnosis[breastCancer4$Diagnosis == 'M'] <- 2
breastCancer4$Diagnosis <- as.integer(breastCancer4$Diagnosis)

# (3) 학습 데이터, 검정 데이터 생성
idx4 <- createDataPartition(breastCancer4$Diagnosis, p = 0.7, list = F)
training_breast4 = breastCancer4[idx4,] #위 index값 그대로 사용
testing_breast4 = breastCancer4[-idx4,]

# (4) 데이터 정규화
normal <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}
training_nor4 <- as.data.frame(lapply(training_breast4, normal))
testing_nor4 <- as.data.frame(lapply(testing_breast4, normal))

# (5) 인공신경망 모델 생성 
formula4 = Diagnosis~.
model_net1 = neuralnet(formula4, data = training_nor4, hidden = 1)
#plot(model_net1)
model_net3 = neuralnet(formula4, data = training_nor4, hidden = 3)
#plot(model_net3)
model_net4 = neuralnet(formula4, data = training_nor4, hidden = 4)
#plot(model_net4)

# (6) 분류모델 성능 평가
# compute()사용
model_result1 <- compute(model_net1, testing_nor4)
model_result3 <- compute(model_net3, testing_nor4)
model_result4 <- compute(model_net4, testing_nor4)
# 상관관계분석
cor(model_result1$net.result, testing_nor4$Diagnosis)
cor(model_result3$net.result, testing_nor4$Diagnosis)
cor(model_result4$net.result, testing_nor4$Diagnosis)




# 1-5)앙상블 기법-랜덤 포레스트 기법 ####
# (1) 데이터 전처리
breastCancer <- read.csv('C:/Rwork/wdbc.csv', header = F) #데이터파일
names(breastCancer) <- c('IDNumber', 'Diagnosis', 'radius', 'texture', 'perimeter',
                         'area', 'smoothness', 'compactness', 'concavity',
                         'concavePoints','symmetry', 'fractalDimension')
breastCancer.RF <- breastCancer[c(2:12)]
breastCancer.RF$Diagnosis <- as.factor(breastCancer.RF$Diagnosis) #종속변수 범주형 변환

# (2) 학습데이터, 검증데이터 생성
idx.RF <- createDataPartition(breastCancer.RF$Diagnosis, p = 0.7, list = F)
breastTrain <- breastCancer.RF[idx.RF,] #학습데이터
breastTest <- breastCancer.RF[-idx.RF,] #테스트데이터

# (3) 랜덤포레스트 모델 생성
formula1 = Diagnosis~.
rfbreast <- randomForest(formula1, data=breastTrain, ntree=100, proximity=TRUE) 
#proximity=TRUE 는 개체들 간의 근접도 행렬을 제공 : 동일한 최종노드에 포함되는 빈도에 기초함

# (4) 모델 성능 확인
table(predict(rfbreast), breastTrain$Diagnosis)
plot(rfbreast)

rf.predbreast <- predict(rfbreast, newdata = breastTest)
r2 = table(rf.predbreast, breastTest$Diagnosis);r2; (r2[1,1]+r2[2,2])/nrow(breastTest)




## 2번문제 ####################################################################
# Abalone Data 데이터셋을 대상으로 전복의 나이를 예측하고자 한다. 예측기법 2개를
# 적용하여 기법별 결과를 비교하시오
# 종속변수는 Rings를 사용




## 3번문제 ####################################################################
# iris 데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering을 실행하고
# 시각화 하시오.



