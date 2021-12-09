## Project4 C3 ################################################################
# 기간: 21.11.22(월)까지
# C3조: 오준서 천성한 한호종 황윤수

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("caret") #데이터파티션()
install.packages("NbClust") #군집분석
install.packages("nnet")
install.packages("neuralnet") #인공신경망
install.packages('rpart') #의사결정트리
install.packages("rpart.plot") #트리 시각화
install.packages('randomForest') #랜덤포레스트()

library(caret)
library(NbClust)
library(nnet)
library(neuralnet)
library(rpart)
library(rpart.plot)
library(randomForest)


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
# Abalone Data 데이터셋을 대상으로 전복의 나이를 예측하고자 한다. 
# 예측기법 2개를 적용하여 기법별 결과를 비교하시오.(종속변수는 Rings를 사용)

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
  #proximity=TRUE: 개체들 간의 근접도 행렬을 제공
    #             (=동일한 최종노드에 포함되는 빈도에 기초함)
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



