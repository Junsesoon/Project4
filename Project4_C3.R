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




## 2번문제 ####################################################################
# Abalone Data 데이터셋을 대상으로 전복의 나이를 예측하고자 한다. 예측기법 2개를
# 적용하여 기법별 결과를 비교하시오
# 종속변수는 Rings를 사용




## 3번문제 ####################################################################
# iris 데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering을 실행하고
# 시각화 하시오.



