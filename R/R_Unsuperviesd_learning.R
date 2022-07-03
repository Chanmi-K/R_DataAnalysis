getwd()

##비지도 학습

#k 평균 군집 분석------------
iris2 = iris
iris2$Species = NULL  #분석 목적상 species 불필요
head(iris2)

kmeans_result = kmeans(iris2, 3) #클러스터 3개
kmeans_result

plot(iris2[c('Sepal.Length','Sepal.Width')], col=kmeans_result$cluster)
points(kmeans_result$centers[, c('Sepal.Length', 'Sepal.Width')], col=1:3, pch=8, cex=2) #평균값 추가


#계층적 군집 분석------------
idx = sample(1:dim(iris)[1], 40)
idx
irisSample = iris[idx,]
head(irisSample)

irisSample$Species = NULL
head(irisSample)

hc_result = hclust(dist(irisSample), method='average')  #평균연결법 사용
hc_result

plot(hc_result, hang=-1, labels=iris$Species[idx])
rect.hclust(hc_result, k=3) #그룹으로 나누어 표시


#주성분 분석------------
cor(iris[1:4])
iris2 = iris[,1:4]
ir.species = iris[,5]

prcomp.result = prcomp(iris2, center=T, scale=T)  #주성분 분석 수행. 중앙 0, 분산 1
prcomp.result

summary(prcomp.result)

prcomp.result$rotation #계수값

NewResult = as.matrix(iris2) %*% prcomp.result$rotation   #행렬곱
head(NewResult)

final = cbind(ir.species, as.data.frame(NewResult))
final[,1] = as.factor(final[,1])
colnames(final)[1] = 'label'
head(final)

fit = lm(label ~ PC1 + PC2, data=final)
fit   # y = 0.5373 + 0.4289PC1 + 0.1376PC2

fit_pred = predict(fit, newdata=final)
b = round(fit_pred)
a = ir.species
table(b,a)

#주성분 분석2
library(datasets)
data('USArrests')
summary(USArrests)
fit = princomp(USArrests, cor=TRUE)
summary(fit)
loadings(fit) #계수값 확인인
plot(fit, type='lines') #스크리 plot
fit$scores  #각 관측치를 주성분들로 표현현
biplot(fit) #관측치들을 두 개의 주성분 좌표에 표시. 주성분과 자료와의 관계파악악


#인자 분석------------
FactorData = read.table("FactorData.txt", header=T)
head(FactorData)

install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)

FactorResult = principal(FactorData, rotate="none") #주성분 인자법
FactorResult

FactorResult$values #고유값
names(FactorResult)
plot(FactorResult$values, type='b')

FactorVariable.varimax = principal(FactorData, nfactors=3, rotate='varimax')
FactorVariable.varimax


Factor.oblimin = factanal(FactorData, factors=4, rotation='oblimin')  #최우 추정법
Factor.oblimin


#다차원 척도법--------------
library(MASS)
data('eurodist')
eurodist #유럽 도시 사이 거리 데이터 

MDSEurodist = cmdscale(eurodist)  #계량적 MDS
MDSEurodist

plot(MDSEurodist)
text(MDSEurodist, rownames(MDSEurodist), cex=0.7, col='red')
abline(v=0, h=0, lty=1, lwd=0.5)


install.packages("HSAUR")
library(HSAUR)
data('voting', package="HSAUR")
library(MASS)
voting

MDSvoting = isoMDS(voting)  #비계량적 MDS
MDSvoting

x = MDSvoting$points[,1]
y = MDSvoting$points[,2]
plot(x,y)
text(x,y,labels=colnames(voting))


#판별 분석------------
install.packages("caTools")
library(caTools)

set.seed(1300)
split = sample.split(iris$Species, SplitRatio = .7)
train = subset(iris, split==T)
test = subset(iris, split==F)
test.y = test[,5]
library(MASS)

iris.lda = lda(Species~., data=train, prior=c(1/3,1/3,1/3))
iris.lda

plot(iris.lda)

testpred = predict(iris.lda, test)
table(test.y, testpred$class)


#이차 판별 분석---------
install.packages('biotools')
library(biotools)
boxM(iris[1:4],iris$Species)  #집단간 분산-공분산 행렬의 동일 여부 확인

iris.qda = qda(Species~., data=train, prior=c(1/3,1/3,1/3))
iris.qda

testqda = predict(iris.qda, test)
table(test.y, testqda$class)


#경로 분석----------------

install.packages("lavaan")
install.packages("semPlot")
library(lavaan)

pathData = read.csv('PathAnalysis.csv')
pathData

model = 'y1~x1+x2+x3
y2~x1+x2+x3'

result = sem(model, data=pathData)
summary(result)

semPaths(result, whatLabels='std', style='lisrel', nDigits=2, layout='tree', edge.label.cex=0.5,
         label.cex=1.0, sizeMan=3, sizeLat=8)