#iris 데이터 파악
head(iris)
tail(iris)
summary(iris)
str(iris)

#01.칼럼 사이 연관관계 파악
plot(iris)

#02.특정 칼럼 정보 파악
plot(iris$Sepal.Length)

#03.칼럼 사이 연관관계 분석
Sepal_Length = iris[,1]
Petal_Length = iris[,3]
temp = cbind(Sepal_Length, Petal_Length)
boxplot(temp)

#04.종류별 분포 파악
plot(iris$Sepal.Length, iris$Sepal.Width, pch=as.numeric(iris$Species))

#05.별도 패키지로 탐색
install.packages('caret')
library(caret)
featurePlot(iris[,1:4], iris$Species)

#------------------
#데이터 시각화
#데이터 : Employee
getwd()
setwd('C:/Users/Chanmi/Desktop/data_R')
employee = read.csv('employees_kr.csv')
head(employee)

sub2008 = subset(employee, employee$Year == 2008)
head(sub2008)
hist(sub2008$Incentive) #히스토그램

subMan = subset(employee, employee$Sex == 'M')
hist(subMan$Incentive)

#그래프 나누기
split.screen(c(2,1))
screen(1)
plot(1:10)
screen(2)
plot(10:1)

#그래프 나누기 다른 방법
par(mfrow = c(2,1))
plot(1:10)
plot(10:1)