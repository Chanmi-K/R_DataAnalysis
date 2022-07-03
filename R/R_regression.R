getwd()

##회귀----------

salary = read.csv('regdata.csv')
head(salary)

#협상을 했을 경우 급여 인상률과 받은 인센티브 인상률에 대한 그래프 확인
plot(salary$Incentive[salary$negotiated==TRUE], salary$Salary[salary$negotiated==TRUE])
#급여 인상률이 10% 이하면 인센티브 인상률과 밀접한 관계 확인

#상관관계 확인
cor(salary$Incentive[salary$negotiated==TRUE], salary$Salary[salary$negotiated==TRUE])
#0.6656481

#선형 회귀
RegResult = lm(Incentive[negotiated==TRUE]~Salary[negotiated==TRUE], data=salary)
RegResult
# y = 2.3121 + 0.7251
summary(RegResult)


##중선형 회귀----------
head(attitude)  #내장 데이터 attitude 사용

model = lm(rating~.,data=attitude)
summary(model)

reduced = step(model, direction='backward')
summary(reduced)


##로지스틱 회귀----------
data = read.csv('binary.csv')
str(data)
head(data)
trainData = data[1:200,]
testData = data[201:400,]
model = glm(admit ~ gre+gpa+rank, data=trainData, family='binomial')
summary(model)

predictData = predict(model, newdata=testData, type='response')

head(predictData)
round(predictData)
table(round(predictData), testData$admit)