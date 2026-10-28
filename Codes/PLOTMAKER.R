##########################################
### PLOT MAKER : DATA ANALYSIS USING R ###
##########################################

#######################
### 02 INSTALLING R ###
#######################

3+(4*5)                # 숫자와 산술연산자(+, *)로 구성되어 간단한 계산을 수행하는 명령문
A <- 51:80             # 51-80 사이 자연수를 A에 저장 / [<-]는 오른쪽 내용을 왼쪽에 저장하라는 의미이다.
print(A)               # 저장된 내용을 화면에 출력 / print()와 같은 형식을 [함수]라고 부른다.


# 소스창의 실행버튼을 클릭하면 명령문이 한줄씩 순차적으로 실행된다.
# 잘못된 명령문을 실행하는 경우 error message와 함께 오류가 발생한 이유를 설명한다.


# 주석은 # 기호로 시작하는 명령문을 의미한다. R은 주석을 제외하고 명령문을 실행한다.
# 한 줄 내에서 [#] 이후의 내용은 주석문으로 간주하여 실행되지 않는다.


# Packages는 함수들을 기능별로 묶어놓은 일종의 꾸러미이다.
# 특정 패키지 안의 함수를 이용하기 위해서는 패키지 설치와 불러오기가 선행되어야 한다.

install.packages('ggplot2') # ggplot2 패키지 설치
library(ggplot2) # ggplot2 패키지 불러오기


# help() 괄호 안에 함수를 입력하고 실행하면 함수에 대한 설명을 Help Pane에 출력한다.
help(sort) # sort()라는 함수에 대한 설명 출력


##########################
### 03 DATA TYPES IN R ###
##########################

era <- c(5,4,3,4,5,6)      # c함수를 이용하여 숫자 5,4,3,4,5,6을 하나의 벡터로 연결
year <- 2001:2006          # colon(:) 기호는 단조증가 or 단조감소 형식의 연속된 값 생성 
game <- c(1,2,8,6,4,4)


sum(game)
era*game
era.all <- sum(game*era)/sum(game)
era.all

era[2]                     # era 벡터의 두 번째 성분 추출
era [2:3]                  # era 벡터의 두 번째 성분부터 세 번째 성분까지 추출
cbind(year, game, era)     # cbind()는 같은 길이의 벡터를 열에 넣어 행렬 생성


##########################
### 04 DATAFRAMES IN R ###
##########################

library(tidyverse)

students <- tibble(
  name = c("민수", "영희", "철수", "지수"),
  class = c(1, 1, 2, 2),
  score = c(80, 95, 70, 90)
)

# select() : 이름과 점수 열만 선택
select(students, name, score)

# filter() : 점수가 90점 이상인 학생만 선택
filter(students, score >= 90)

# arrange() : 점수 순으로 학생 정렬
arrange(students, score)              # 점수가 낮은 학생부터 높은 학생 순서로 정렬
arrange(students, desc(score))        # 점수가 높은 학생부터 낮은 학생 순서로 정렬

# mutate() : passed라는 새로운 열 추가(80점 이상이면 TRUE, 아니면 FALSE)
mutate(students, passed = score >= 80)


# %>% 연산 기초 : 80점 이상인 학생 선택 > 점수를 내림차순 정렬 > 이름과 점수만 출력
students %>% filter(score >= 80) %>% arrange(desc(score)) %>% select(name, score)

# 주요 변수에 대한 변수명 변경 : students의 변수명을 각각 A1, A2, A3로 변경
students2 <- students %>% rename(A1 = name, A2 = class, A3 = score)

# 데이터 가공 : A2, A3의 결측치 삭제
df <- students2 %>% select(A1, A2, A3) %>% filter(!is.na(A1), !is.na(A2))

# 데이터 가공 : 그 외 결측치를 0으로 치환 (NA -> 0)
df2 <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# 데이터 추가 : df2에 새로운 데이터 A4 = A3 * 0.4 추가
df2$A4 <- (df2$A3)*0.4


#############################
### 05 DATA PREPROCESSING ###
#############################

# 자료 불러오기 & 자료 파악하기
exams <- read.csv("/Users/jaemin/Desktop/RDATA/exams.csv", header = TRUE)
head(exams, 5)

# 기말고사 성적의 중앙값 계산 exams$final은 exams란 데이터셋의 final 열을 의미
median(exams$final)

# 결측치 확인 : is.na() : NA인 경우 T, NA가 아닌 경우 F를 반환하는 함수

# 기말고사 성적의 결측치를 0으로 치환
exams$final[is.na(exams$final)] <- 0
median(exams$final)


#############################
### 06 DATA VISUALIZATION ###
#############################

# DATA : exams2.txt (1차시험, 2차시험, 최종시험으로 구성된 데이터셋, 3차원 구조)
exams2 <- read.table("/Users/jaemin/Desktop/RDATA/exams2.txt", sep="\t", header = TRUE)
head(exams2, 5)

# F의 분포 : 히스토그램
hist(exams2$F, breaks = 10, main = "FINAL EXAM", col = "powderblue")

# F의 분포 : 상자그림 (사분위수)
q <- quantile(exams2$F, probs=c(0, 0.25, 0.50, 0.75, 1), na.rm=TRUE)
boxplot(exams2$F, main = "FINAL EXAM", col = "powderblue", horizontal=TRUE)
text(x = q, y = 1, labels=round(q,1))


# 두 변수 사이의 산점도
plot(exams2$P1, exams2$P2, main = "P1 P2 Scatter Plot", xlab="P1", ylab="P2", pch = 19)
plot(exams2$P1, exams2$F, main = "P1 F Scatter Plot", xlab="P1", ylab="P2", pch = 19)
plot(exams2$P2, exams2$F, main = "P2 F Scatter Plot", xlab="P1", ylab="P2", pch = 19)

# 산점도를 동시에 그리는 방법
pairs(exams2[, c("P1", "P2", "F")], main="SCATTER PLOTS", pch = 19, col = "steelblue")


# 최종성적 score열 추가 (P1 30% / P2 30% / F 40%)
exams2$score <- 0.3*exams2$P1 + 0.3*exams2$P2 + 0.4*exams2$F

# 커트라인 cuts 생성 (10% 34% 66% 90%)
cuts <- quantile(exams2$score, probs = c(0.90, 0.67, 0.34, 0.10), na.rm = TRUE)

# score 히스토그램 + 등급컷
hist(exams2$score, breaks = 4, main = "FINAL GRADE", col = "powderblue")
abline(v=cuts, col = "blue", lwd=2, lty=2)
axis(1, at=cuts, labels=round(cuts,1), col.axis = "blue")


#####################
### 07 COVARIANCE ###
#####################

# DATA : covariance.txt (Y, X1, X2, X3, X4, X5, X6으로 구성된 데이터셋, 7차원 구조)
df <- read.table("/Users/jaemin/Desktop/RDATA/covariance.txt", sep="\t", header = TRUE)

# Y, X1, X2, X3 사이 산점도
pairs(df[, c("Y", "X1", "X2", "X3")], main="SCATTER PLOT", pch = 19, col = "steelblue")

# Y와 X1 ~ X3 사이 공분산
cov(df$Y, df$X1)
cov(df$Y, df$X2)
cov(df$Y, df$X3)

# 데이터 전체 산점도 행렬
pairs(df[, c("Y", "X1", "X2", "X3", "X4", "X5", "X6")], pch = 19, col = "steelblue")


##################################
### 08 CORRELATION COEFFICIENT ###
##################################

# DATA : data1.txt, data2.txt, data3.txt, data4.txt (각각 2차원 구조)
# 4명의 사람(dino, away, star, bullseye)이 동일한 실험을 진행하고 자료를 수집한 것
dino <- read.table("/Users/jaemin/Downloads/RDATA/data1.txt", sep="\t", header = TRUE)
away <- read.table("/Users/jaemin/Downloads/RDATA/data2.txt", sep="\t", header = TRUE)
star <- read.table("/Users/jaemin/Downloads/RDATA/data3.txt", sep="\t", header = TRUE)
bull <- read.table("/Users/jaemin/Downloads/RDATA/data4.txt", sep="\t", header = TRUE)

head(dino, 5)

# 각 데이터셋의 평균, 분산 계산
mean(dino$x); sd(dino$x)    # 데이터셋 dino의 x열 평균 & 분산
mean(dino$y); sd(dino$y)    # 데이터셋 dino의 y열 평균 & 분산
cor(dino$x, dino$y)         # 데이터셋 dino의 두 변수 x와 y의 상관계수

# 같은 줄에 (;)으로 연결된 명령문은 서로 다른 명령문으로 인식한다. (줄바꿈 대신 사용)

plot(dino$x, dino$y, main = "Scatter Plot (1)", pch = 19)


###################################
### 10 SIMPLE LINEAR REGRESSION ###
###################################

# DATA : Computer.txt (수리해야 하는 부품수(Units)와 수리시간(Minutes)에 대한 데이터셋)
Computer <- read.table("/Users/jaemin/Desktop/RDATA/Computer.txt", sep="\t", header = TRUE)

# 산점도
plot(Minutes ~ Units, data = Computer, pch = 19)

# 공분산 & 상관계수
cov(Computer$Minutes, Computer$Units)
cor(Computer$Minutes, Computer$Units)

# GOAL : Minutes를 반응변수 Y, Units를 예측변수 X로 하여 선형회귀모델 도출

# 회귀계수 & 적합값 & 잔차
reg <- lm(Minutes ~ Units, data = Computer)
print(reg)
Y.hat <- fitted(reg)
e <- residuals(reg)

# 산점도 & 추세선(회귀선)
plot(Minutes ~ Units, data = Computer, xlab="UNITS", ylab="MINUTES", col="darkblue", pch = 19)
abline(reg, col="red")

# 회귀모형 : (Minutes) = beta0 + beta1*(Units) + epsilon
reg <- lm(Minutes ~ Units, data = Computer)
print(reg)


##########################
### 11 GOODNESS OF FIT ###
##########################

# summary(reg)에서 Multiple R-squared에 해당하는 값이 결정계수 R-Sqaure이다.
# R-sqaure가 1에 가까울수록 설명변수 X가 반응변수 Y의 변이 중 많은 부분을 설명한다.
# R-sqaure가 1에 가까울수록 설명변수 X와 반응변수 Y 사이에 강한 선형관계를 가진다.


##########################
### 13 HYPOTHESIS TEST ###
##########################

# DATA : Computer.txt (수리해야 하는 부품수(Units)와 수리시간(Minutes)에 대한 데이터셋)
Computer <- read.table("/Users/jaemin/Desktop/RDATA/Computer.txt", sep="\t", header = TRUE)
reg <- lm(Minutes ~ Units, data = Computer)

# Units 계수 / 검정통계량 t1
coef <- coef(summary(reg))
t1 <- (coef[2,1])/coef[2,2]

# 임계값 / p-value 계산 / 자유도(데이터수 – 모수 개수) 14-2
alpha = 0.05                                      # 유의수준 alpha
qt(alpha/2, 14-2, lower.tail = FALSE)             # 임계값
p.v <- 2 * pt(abs(t1), 14-2, lower.tail = FALSE)  # p-value

# 유의수준 0.05에서 가설검정 (p-value와 0.05 비교)
if (p.v < 0.05) {cat("귀무가설 기각. 기울기가 통계적으로 유의")} else {cat("귀무가설 기각 불가")}


# 고장난 부품을 하나 더 수리하는데 12분 걸릴 것으로 기대한다고 가정
# 귀무가설 H0 : beta1 = 12
# 대립가설 H0 : beta1 ≠ 12

# Units 계수 / 검정통계량 t1
coef <- coef(summary(reg))
t1 <- (coef[2,1]-12)/coef[2,2]

# c-value / p-value 계산
df <- reg$df.residual
c.value <- qt(0.05/2, df, lower.tail = FALSE)  # c-value (임계값)
p.value <- 2*pt(t1, df, lower.tail = FALSE)    # p-value

if (t1 > c.value) {cat("귀무가설 기각. 대립가설 채택")} else {cat("귀무가설 기각 불가")}


###########################
### 14 CONFIDENCE LEVEL ###
###########################

# 신뢰구간 계산
confint(reg, level=0.95)

# 예측한계 & 신뢰한계
predict(reg, interval='prediction', level=0.95)       # 예측한계 prediction limit
predict(reg, interval='confidence', level=0.95)       # 신뢰한계 confidence limit

# 4행, 10행, 11행 데이터에 대한 신뢰한계와 예측한계
test <- data.frame(Units=c(4,10,11))
predict(reg, newdata = test, interval='prediction', level=0.95)
predict(reg, newdata = test, interval='confidence', level=0.95)


#################################
### 15 REGRESSION DIAGNOSTICS ###
#################################

# 일차원 그래프 : 개별 데이터 분포 파악
Hamilton <- read.table("/Users/jaemin/Downloads/RDATA/Hamilton.Data.txt", sep="\t", header = TRUE)
reg <- lm(Y ~ X1 + X2, data = Hamilton)
Y.hat <- fitted(reg)
e <- residuals(reg)
h <- hatvalues(reg)
r <- rstandard(reg)
rs <- rstudent(reg)
resid <- data.frame(Hamilton, Y.hat, e, h, r, rs)

if(!require("DescTools")) install.packages("DescTools"); library(DescTools)
Desc(Hamilton[c("Y", "X1", "X2")], plotit = TRUE)

plot(Hamilton[c("Y", "X1", "X2")], pch = 19, cex = 1)


# 이차원 그래프 : 데이터 사이 관계 파악
if(!require("corrplot")) install.packages("corrplot"); library(corrplot)

# 상관계수 표
cor <- cor(Hamilton[c("Y", "X1", "X2")])
round(cor, digits = 3)

# 상관계수 시각화
corrplot(cor, method = "number", numer.digits = 3)
corrplot(cor, method = "ellipse", diag = TRUE, type = "full")


# (1) 표준화잔차의 정규확률플롯
qqnorm(resid$r, ylim = c(-3,3))
qqline(resid$r, lwd = 1, col = "red")

# (2) 표준화잔차 대 각 예측변수들의 산점도
plot(r ~ X1, data = resid, pch = 19); abline(h = c(--2,0,2), lty=2)
plot(r ~ X2, data = resid, pch = 19); abline(h = c(-2,0,2), lty=2)

# (3)표준화잔차 대 적합값의 플롯
plot(r ~ Y.hat, data = resid, pch = 19); abline(h = c(-2,0,2), lty=2)

# (4) 표준화잔차의 인덱스플롯
plot(row.names(resid), resid$r, pch = 19); abline(h = c(-2,0,2), lty=2)
