###################################################
### PLOT MAKER : DATA ANALYSIS USING R (PART 2) ###
###################################################

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