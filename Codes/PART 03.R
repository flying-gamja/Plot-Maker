###################################################
### PLOT MAKER : DATA ANALYSIS USING R (PART 3) ###
###################################################

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