# case control study  
# the data in 2*2 table --  saturated model
# log-linear model--------------------------------------------------------------------------------------
y <- c(688,21,650,59)
lambda <- mean(y)
# with intersection

# this is design matrix
x <- matrix(c(1,0,0,0,
              1,1,0,0,
              1,0,1,0,
              1,1,1,1),nrow = 4,byrow = TRUE)

m1 <- glm(y ~ (x-1), family="poisson")
summary(m1)
install.packages("stats")
library(stats)
nifisher1 <- vcov(m1)
fisher1 <- solve(nifisher1)
require(Matrix)
# rank of fisher's information matrix
rankMatrix(fisher1)[1]
# determinant
det(fisher1)
# eigen values
eigen(fisher1)$values
#---------------------------------------------------------------------------------------------------
# # independent model

y_23 <- c(688,21,0,59)
x_23 <- matrix(c(1,0,0,
                 1,1,0,
                 1,0,1,
                 1,1,1),nrow = 4,byrow = TRUE)

m2_3 <- glm(y_23 ~ (x_23-1), family="poisson")
summary(m2_3)
library(stats)
nifisher2_3 <- vcov(m2_3)
fisher2_3 <- solve(nifisher2_3)
require(Matrix)
# rank of fisher's information matrix
rankMatrix(fisher2_3)[1]
# determinant
det(fisher2_3)
# eigen values
eigen(fisher2_3)$values

# -------------------------------------------------------------------------------------
# logistic regression model
y <- c(688,21,650,59)
smokem2 <- matrix(c(688,650,21,59),ncol=2,byrow=TRUE)
dimnames(smokem2) <- list(c("yes","no"),c(0,1))
names(dimnames(smokem2)) <- c("X","Y")

smokem2 <- as.table(smokem2)
DET <- as.data.frame(smokem2)
DET

fit <- glm(Y ~ X, weights = Freq , data=DET,family = binomial(link="logit"))
summary(fit)

library(stats)
nifisher1 <- vcov(fit)
fisher1 <- solve(nifisher1)
require(Matrix)
# rank of fisher's information matrix
rankMatrix(fisher1)[1]
# determinant
det(fisher1)
# eigen values
eigen(fisher1)$values


# Three way contingency table
#-----------------------------------------------------------------------------------------------------
# log-linear model
# this is not the saturated model.it has no three-order interaction.
y <- c(14,32,93,81,11,12,52,43)
# this is design matrix
x2 <- matrix(c(1,0,0,0,0,0,0,
               1,0,1,0,0,0,0,
               1,0,0,1,0,0,0,
               1,0,1,1,0,0,1,
               1,1,0,0,0,0,0,
               1,1,1,0,1,0,0,
               1,1,0,1,0,1,0,
               1,1,1,1,1,1,1),nrow = 8,byrow = TRUE)

m2 <- glm(y ~ (x2-1), family="poisson")
summary(m2)
library(stats)
nifisher1 <- vcov(m2)
fisher1 <- solve(nifisher1)
require(Matrix)
# rank of fisher's information matrix
rankMatrix(fisher1)[1]
# determinant
det(fisher1)
# eigen values
eigen(fisher1)$values


#-----------------------------------------------------------------------------------------------------
# this is the saturated model

y <- c(14,32,93,81,11,12,52,43)
# this is design matrix
x <- matrix(c(1,0,0,0,0,0,0,0,
              1,0,1,0,0,0,0,0,
              1,0,0,1,0,0,0,0,
              1,0,1,1,0,0,1,0,
              1,1,0,0,0,0,0,0,
              1,1,1,0,1,0,0,0,
              1,1,0,1,0,1,0,0,
              1,1,1,1,1,1,1,1),nrow = 8,byrow = TRUE)

m1 <- glm(y ~ (x-1), family="poisson")
summary(m1)
library(stats)
nifisher1 <- vcov(m1)
fisher1 <- solve(nifisher1)
require(Matrix)
# rank of fisher's information matrix
rankMatrix(fisher1)[1]
# determinant
det(fisher1)
# eigen values
eigen(fisher1)$values


# logistic regression(saturated model)
#-----------------------------------------------------------------------------------------------------

AZT <- array(data = c(14,32,93,81,11,12,0,0),
             dim = c(2,2,2),
             dimnames = list("AZT" = c("Yes","No"),
                             "Symptoms"  = c("Yes","No"),
                             "Race"= c("White","Black")))
AZT.df <- as.data.frame(as.table(AZT))
fit <- glm(Symptoms ~ Race * AZT, weights = Freq , data=AZT.df,family = binomial(link="logit"))
summary(fit)
#-----------------------------------------------------------------------------------------------------
# logistic regression(not saturated model)

AZT <- array(data = c(14,32,93,81,11,0,0,43),
             dim = c(2,2,2),
             dimnames = list("AZT" = c("Yes","No"),
                             "Symptoms"  = c("Yes","No"),
                             "Race"= c("White","Black")))
AZT.df <- as.data.frame(as.table(AZT))
fit <- glm(Symptoms ~ Race + AZT, weights = Freq , data=AZT.df,family = binomial(link="logit"))
summary(fit)
library(stats)
nifisher1 <- vcov(fit)
fisher1 <- solve(nifisher1)
require(Matrix)
# rank of fisher's information matrix
rankMatrix(fisher1)[1]
# determinant
det(fisher1)
# eigen values
eigen(fisher1)$values


# Five way contingency table from Brown's paper
#-----------------------------------------------------------------------------------------------------
# log-linear model
data123 <- array(data = c(33,32,8,8,0,1,1,0,0,1,0,0,0,1,0,0,2,10,3,6,1,2,0,2,0,1,0,4,0,1,0,2),
                 dim = c(2,2,2,2,2),
                 dimnames = list("E" = c("0","1"),
                                 "N"= c("0","1"),
                                 "M"= c("0","1"),
                                 "B"= c("0","1"),
                                 "D"= c("0","1")))  
ftable(data123)
data123.df <- as.data.frame(as.table(data123))

model0 <- glm(Freq ~ (D*E*B)+(D*N)+(D*M)+(E*N*M*B) ,
              data = data123.df,family = poisson)
summary(model0)



# logistic regression model
#-----------------------------------------------------------------------------------------------------


data123 <- array(data = c(33,32,8,8,0,1,1,0,0,1,0,0,0,1,0,0,2,10,3,6,1,2,0,2,0,1,0,4,0,1,0,2),
                 dim = c(2,2,2,2,2),
                 dimnames = list("E" = c("0","1"),
                                 "N"= c("0","1"),
                                 "M"= c("0","1"),
                                 "B"= c("0","1"),
                                 "D"= c("0","1")))  
ftable(data123)
data123.df <- as.data.frame(as.table(data123))
fit5 <- glm(D ~ (E*B) +N + M, weights = Freq , data=data123.df,family = binomial(link="logit"))
summary(fit5)

