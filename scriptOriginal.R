
############################################################################################################
# DESCRIPTION : the following are the R codes used for the numerical part of the article. Some R packages  #
#               have been used to achieve the programme. To check the results we got in our article, one   #
#               need to install and load those package in the R software. Then the code should run succes- #
#               fully. Therefore, inside the code, when a package is load it's needed to install that      #
#               package on your R software.#
############################################################################################################
# The data
rm(list = ls())

#
# Auxiliary function: Generating the matrix of explanatory variables
# n is the number of coefficients
#

k <- 5

Xmat <- function(n){
  # Matrix that will merge all the Y and positions of i and j of th
  # UTM
  M <- matrix(0, nrow = n, ncol = n)


  # Code that select the indices of the upper triangular (UTM) matrix
  for (i in 1:n) {
    for (j in 1:n) {
      if (i + j <= (n+1)){
        M[i,j] <- paste("Y", i,j, sep = "_")
      } else {
        break
      }
    }
  }

  # Vector of indices
  # We will use them to extract the positions of i and j
  A <- as.character(t(M))
  A <-  A[A!="0"]

  # Use A to create the matrix X

  n_col <- length(c(1, rep(0,2*(n-1))))

  pos_i <- c(1, seq(2, n_col, by = 2))
  pos_j <- seq(1, n_col, by = 2)

  X <- matrix(0, nrow = length(A), ncol = n_col)
  X[,1] <- 1
  rownames(X) <- A # Can be omitted

  for (k in 1:length(A)) {
    i <- as.numeric(strsplit(A[k], split = "_")[[1]][2])
    j <- as.numeric(strsplit(A[k], split = "_")[[1]][3])

    X[k, ][pos_i[i]] <- 1
    X[k, ][pos_j[j]] <- 1
  }

  #
File: /home/woundjiague/Mes_Donnes/.../Code R Datat 1/RcodeArticle.RPage 2 of 4
  return(X)

}

# -------------------------------------------------------------------------------
#                         Classical Log-Poisson model                           #
# -------------------------------------------------------------------------------
k = 5
n <- k*(k+1)/2
Triangle <- data.frame(originf = as.factor(rep(1:k, k:1)),
                       devf=as.factor(sequence(k:1)),
                       inc.paid=
c(1120,2090,2610,2920,3130,1030,1920,2370,2710,1090,2140,2610,1300,2650,1420))

# The Poisson model

RegPoisson <- glm(inc.paid ~ originf + devf, data=Triangle, family=poisson(link =
"log"))
summary(RegPoisson)

# Prediction of the incremental claims payments

allClaims <- data.frame(origin = sort(rep(1:k, k)), dev = rep(1:k,k))
allClaims <- within(allClaims, {
  devf <- as.factor(dev)
  cal <- origin + dev - 1
  originf <- factor(origin)
})
(pred.inc.tri <- t(matrix(predict(RegPoisson,type="response", newdata=allClaims),
k, k)))

# The total Loss Reserve

sum(predict(RegPoisson,type="response", newdata=subset(allClaims, cal > k)))

# MSE from the package
#install.packages("Metrics")
library(Metrics)
pred.inc <- c(pred.inc.tri[1,], pred.inc.tri[2,1:4], pred.inc.tri[3, 1:3],
pred.inc.tri[4, 1:2], pred.inc.tri[5,1])
mse(Triangle$inc.paid, pred.inc)
# -------------------------------------------------------------------------------
#                               Overdispersion test                             #
# -------------------------------------------------------------------------------

library(AER)
if (!require(AER)) install.packages("AER")

dispersiontest(RegPoisson)

# -------------------------------------------------------------------------------

# --------------------------------------------------------------- 8/9/2018
----------------------------
#
library(dplyr)
inc.paid1= c(1120,2090,2610,2920,3130,1030,1920,2370,2710, NA, 1090,2140,2610, NA,
NA, 1300,2650, NA, NA, NA,
            1420, NA, NA, NA, NA)

f <- function(k, values = inc.paid){ #
  # Matrix that will merge all the Y and positions of i and j of th
  # UTM
  M <- matrix(0, nrow = k, ncol = k)
