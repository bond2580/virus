
library(deSolve)

FCUK <- function(S, E, I, R, R0, meanl, meani, span){ #SEIRモデルによる計算
  
  R_t <- R0
  population <- 120000000
  
  SEIR <- function(t, state, parameters){
    with(as.list(c(state, parameters)),{
      dS <- -Rt / meani * S / population * I
      dE <- Rt / meani * S / population * I - E / meanl
      dI <- E / meanl - I / meani
      dR <- I / meani
      list(c(dS, dE, dI, dR))
    })
  }
  
  parameters <- c(Rt = R_t)
  times <- seq(0,span, 1)
  initial <- c(S=S, E=E, I=I, R=R)
  out2 <- ode(y = initial, times = times, func = SEIR, parms = parameters)
  returnValue(out2)
}

zansa <- function(X, x){#残差を求める
  s <-sqrt( sum( (X - x) * (X - x) / length(X)) )
  returnValue(s)
}

R2 <- function(X,x){#決定係数を求める
  r2 <- 1 - (sum((X - x) * (X - x) ) / sum((X - mean(X)) * (X - mean(X)) ))
  returnValue(r2) 
}



simS3 <- function(start, end){#Rの残差の最も小さくなるパラメータを求める
  sbest <- -100000000
  #ans <- array(0, dim = c(1, 6))
  ans <- data.frame()
  for(i in  1:30){
    for(j in 5:10){
      for(k in 6:14){
        b <- FCUK(covid$S[start], covid$E[start], covid$I[start], covid$R[start], i/10,  j, k,(end-start+1))
        s <- R2(covid$S[start:end], b[1:(end-start+1),2])
        r <- R2(covid$R[start:end], b[1:(end-start+1),5])
        if(s+r >= sbest){
          sbest <- s+r
          ans <-data.frame(covid$日付[start], i/10, j, k, s, r, sbest)
          colnames(ans) <- c("days","R0", "e", "l", "SR2", "RR2", "S+R")
        }
      }
    }
  }
  returnValue(ans)
}


sim <- function(start, end, span){　#最適パラメータの推測
  opt <- data.frame()
  for(i in start:end){
    opt <- rbind(opt, simS3(i, i+span))
  }
  
  returnValue(opt)
}



