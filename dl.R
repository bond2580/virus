library(dlm)

R0 <- ans_xts$R0
e <- ans_xts$e
l <- ans_xts$l

#状態空間モデルの設定
build_dlm_R0a<- function(par){
  return(
    dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) + 
    dlmModSeas(frequency = 84, dW = c(exp(par[4]), rep(0, times=82)), dV = 0)
  )
}
#周期性+ローカルレベルモデル
build_dlm_R0b <- function(par){
  return(
    dlmModPoly(order = 1, dW = exp(par[1]), dV = exp(par[2])) + 
    dlmModSeas(frequency = 84, dW = c(exp(par[3]), rep(0, times=82)), dV = 0)
  )
}
build_dlm_R0c <- function(par){
  return(
    dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) + 
    dlmModTrig(s = 84, q=2, dW = exp(par[4]), dV = 0)
  )
}

#最尤推定
fit_dlm_R0a <- dlmMLE(y = R0, parm = rep(0, 4), build = build_dlm_R0b, method="CG")
fit_dlm_ea <- dlmMLE(y = e, parm = rep(0, 4), build = build_dlm_R0b, method="CG")
fit_dlm_la <- dlmMLE(y = l, parm = rep(0, 4), build = build_dlm_R0b, method="CG")
fit_dlm_R0a

#最尤推定結果をモデルに指定
mod_R0 <- build_dlm_R0a(fit_dlm_R0$par)
mod_e <- build_dlm_ea(fit_dlm_ea$par)
mod_l <- build_dlm_la(fit_dlm_R0$par)

#過去の最適パラメータのカルマンフィルタリング
dlmFiltered_obj_R0 <- dlmFilter(y = R0, mod=mod_R0)
dlmFiltered_obj_e <- dlmFilter(y = e, mod=mod_e)
dlmFiltered_obj_l <- dlmFilter(y = l, mod=mod_R0)

#未来の最適パラメータのカルマン予測
dlmForcast_R0 <- dlmForecast(mod = dlmFiltered_obj_R0, n.ahead = span+1)
dlmForcast_e <- dlmForecast(mod = dlmFiltered_obj_e, n.ahead = span+1)
dlmForcast_l <- dlmForecast(mod = dlmFiltered_obj_l, n.ahead = span+1)

#dlmFiltered_obja <- dlmFiltered_obj

#mu <- dropFirst(dlmFiltered_obj$m[, 1])
#gammma <- dropFirst(dlmFiltered_obj$m[, 3])

#oldpar <- par(no.readonly = TRUE)
#par(mfrow = c(3,1)): par(oma=c(2,0,0,0));par(mar=c(2,4,1,1))
#plot(R0, ylab="measured")
#plot(mu, ylab="level")
#plot(gammma, ylab="season")
#par(oldpar)


#