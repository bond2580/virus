#未来の最適パラメータを予測してSEIRモデルで未来の流行を計算する



tmp <- as.xts(ans[,c(-1, -2)], order.by = dates)
attr(tmp, 'frequency') <- 7

#ホルトウィンタース法にによるパラメータ推測モデル
HR0 <- HoltWinters(ans_xts$R0,
                   gamma=FALSE
                   ) #周期性が確認できなかったため, gammma=FALSEとする. 
He <- HoltWinters(tmp$e)
Hl <- HoltWinters(tmp$l)

#12月28時点の最適パラメータを推測
HR0 <- predict(HR0, n.ahead=span+60)
He <- predict(He, n.ahead=span+60)
Hl <- predict(Hl, n.ahead=span+1)


new <- FCUK(covid$S[end+span], covid$E[end+span], covid$I[end+span], covid$R[end+span], HR0[8],  He[8], Hl[8],span) #予測値
valid = covid[(end+span):(end+span+span),] #実測値

err_S <- abs(valid$S - new[,2])  #Susupectiveの誤差
err_R <- abs(valid$R - new[,5]) #Recoverdの誤差

dates2 <- seq(as.Date("2021-3-01"), length=nrow(valid), by="days")

png("predict.png", width = 959, height=610) #予測値と実測値, 誤差をプロット
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2),par(mar=c(0,0,0,0)),par(mar=c(4,4,3,1)))

plot(dates2, valid$S, main="Suspective", xlab="Month/Day", ylab="Population",type="l",lwd=2.0)
grid(col="black")
points(dates2, new[,2], main="Suspective", xlab="Month/Day", ylab="Population",type="l",lwd=2.0, col="red")
legend("bottomleft", legend=c("predicted", "measured"), col=c("red", "black"), lty=1, cex=1.5)

plot(dates2, err_S, main="Error of Suspective",xlab="Months/day", ylab="Population",col="blue", type="l", lwd=2.0)
grid(col="black")

plot(dates2, valid$R, main="Recovered", xlab="Month/Day", ylab="Population",type="l",lwd=2.0)
grid(col="black")
points(dates2, new[,5], main="Recovered", xlab="Month/Day", ylab="Population",type="l",lwd=2.0, col="red")
legend("topleft", legend=c("predicted", "measured"), col=c("red", "black"), lty=1, cex=1.5)

plot(dates2, err_R, main="Error of Recovered",xlab="Months/day", ylab="Population",col="blue", type="l", lwd=2.0)
grid(col="black")

par(oldpar)
dev.off()