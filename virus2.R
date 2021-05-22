#未来の最適パラメータを予測してSEIRモデルで未来の流行を計算する


span2 <- 30#未来の流行を何日間予測するか

tmp <- as.xts(ans[,c(-1)], order.by = dates)
attr(tmp, 'frequency') <- 84

#ホルトウィンタース法にによるパラメータ推測
source("HW.R")
#カルマンフィルタによる未来の最適パラメータ推測
source("dl.R")
#

new <- FCUK(covid$S[end+span+1], covid$E[end+span+1], covid$I[end+span+1], covid$R[end+span+1],
            HR0[span+1],  He[span+1], Hl[span+1],span2) #予測値

new <- FCUK(covid$S[end+span+1], covid$E[end+span+1], covid$I[end+span+1], covid$R[end+span+1],
            dlmForcast_R0[span+1], dlmForcast_e[span+1], dlmForcast_l[span+1])#予測値

valid <- covid[(end+span):(end+span+span2),] #実測値

err_S <- sqrt((valid$S - new[,2])**2)  #Susupectiveの誤差
err_R <- sqrt((valid$R - new[,5])**2) #Recoverdの誤差

dates2 <- seq(as.Date("2021-04-07"), length=nrow(valid), by="days")

#予測値と実測値, 誤差をプロット
png("predict7d.png", width = 959, height=610) 
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2),par(mar=c(0,0,0,0)),par(mar=c(4,4,3,1)))

plot(dates2, valid$S, main="Suspective", xlab="Month/Day", ylab="Population",type="l",lwd=2.0)
points(dates2, new[,2], main="Suspective", xlab="Month/Day", ylab="Population",type="l",lwd=2.0, col="red")
legend("bottomleft", legend=c("predicted", "measured"), col=c("red", "black"), lty=1, cex=1.5)
text(18709, 119610000, paste("R^2=", as.character(round(R2(valid$S, new[,2]), 4))), cex=3.0)

plot(dates2, err_S, main="Error of Suspective",xlab="Months/day", ylab="Population",ylim=c(0, 1e+05),col="blue", type="l", lwd=2.0)
grid(col="black")

plot(dates2, valid$R, main="Recovered", xlab="Month/Day", ylab="Population",type="l",lwd=2.0)
grid(col="black")
points(dates2, new[,5], main="Recovered", xlab="Month/Day", ylab="Population",type="l",lwd=2.0, col="red")
legend("topleft", legend=c("predicted", "measured"), col=c("red", "black"), lty=1, cex=1.5)
text(18709, 420000, paste("R^2=", as.character(round(R2(valid$R, new[,5]), 4))), cex=3.0)

plot(dates2, err_R, main="Error of Recovered",xlab="Months/day", ylab="Population",ylim=c(0, 35000),col="blue", type="l", lwd=2.0)
grid(col="black")

par(oldpar)
dev.off()



text(18709, 119610000, paste("R^2=", as.character(round(R2(valid$S, new[,2]), 4))), cex=4.0)