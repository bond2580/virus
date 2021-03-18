#SEIRモデルの過去の最適パラメータを推測し
#それらの周波数スペクトルから周期性ｗ確認する

library(xts)
library(deSolve)

start <- 149 #2020年7月1日
end <- 321 #2020年12月20日

covid <- read.csv("COVID-19 (6).csv")
ans <- sim(start, end, 7) #過去の最適パラメータを予測
dates <- seq(as.Date("2020-07-01"), length=nrow(ans), by="days")
ans_xts <- as.xts(ans[, -1], order.by = dates)

png("parameter.png", width = 959, height = 605) #過去の最適パラメータをプロット
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2),par(mar=c(0,0,0,0)),par(mar=c(4,4,3,1))) #プロットを四分割
plot(ans_xts$R0, main="R0",ylim=c(0, 3.5),xlab="days",col="red", lwd=2.0)
plot(ans_xts$e, main="e",ylim=c(4, 14),xlab="days",col="darkgreen", lwd=2.0)
plot(ans_xts$l, main="l",ylim=c(6, 16),xlab="days",col="blue", lwd=2.0)
par(oldpar)
dev.off()


png("spectrum.png", width = 959, height = 605) #過去の最適パラメータの周波数スペクトルをプロット
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2),par(mar=c(0,0,0,0)),par(mar=c(4,4,3,1)))
plot.spectrum(ans_xts$R0, lab="日", main="R0の周波数スペクトル", col="red")
plot.spectrum(ans_xts$e, lab="日", main="eの周波数スペクトル", col="green")
plot.spectrum(ans_xts$l, lab="日", main="lの周波数スペクトル", col="blue")
par(oldpar)
dev.off()


